{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module InferUF (
  Constraint,
  TypeError(..),
  Subst(..),
  test
) where
  -- inferTop,
  -- constraintsExp

import Env ( Env(TypeEnv), emptyEnv, extend, remove )
import Type ( Scheme(..), Type(..), TVar(..), typeInt, typeBool )
import Syntax ( Binop(..), Lit(LBool, LInt), Exp(..), Name )

import Control.Monad.Except
    ( runExcept,
      runExceptT,
      MonadError(throwError),
      Except,
      ExceptT, MonadTrans (lift) )
import Control.Monad.State
    ( evalStateT, MonadState(put, get), StateT (runStateT), modify, execStateT, gets )
import Control.Monad.Reader
    ( replicateM, MonadReader(local, ask), ReaderT(runReaderT) )
import Control.Monad.Identity ( Identity(runIdentity) )

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Unification.ST
import Control.Monad.ST

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

type Infer s a = ReaderT
                  Env
                  (StateT
                   (InferState s)
                   (ExceptT TypeError
                    (ST s)))
                  a

type UF s = Map.Map TVar (VarNode s)

-- | Inference state
data InferState s = InferState
  { count :: Int
  , unionFind :: UF s
  }

-- | Initial inference state
initInfer :: InferState s
initInfer = InferState { count = 0, unionFind = Map.empty }

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type UnifierUF s = (UF s, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

type SolveST s a = ExceptT TypeError (ST s) a

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply (Subst s) (Forall as t)   = Forall as $ apply s' t
                            where s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
   apply s (t1, t2) = (apply s t1, apply s t2)
   ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | Ambigious [Constraint] -- ? Not sure if this is needed
  | UnificationMismatch [Type] [Type]
  deriving Show

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> Infer s a -> ST s (Either TypeError (a, InferState s))
runInfer env m =
  let r1 = runReaderT m env
      r2 = runStateT r1 initInfer
      r3 = runExceptT r2
  in r3

convertUFToSubst :: UF s -> ST s (Map.Map TVar Type)
convertUFToSubst uf =
  Map.fromList <$> mapM helper (Map.toList uf)
  where
    helper :: (TVar, VarNode s) -> ST s (TVar, Type)
    helper (k, node) = do
      root <- find node
      maybe_ty <- getType root
      return $ case maybe_ty of
        Nothing -> (k, TVar k)
        Just ty -> (k, ty)

test :: Env -> Exp -> Either TypeError Subst
test env ex = runST $ do
  inferResult <- runInfer env (infer ex)
  case inferResult of
    Left err -> return (Left err)
    Right (_, infState) -> do
      pureUF <- convertUFToSubst $ unionFind infState
      return $ Right $ Subst pureUF

inferExpr :: Env -> Exp -> Either TypeError Scheme
inferExpr env ex = runST $ do
  inferRes <- runInfer env (infer ex)
  case inferRes of
    Left err -> pure (Left err)
    Right ((ty, cs), infState) -> do
      res <- runSolveUF (unionFind infState) cs
      case res of 
        Left err -> pure $ Left err
        Right uf -> do
          converted <- convertUFToSubst uf
          let s = Subst converted
          return $ Right $ closeOver $ apply s ty

-- ! Original code
-- | Solve for the toplevel type of an expression in a given environment
-- inferExpr :: Env -> Exp -> Either TypeError Scheme
-- inferExpr env ex = do
--   inferRes <- runInfer env (infer ex)
--   case inferRes of
--     Left err -> Left err
--     Right (ty, cs) -> case runSolve cs of
--       Left err -> Left err
--       Right subst -> Right $ closeOver $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
-- constraintsExp :: Env -> Exp -> Either TypeError ([Constraint], Subst, Type, Scheme)
-- constraintsExp env ex = case runInfer env (infer ex) of
--   Left err -> Left err
--   Right (ty, cs) -> case runSolve cs of
--     Left err -> Left err
--     Right subst -> Right (cs, subst, ty, sc)
--       where
--         sc = closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize Env.emptyEnv

-- | Extend type environment
inEnv :: (Name, Scheme) -> Infer s a -> Infer s a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupEnv :: Name -> Infer s Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
      Nothing   ->  throwError $ UnboundVariable x
      Just s    ->  do instantiate s

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer s Type
fresh = do
  s <- get
  let n = count s
      tv = TV $ letters !! n
  node <- liftST $ makeSet tv
  let uf' = Map.insert tv node (unionFind s)
  put s { count = n + 1, unionFind = uf' }
  return $ TVar tv
  
  where
    liftST :: ST s a -> Infer s a
    liftST = lift . lift . lift


instantiate ::  Scheme -> Infer s Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

generalize :: Env -> Type -> Scheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Binop -> Type
ops Add = typeInt `TArr` (typeInt `TArr` typeInt)
ops Mul = typeInt `TArr` (typeInt `TArr` typeInt)
ops Sub = typeInt `TArr` (typeInt `TArr` typeInt)
ops Eql = typeInt `TArr` (typeInt `TArr` typeBool)

getUF :: Infer s (UF s)
getUF = gets unionFind

infer :: Exp -> Infer s (Type, [Constraint])
infer expr = case expr of
  Lit (LInt _)  -> return (typeInt, [])
  Lit (LBool _) -> return (typeBool, [])

  Var x -> do
      t <- lookupEnv x
      return (t, [])

  Lam x e -> do
    tv <- fresh
    (t, c) <- inEnv (x, Forall [] tv) (infer e)
    let exprType = tv `TArr` t
    return (exprType, c)

  App e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    let cs = [(t1, t2 `TArr` tv)]
    return (tv, c1 ++ c2 ++ cs)

  Let x e1 e2 -> do
    (t1, c1) <- infer e1

    uf <- getUF
    res <- liftST $ runSolveUF uf c1
    case res of
      Left err -> throwError err
      Right uf' -> do
        -- ?TODO: Do this without converting 'uf'.
        env <- ask
        s <- liftST $ convertUFToSubst uf'
        let sub = Subst s
            sc = generalize (apply sub env) (apply sub t1)
        (t2, c2) <- inEnv (x, sc) $ local (apply sub) (infer e2)
        return (t2, c1 ++ c2)    
    where 
      liftST :: ST s a -> Infer s a
      liftST = lift . lift . lift

  Fix e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    let cs = [(tv `TArr` tv, t1)]
    return (tv, c1 ++ cs)

  Op op e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    let u1 = t1 `TArr` (t2 `TArr` tv)
        u2 = ops op
    let cs = [(u1, u2)]
    return (tv, c1 ++ c2 ++ cs)

  If cond tr fl -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tr
    (t3, c3) <- infer fl
    let cs = [(t1, typeBool), (t2, t3)]
    return (t2, c1 ++ c2 ++ c3 ++ cs)

-- inferTop :: Env -> [(String, Exp)] -> Either TypeError Env
-- inferTop env [] = Right env
-- inferTop env ((name, ex):xs) = case inferExpr env ex of
--   Left err -> Left err
--   Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _)    = []

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

lookupUF :: TVar -> UF s -> SolveST s (VarNode s)
lookupUF tv@(TV v) uf =
  case Map.lookup tv uf of
    Nothing   -> throwError $ UnboundVariable v
    Just node -> return node

-- | Run the constraint solver
runSolveUF :: UF s -> [Constraint] -> ST s (Either TypeError (UF s))
runSolveUF uf cs = runExceptT $ solverUF st
  where st = (uf, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unify :: Type -> Type -> Solve Subst
unify t1 t2 | t1 == t2 = return emptySubst
unify (TVar v) t = v `bind` t
unify t (TVar v) = v `bind` t
unify (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unify t1 t2 = throwError $ UnificationFail t1 t2

solverUF :: UnifierUF s -> SolveST s (UF s)
solverUF (uf, cs) =
  case cs of
    [] -> return uf
    ((t1, t2) : cs') -> do
      uf' <- unifyUF t1 t2 uf
      solverUF (uf', cs')

unifyUF :: Type -> Type -> UF s -> SolveST s (UF s)
unifyUF t1 t2 uf | t1 == t2 = return uf
unifyUF (TVar v1) (TVar v2) uf = unifyVars v1 v2 uf
unifyUF (TVar v) t uf = bindUF v t uf
unifyUF t (TVar v) uf = bindUF v t uf
unifyUF (TArr arg1 ret1) (TArr arg2 ret2) uf = do
  uf' <- unifyUF arg1 arg2 uf
  unifyUF ret1 ret2 uf'
unifyUF t1 t2 _ = throwError $ UnificationFail t1 t2

bindUF :: TVar -> Type -> UF s -> SolveST s (UF s)
bindUF a t uf | t == TVar a     = return uf
              | occursCheck a t = throwError $ InfiniteType a t
              | otherwise       = 
                do  node <- lookupUF a uf
                    lift $ assignType node t
                    return uf

unifyVars :: TVar -> TVar -> UF s -> SolveST s (UF s)
unifyVars v1 v2 uf = do
  n1 <- lookupUF v1 uf
  n2 <- lookupUF v2 uf
  mt1 <- lift $ getType n1
  mt2 <- lift $ getType n2
  case (mt1, mt2) of
    (Just t1, Just t2) | t1 /= t2  -> throwError $ UnificationFail t1 t2
                       | otherwise -> return uf
    _ -> do
      lift $ union n1 n2
      return uf

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t