{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module InferUF (
  Constraint,
  TypeError(..),
  Subst(..),
  constraintsExp,
  inferExpr,
) where

import Env ( Env(TypeEnv), emptyEnv, extend, remove )
import Type ( Scheme(..), Type(..), TVar(..), typeInt, typeBool )
import Syntax ( Binop(..), Lit(LBool, LInt), Exp(..), Name )

import Control.Monad.Except
    ( runExceptT,
      MonadError(throwError),
      ExceptT, MonadTrans (lift) )
import Control.Monad.State
    ( MonadState(put, get), StateT (runStateT), gets )
import Control.Monad.Reader
    ( replicateM, MonadReader(local, ask), ReaderT(runReaderT) )

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Unification.ST
import Control.Monad.ST

import Data.Maybe (catMaybes)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Inference monad
type Infer s a = ReaderT Env              -- Typing environment
                  (StateT (InferState s)  -- Inference state
                   (ExceptT TypeError     -- Inference errors
                    (ST s)))              -- Inner state
                  a                       -- Result

-- | UnionFind data structure
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

type UnifierUF s = (UF s, [Constraint])

-- | Constraint solver monad
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
convertUFToSubst uf = do
  mappings <- mapM helper (Map.toList uf)

  -- Remove type variables that haven't been assigned any type.
  return $ Map.fromList (catMaybes mappings)

  where
    helper :: (TVar, VarNode s) -> ST s (Maybe (TVar, Type))
    helper (k, node) = do
      root <- find node
      maybe_ty <- getType root
      return $ case maybe_ty of
        Nothing -> Nothing
        Just ty -> Just (k, ty)

inferExpr :: Env -> Exp -> Either TypeError Scheme
inferExpr env ex = runST $ do
  inferRes <- runInfer env (infer ex)
  case inferRes of
    Left err -> return $ Left err
    Right ((ty, cs), infState) -> do
      res <- runSolveUF (unionFind infState) cs
      case res of
        Left err -> return $ Left err
        Right uf -> do
          s <- convertUFToSubst uf
          let subst = Subst s
          return $ Right $ closeOver $ apply subst ty

constraintsExp :: Env
                  -> Exp
                  -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExp env ex = runST $ do
  inferRes <- runInfer env (infer ex)
  case inferRes of
    Left err -> return $ Left err
    Right ((ty, cs), infState) -> do
      res <- runSolveUF (unionFind infState) cs
      case res of
        Left err -> return $ Left err
        Right uf -> do
          s <- convertUFToSubst uf
          let subst = Subst s
              sc = closeOver $ apply subst ty
          return $ Right (cs, subst, ty, sc)

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
        -- TODO: If possible, do this without converting 'uf'.
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

-- | Run the constraint solver.
runSolveUF :: UF s -> [Constraint] -> ST s (Either TypeError (UF s))
runSolveUF uf cs = runExceptT $ solverUF (uf, cs)

-- | Get the type assigned to representative of equivalence class.
probeValue :: TVar -> UF s -> SolveST s (Maybe Type)
probeValue tv uf = do
  node <- lookupUF tv uf
  lift $ getType node

-- | Normalize a type, i.e., find out if a type variable
--   has already been resolved to a type.
normalizeType :: Type -> UF s -> SolveST s Type
normalizeType ty uf = case ty of
  ty'@(TCon _)     -> return ty'
  (TArr argT retT) -> do
    argT' <- normalizeType argT uf
    retT' <- normalizeType retT uf
    return $ TArr argT' retT'
  (TVar v) -> do
    mt <- probeValue v uf -- Note: probeValue performs path compression.
    case mt of
      -- Also normalize the found type.
      Just ty' -> normalizeType ty' uf

      -- Return the type variable representing
      -- the root of equivalence class.
      Nothing -> do
        node <- lookupUF v uf
        key' <- lift $ getKey node
        return $ TVar key'

-- | Unification solver.
solverUF :: UnifierUF s -> SolveST s (UF s)
solverUF (uf, cs) =
  case cs of
    [] -> return uf
    ((t1, t2) : cs') -> do
      t1' <- normalizeType t1 uf
      t2' <- normalizeType t2 uf
      uf' <- unifyUF t1' t2' uf
      solverUF (uf', cs')

-- | Unify two types.
unifyUF :: Type -> Type -> UF s -> SolveST s (UF s)
unifyUF t1 t2 uf | t1 == t2 = return uf
unifyUF (TVar v1) (TVar v2) uf = unifyVars v1 v2 uf
unifyUF (TVar v) t uf = bindUF v t uf
unifyUF t (TVar v) uf = bindUF v t uf
unifyUF (TArr arg1 ret1) (TArr arg2 ret2) uf = do
  uf' <- unifyUF arg1 arg2 uf
  unifyUF ret1 ret2 uf'
unifyUF t1 t2 _ = throwError $ UnificationFail t1 t2

-- | Bind a type variable to a type.
bindUF :: TVar -> Type -> UF s -> SolveST s (UF s)
bindUF a t uf | t == TVar a     = return uf
              | occursCheck a t = throwError $ InfiniteType a t
              | otherwise       =
                do  node <- lookupUF a uf
                    lift $ assignType node t
                    return uf

-- | Unify two type variables.
unifyVars :: TVar -> TVar -> UF s -> SolveST s (UF s)
unifyVars v1 v2 uf = do
  n1 <- lookupUF v1 uf
  n2 <- lookupUF v2 uf
  mt1 <- lift $ getType n1
  mt2 <- lift $ getType n2
  case (mt1, mt2) of
    (Just t1, Just t2) | t1 /= t2  -> throwError $ UnificationFail t1 t2
                       | otherwise -> return uf
    (Just _, Nothing) -> do
      lift $ union n2 n1
      return uf
    _ -> do
      lift $ union n1 n2
      return uf

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t