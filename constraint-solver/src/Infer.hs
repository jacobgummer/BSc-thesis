{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer (
  Constraint,
  TypeError(..),
  Subst(..),
  inferTop,
  constraintsExp
) where

import Env ( Env(TypeEnv), emptyEnv, extend, remove )
import Type ( Scheme(..), Type(..), TVar(..), typeInt, typeBool )
import Syntax ( Binop(..), Lit(LBool, LInt), Exp(..), Name )

import Control.Monad.Except
    ( runExcept,
      runExceptT,
      MonadError(throwError),
      Except,
      ExceptT )
import Control.Monad.State
    ( evalStateT, MonadState(put, get), StateT )
import Control.Monad.Reader
    ( replicateM, MonadReader(local, ask), ReaderT(runReaderT) )
import Control.Monad.Identity ( Identity(runIdentity) )

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace ( traceM )

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Inference monad
type Infer a = (ReaderT
                  Env             -- Typing environment
                  (StateT         -- Inference state
                  InferState
                  (Except         -- Inference errors
                    TypeError))
                  a)              -- Result

-- | Inference state
newtype InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

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
runInfer :: Env -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> Exp -> Either TypeError Scheme
inferExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExp :: Env -> Exp -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExp env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize Env.emptyEnv

-- | Extend type environment
inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupEnv :: Name -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
      Nothing   ->  throwError $ UnboundVariable x
      Just s    ->  do instantiate s

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
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

infer :: Exp -> Infer (Type, [Constraint])
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
    traceM $ 
      "expression '" ++ printExp expr ++ "' has type:\n\t"
      ++ printType exprType
    return (exprType, c)

  App e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    let cs = [(t1, t2 `TArr` tv)]
    traceM $
      "expression '" ++ printExp expr ++ "' introduced the constraint:\n\t"
      ++ printConstraints cs ++ "\n and has type:\n\t" ++ printType tv
    return (tv, c1 ++ c2 ++ cs)

  Let x e1 e2 -> do
    env <- ask
    (t1, c1) <- infer e1
    case runSolve c1 of
        Left err -> throwError err
        Right sub -> do
            let sc = generalize (apply sub env) (apply sub t1)
            (t2, c2) <- inEnv (x, sc) $ local (apply sub) (infer e2)
            return (t2, c1 ++ c2)

  Fix e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    let cs = [(tv `TArr` tv, t1)]
    traceM $
      "expression '" ++ printExp expr ++ "' introduced the constraint:\n\t"
      ++ printConstraints cs
    return (tv, c1 ++ cs)

  Op op e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    let u1 = t1 `TArr` (t2 `TArr` tv)
        u2 = ops op
    let cs = [(u1, u2)]
    traceM $ 
      "expression '" ++ printExp expr ++ "' introduced the constraint:\n\t"
      ++ printConstraints cs ++ "\n and has type:\n\t" ++ printType tv
    return (tv, c1 ++ c2 ++ cs)

  If cond tr fl -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tr
    (t3, c3) <- infer fl
    let cs = [(t1, typeBool), (t2, t3)]
    traceM $ 
      "expression '" ++ printExp expr ++ "' introduced the constraints:\n\t"
      ++ printConstraints cs
    return (t2, c1 ++ c2 ++ c3 ++ cs)

inferTop :: Env -> [(String, Exp)] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

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

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where st = (emptySubst, cs)

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

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs') -> do
      su1  <- unify t1 t2
      solver (su1 `compose` su, apply su1 cs')

bind ::  TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

maybeParenthesisType :: Type -> String
maybeParenthesisType t' = case t' of
  TVar (TV v) -> v
  TCon c      -> c
  _           -> "(" ++ printType t' ++ ")"

printType :: Type -> String
printType t = case t of
  TVar (TV v) -> v
  TCon c      -> c
  TArr t1 t2  -> maybeParenthesisType t1 ++ " -> " ++ maybeParenthesisType t2

printConstraint :: Constraint -> String
printConstraint (t1, t2) = maybeParenthesisType t1 ++ " ~ " ++ maybeParenthesisType t2

printConstraints :: [Constraint] -> String
printConstraints []     = "No constraints."
printConstraints [c]    = printConstraint c
printConstraints (c:cs) = printConstraint c ++ ",\n\t" ++ printConstraints cs

printExp :: Exp -> String
printExp expr =
  case expr of
    Var n -> n
    App e1 e2 -> 
      maybeParenthesisExp e1 ++ " " ++ maybeParenthesisExp e2
    Lam n e -> 
      "λ" ++ n ++ " -> " ++ printExp e
    Let n e1 e2 -> 
      "let " ++ n ++ " = " ++ printExp e1 ++ " in " ++ printExp e2
    If e1 e2 e3 -> 
      "if " ++ printExp e1 ++ " then " ++ printExp e2 ++ " else " ++ printExp e3
    Fix e -> "rec " ++ printExp e
    Op binop e1 e2 ->
      maybeParenthesisExp e1 ++ printBinop binop ++ maybeParenthesisExp e2
    Lit l -> 
      case l of
        LInt i -> show i
        LBool b -> show b
    where
      maybeParenthesisExp :: Exp -> String
      maybeParenthesisExp e = case e of
        Lit l -> 
          case l of
            LInt i  -> show i
            LBool b -> show b
        Var v -> v
        _ -> "(" ++ printExp e ++ ")"

      printBinop :: Binop -> String
      printBinop binop = case binop of
        Add -> " + "
        Sub -> " - "
        Mul -> " * "
        Eql -> " == "