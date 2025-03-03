module Pretty where

import Env
import Infer
import Syntax
import Type

import qualified Data.Map as Map

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

-- printConstraint :: Constraint -> String
-- printConstraint (t1, t2) = maybeParenthesisType t1 ++ " ~ " ++ maybeParenthesisType t2

printConstraints :: [Constraint] -> String
printConstraints = removeLastComma . foldr consConstraint ""
  where
    consConstraint :: Constraint -> String -> String
    consConstraint (t1, t2) acc =
      maybeParenthesisType t1
      ++ " ~ "
      ++ maybeParenthesisType t2
      ++ ",\n\t"
      ++ acc

    removeLastComma :: String -> String
    removeLastComma str = take (length str - 3) str

printSubstitutions :: Subst -> String
printSubstitutions substs =
  case substs of
    Subst subMap -> 
      let pairs = Map.toList subMap
      in printSubstitution pairs 
      where
        printSubstitution :: [(TVar, Type)] -> String
        printSubstitution pairs =
          case pairs of
            [] -> ""
            [(TV t1, t2)] -> t1 ++ " ↦ " ++ printType t2
            ((TV t1, t2) : ts) -> t1 ++ " ↦ " ++ printType t2 ++ ",\n\t" ++ printSubstitution ts

printScheme :: Scheme -> String
printScheme sch =
  case sch of
    Forall [] _ -> "No schemes found."
    Forall [TV v] t -> "∀" ++ v ++ ". " ++ printType t
    Forall tvs t -> "∀" ++ printTVars tvs ++ ". " ++ printType t
    where
      printTVars :: [TVar] -> String
      printTVars h =
        case h of
          [] -> ""
          [TV v] -> v
          ((TV v) : tvs) -> v ++ "," ++ printTVars tvs

printTypeError :: TypeError -> String
printTypeError typeError =
  case typeError of
    UnificationFail t1 t2 -> "Cannot unify types: " ++ printType t1 ++ " and " ++ printType t2
    InfiniteType (TV v) t1 -> "Cannot construct the infinite type: " ++ v ++ " ~ " ++ printType t1
    UnboundVariable str -> "Unbound variable: " ++ str
    Ambigious con -> "Ambiguous constraint: " ++ printConstraints con
    UnificationMismatch t1 t2 -> 
      "Unification mismatch between: " ++ printTypes t1 ++ " and " ++ printTypes t2
        where
          printTypes :: [Type] -> String
          printTypes ts =
            case ts of
              [] -> ""
              [t] -> printType t
              (t:ts') -> printType t ++ ", " ++ printTypes ts'

printExp :: Exp -> String
printExp expr =
  case expr of
    Var n -> n
    App e1 e2 -> 
      maybeParenthesisExp e1 ++ " " ++ maybeParenthesisExp e2
    Lam n e -> 
      "λ" ++ n ++ " -> " ++ printExp e
    Let n e1 e2 -> 
      "let" ++ n ++ printExp e1 ++ printExp e2
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
        (Lit l) -> 
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

printInferResult :: Exp -> String
printInferResult e = 
  "Expression: \n\t" ++ printExp e ++ "\n" ++
  case constraintsExp emptyEnv e of
    Left err -> printTypeError err

    Right (csts, substs, t, sch) ->
      "Constraints: \n\t" ++ printConstraints csts ++ "\n"

      ++ "Substitution: \n\t" ++ printSubstitutions substs ++ "\n"

      ++ "Type: \n\t" ++ printType t ++ "\n"

      ++ "Scheme: \n\t" ++ printScheme sch