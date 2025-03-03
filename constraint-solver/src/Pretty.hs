module Pretty where

import Env
import Infer
import Syntax
import Type

maybeParenthesis :: Type -> String
maybeParenthesis t' = case t' of
  TVar (TV v) -> v
  TCon c      -> c
  _           -> "(" ++ printType t' ++ ")"

printType :: Type -> String
printType t = case t of
  TVar (TV v) -> v
  TCon c      -> c
  TArr t1 t2  -> maybeParenthesis t1 ++ " -> " ++ maybeParenthesis t2

printConstraint :: Constraint -> String
printConstraint (t1, t2) = maybeParenthesis t1 ++ " ~ " ++ maybeParenthesis t2

printConstraints :: [Constraint] -> String
printConstraints = removeLastComma . foldr consConstraint ""
  where
    consConstraint :: Constraint -> String -> String
    consConstraint (t1, t2) acc =
      maybeParenthesis t1
      ++ " ~ "
      ++ maybeParenthesis t2
      ++ ",\n\t"
      ++ acc

    removeLastComma :: String -> String
    removeLastComma str = take (length str - 3) str

-- TODO: Implement this.
printSubstitution :: Subst -> String
printSubstitution = show

-- TODO: Implement this.
printScheme :: Scheme -> String
printScheme = show

-- TODO: Implement this.
printTypeError :: TypeError -> String
printTypeError = show

-- TODO: Implement this.
printExp :: Exp -> String
printExp = show

printInferResult :: Exp -> String
printInferResult e = 
  "Expression: \n\t" ++ printExp e ++ "\n" ++
  case constraintsExp emptyEnv e of
    Left err -> printTypeError err

    Right (csts, subst, t, sch) ->
      "Constraints: \n\t" ++ printConstraints csts ++ "\n"

      ++ "Substitution: \n\t" ++ printSubstitution subst ++ "\n"

      ++ "Type: \n\t" ++ printType t ++ "\n"

      ++ "Scheme: \n\t" ++ printScheme sch