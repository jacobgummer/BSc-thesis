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
printConstraints [] = ""
printConstraints csts = 
  removeLastComma $ foldr consConstraint "" csts
  where
    consConstraint :: Constraint -> String -> String
    consConstraint (t1, t2) acc = 
      maybeParenthesis t1
      ++ " ~ "
      ++ maybeParenthesis t2
      ++ ",\n\t"
      ++ acc

    removeLastComma :: String -> String
    removeLastComma str = take (length str - 4) str

printInferResult :: Either TypeError ([Constraint], Subst, Type, Scheme)
                    -> String
printInferResult infRes = case infRes of
  Left err -> show err
  Right (csts, s, t, sch) ->
    "Constraints: \n\t" ++ printConstraints csts ++ "\n"

    -- TODO: make function for printing substitution.
    ++ "Substitution: \n\t" ++ show s ++ "\n"

    ++ "Type: \n\t" ++ printType t ++ "\n"

    -- TODO: make function for printing scheme.
    ++ "Scheme: \n\t" ++ show sch