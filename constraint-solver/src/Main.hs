module Main where

import Env
import Infer
import Syntax
import Type
import Pretty

exp1 :: Exp
exp1 = Op Add (Lit (LInt 1)) (Op Add (Lit (LInt 2)) (Lit (LInt 5)))
-- 1 + 2

exp1Inferred :: Either TypeError ([Constraint], Subst, Type, Scheme)
exp1Inferred = constraintsExp emptyEnv exp1

main :: IO ()
main = do 
    -- putStrLn "Hello, Haskell!"
    putStrLn $ printInferResult exp1Inferred