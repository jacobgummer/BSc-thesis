module Main where

import Env
import Infer
import Syntax
import Type
import Pretty

exp1 :: Exp
exp1 = Op Add (Lit (LInt 1)) (Op Add (Lit (LInt 2)) (Lit (LInt 5)))
-- 1 + (2 + 5)
exp1Inferred :: Either TypeError ([Constraint], Subst, Type, Scheme)
exp1Inferred = constraintsExp emptyEnv exp1

identityFunction :: Exp
identityFunction = Lam "x" (Var "x")

twoForAllFunc :: Exp
twoForAllFunc = Lam "x" (Lam "y" (Var "x"))

factorialFunc :: Exp
factorialFunc = Fix (Lam "f" (Lam "n" 
                (If (Op Eql (Var "n") (Lit (LInt 0)))
                    (Lit (LInt 1))
                    (Op Mul (Var "n") (App (Var "f") (Op Sub (Var "n") (Lit (LInt 1)))))
                )))

-- 1. UnificationFail - Occurs when two types can't be unified
-- For example, trying to use an integer where a boolean is expected
testUnificationFail :: Exp
testUnificationFail =
  If (Lit (LInt 1)) -- Condition should be a Bool, not an Int
     (Lit (LInt 2)) 
     (Lit (LInt 3))

-- 2. InfiniteType - Occurs with recursive types that would lead to infinite types
-- Classic example: trying to apply a function to itself recursively
testInfiniteType :: Exp
testInfiniteType =
  -- λx. x x - a function that applies its argument to itself
  Lam "x" (App (Var "x") (Var "x"))

-- 3. UnboundVariable - Occurs when a variable is not defined in the environment
testUnboundVariable :: Exp
testUnboundVariable =
  -- Trying to use a variable 'y' that hasn't been defined
  App (Lam "x" (Var "x")) (Var "y")

-- 4. Ambiguous - Occurs when there are ambiguous constraints
-- This typically happens when there's not enough information to determine a type
testAmbiguous :: Exp
testAmbiguous =
  -- Using polymorphic id function with no context to determine its type
  Lam "id" (Lam "x" (App (Var "id") (Var "x")))

-- 5. UnificationMismatch - Occurs when trying to unify lists of types of different lengths
-- This often happens with wrong number of arguments to a function
testUnificationMismatch :: Exp
testUnificationMismatch =
  -- Create a function application with mismatched types
  App (Lam "x" (Lam "y" (Var "x"))) -- Function expects two arguments
      (Lit (LInt 1))                -- But we're trying to apply it to three
      -- Synthetic error - your current code might not directly generate this error

main :: IO ()
main = do 
    -- putStrLn "Hello, Haskell!"
    putStrLn $ printInferResult exp1
    putStrLn "-------------------------------------------------------------------------------------"
    putStrLn $ printInferResult identityFunction
    putStrLn "-------------------------------------------------------------------------------------"
    putStrLn $ printInferResult twoForAllFunc
    putStrLn "-------------------------------------------------------------------------------------"
    putStrLn $ printInferResult factorialFunc
    putStrLn "-------------------------------------------------------------------------------------"
    putStrLn $ printInferResult testUnificationFail
    putStrLn "-------------------------------------------------------------------------------------"
    putStrLn $ printInferResult testInfiniteType
    putStrLn "-------------------------------------------------------------------------------------"
    putStrLn $ printInferResult testUnboundVariable
    putStrLn "-------------------------------------------------------------------------------------"
    putStrLn $ printInferResult testAmbiguous
    putStrLn "-------------------------------------------------------------------------------------"
    putStrLn $ printInferResult testUnificationMismatch