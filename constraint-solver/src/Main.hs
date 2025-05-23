module Main where

import Syntax
import Pretty

-- 1 + (2 + 5)
simpleExp :: Exp
simpleExp = Op Add (Lit (LInt 1)) (Op Add (Lit (LInt 2)) (Lit (LInt 5)))

appExp :: Exp
appExp = App
          (App (Lam "x" (Lam "y" (Op Mul (Var "x") (Var "y")))) (Lit $ LInt 5))
          (Lit $ LInt 2)

-- \x -> x
identityFunction :: Exp
identityFunction = Lam "x" (Var "x")

-- \x y -> x
twoForAllFunc :: Exp
twoForAllFunc = Lam "x" (Lam "y" (Var "x"))

-- \f n -> if n == 0 then 1 else f (n - 1)
factorialFunc :: Exp
factorialFunc = Fix (Lam "f" (Lam "n"
                (If (Op Eql (Var "n") (Lit (LInt 0)))
                    (Lit (LInt 1))
                    (Op Mul (Var "n") (App (Var "f") (Op Sub (Var "n") (Lit (LInt 1)))))
                )))

-- if 1 then 2 else 3
unificationFail :: Exp
unificationFail =
  If (Lit (LInt 1))
     (Lit (LInt 2))
     (Lit (LInt 3))

-- \x -> x x
infiniteType :: Exp
infiniteType =
  Lam "x" (App (Var "x") (Var "x"))

-- (\x -> x) y
unboundVariable :: Exp
unboundVariable =
  App (Lam "x" (Var "x")) (Var "y")

letExp1 :: Exp
letExp1 =
  Let "id" (Lam "x" (Var "x")) (App (Var "id") (Var "id"))

letExp2 :: Exp
letExp2 =
  Let "id" (Lam "x" (Var "x"))
    (Op Add (App (Var "id") (Lit $ LInt 1)) (App (Var "id") (Lit $ LInt 6)))

main :: IO ()
main = do
  -- let result = runST $ do
  --       p <- makeSet $ TV "p"
  --       q <- makeSet $ TV "q"
  --       p `union` q
  --       assignType p typeBool
  --       getType q
  -- putStrLn ("q has type: " ++ maybe "No value assigned" printType result)
  putStrLn $ printInferResult simpleExp
  putStrLn "-------------------------------------------------------------------------------------"
  putStrLn $ printInferResult appExp
  putStrLn "-------------------------------------------------------------------------------------"
  putStrLn $ printInferResult identityFunction
  putStrLn "-------------------------------------------------------------------------------------"
  putStrLn $ printInferResult twoForAllFunc
  putStrLn "-------------------------------------------------------------------------------------"
  putStrLn $ printInferResult factorialFunc
  putStrLn "-------------------------------------------------------------------------------------"
  putStrLn $ printInferResult unificationFail
  putStrLn "-------------------------------------------------------------------------------------"
  putStrLn $ printInferResult infiniteType
  putStrLn "-------------------------------------------------------------------------------------"
  putStrLn $ printInferResult letExp1
  putStrLn "-------------------------------------------------------------------------------------"
  putStrLn $ printInferResult letExp2