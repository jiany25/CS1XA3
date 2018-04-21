{-|
Module : ExprTest
Description : 
  This section contains the following content:
    Some test case (Defined for using of QuickCheck tool)
Copyright : (c) Yifan Jiang @2018
License : WTFPL
Maintainer : jiany25@mcmaster.ca
Stability : experimental
Portability : POSIX
-}
module ExprTest where


import ExprDiff-- ^ This depends on the "ExprDiff" module

import ExprParser-- ^ This depends on the "ExprParser" module

import ExprPretty-- ^ This depends on the "ExprPretty" module

import ExprType-- ^ This depends on the "ExprType" module

import qualified Data.Map.Strict as Map
-- | A Haskell tool that tests your code and return boolean expression
import Test.QuickCheck


--* Some test case

sampleExpr1 :: Expr Int
sampleExpr1 = (var "x") !+ (var "y")


listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"


test1 :: Int -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0
