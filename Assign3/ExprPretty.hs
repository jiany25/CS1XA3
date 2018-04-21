{-|
Module : ExprPretty
Description : 
  This section contains the following content:
    1. Auxiliay function
    2. Method definition of Show (Expr a)
Copyright : (c) Yifan Jiang @2018
License : WTFPL
Maintainer : jiany25@mcmaster.ca
Stability : experimental
Portability : POSIX
TODO write a longer description of the module,
containing some commentary with @some markup@.
-}
module ExprPretty where

-- | This depends on the "ExprType" module
import ExprType

-- * Auxiliay function
-- | A funtion that put string in the parentheses.
parens :: String -> String
parens ss = "(" ++ ss ++ ")"

-- * Method definition of Show (Expr a)
{-| From very aspect of the Expr a expression tree to define the implement of show function 
    to covert the Expr a datatype into cooresponding string expression.
-}
instance Show a => Show (Expr a) where
  show (Add e1 e2) = parens (show e1) ++ " !+ " ++ parens (show e2)-- ^ Example: show (Add (Const 1) (Const 1)) == "(val 1)!+(val 1)"
  show (Sub e1 e2)  = parens (show e1) ++ " !- " ++ parens (show e2)-- ^ Example: show (Sub (Const 1) (Const 1)) == "(val 1)!-(val 1)"
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)-- ^ Example: show (Mult (Const 1) (Const 1)) == "(val 1)!*(val 1)"
  show (Div e1 e2)  = parens (show e1) ++ " !/ " ++ parens (show e2)-- ^ Example: show (Div (Const 1) (Const 1)) == "(val 1)!/(val 1)"
  show (Sin e1 e2) = "sin"++parens(show e1)++parens (show e2)-- ^ Example: show (Sin (Const 1) (Var "x")) == "sin(val 1)(val 1)"
  show (Cos e1 e2) = "cos"++parens(show e1)++parens (show e2)-- ^ Example: show (Cos (Const 1) (Var "x")) == "cos(val 1)(val 1)"
  show (Log e1 e2) = "Log"++parens(show e1)++parens (show e2)-- ^ Example: show (Log (Const 2) (Var "x")) == "log(val 2)(val 1)"
  show (Ln e1 e2) = "Ln"++parens (show e2)-- ^ Example: show (Ln (Const e) (Var "x")) == "Ln(var "x")"
  show (Exp e1 e2) = parens (show e1)++"^"++parens(show e2)-- ^ Example: show (Exp (Const 1) (Var "x")) == "(val 1)^(Var "x")"
  show (NatExp e1 e2) = "e^"++parens(show e2)-- ^ Example: show (NatExp (Const e) (Const 1)) == "e^(Var "x")"
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\"" -- ^ Example: show (Var "x") == "(var "x")"
  show (Const x)    = parens $ "val " ++ show x -- ^ Example: show (Const 1) == "(val 1)"