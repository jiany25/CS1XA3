{- |
Module : ExprType
Description : 
  This section contains the following content:
    1. a Expr a datatype decleration which is designed for common numric expression
      (1) Addition expression
      (2) Substraction exprssion
      (3) Multiplication exprssion
      (4) division exprssion
      (5) Sine function exprssion
      (6) Cos function exprssion
      (7) Logarithmic(General) function exprssion
      (8) Logarithmic(Natural) function exprssion
      (9) Exponential(General) function exprssion
      (10)Exponential(Natural) function exprssion
    2. a function getVars which can implement these numeric expression in Expr a type and generate a list of variables
Copyright : (c) Yifan Jiang @2018
License : WTFPL
Maintainer : jiany25@mcmaster.ca
Stability : experimental
Portability : POSIX
-}
module ExprType where


import Data.List -- ^ So we can use the `union` method to define the getVars function

-- * DataType Decleration
-- | The Expr a is a wrapper that wraps different operations in a expression tree.
data Expr a = Const a -- ^ Value wrapper: Const Num a => a, such as Const Integer Const Float, etc.
            | Var String -- ^ Variable Identifier: ex Var "x" Var "y", etc.
            | Add (Expr a) (Expr a) -- ^ Standard binary addition
            | Sub (Expr a) (Expr a) -- ^ Standard binary substraction
            | Mult (Expr a) (Expr a)-- ^ Standard binary multiplication
            | Div (Expr a) (Expr a)-- ^ Standard binary division
            | Sin (Expr a) (Expr a)-- ^ Sine function: f(x)=sinx
            | Cos (Expr a) (Expr a)-- ^ Cosine function: f(x)=cosx 
            | Log (Expr a) (Expr a)-- ^ General logarithmic function: f(x)=loga(x)
            | Ln (Expr a) (Expr a)-- ^ Natural logarithmic function: f(x)=ln(x)(where a = e)
            | Exp (Expr a) (Expr a)-- ^ General exponential function: f(x)=a^x
            | NatExp (Expr a) (Expr a)-- ^ Natural expoential function: f(x)=e^x(where a = e)
      deriving Eq

-- * Miscellaneous Functions
-- | A function that obtains the varibale identifiers and forms a list of strings.
getVars :: Expr a -- ^ Input datatype: Expr a , which is defined above
           -> [String]-- ^ Output type: List of String
getVars e = case e of-- ^ Using pattern matching to let the definition cover very type from the expression tree.
       (Var ident) -> [ident]-- ^ Return the list of variable idetifiers.
       (Const a) -> []-- ^ Return a empty list since the Const tag wraps a Num a value and no variable can be obtained.
       (Add e1 e2) -> getVars e1 `union` getVars e2-- ^ Return the union list of two lists that are obtained from the two args of Add (Expr a) (Expr a) expression.
       (Sub e1 e2) -> getVars e1 `union` getVars e2-- ^ According to the property of `union` method, if two same variable idents are obtained, the result list only show one.
       (Mult e1 e2) -> getVars e1 `union` getVars e2-- ^ For example: getVars e1 == ["x"], getVars e2 == ["x"], then getVars e1 `union` getVars e2 == ["x"]
       (Div e1 e2) -> getVars e1 `union` getVars e2-- ^ However, getVars e1 == ["x"], getVars e2 == ["y"], then getVars e1 `union` getVars e2 == ["x","y"]
       (Sin e1 e2) -> getVars e2-- ^ The first parameter should be (Const a) type, normally be exact (Const 1.0), so only operate the second parameter.
       (Cos e1 e2) -> getVars e2-- ^ Similarly, (Cos e1 e2) should be (Cos (Const 1.0) (Expr a))
       (Log e1 e2) -> getVars e2-- ^ Log (Const a) (Expr a), only the second parameter gives variables
       (Ln e1 e2) -> getVars e2-- ^ Log (Const e) (Expr a) = Ln (Const 1.0) (Expr a), only the second parameter gives variables
       (Exp e1 e2) -> getVars e2-- ^ Exp (Const a) (Expr a), only the second parameter gives variables
       (NatExp e1 e2) -> getVars e2-- ^ Exp (Const e) (Expr a) = NatExp (Const 1.0) (Expr a), only the second parameter gives variables
