{-|
Module : ExprType
Description : Contains a type class and instances for
differentiable expressions
Copyright : (c) Yifan Jiang @2018
License : WTFPL
Maintainer : jiany25@mcmaster.ca
Stability : experimental
Portability : POSIX
TODO write a longer description of the module,
containing some commentary with @some markup@.
-}
module ExprType where

import Data.List

-- * DataType Decleration
-- | A datatype for common numeric expression
data Expr a = Const a -- ^ Value wrapper: Const Num a => a such as Const Integer Const Float, etc.
            | Var String -- ^ Variable Identifier: ex Var "x" Var "y", etc.
            | Add (Expr a) (Expr a) -- ^ Standard binary addition
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
getVars :: Expr a -> [String]
getVars e = case e of
       (Var ident) -> [ident]--
       (Const a) -> []--
       (Add e1 e2) -> getVars e1 `union` getVars e2--
       (Mult e1 e2) -> getVars e1 `union` getVars e2--
       (Div e1 e2) -> getVars e1 `union` getVars e2--
       (Sin e1 e2) -> getVars e2--
       (Cos e1 e2) -> getVars e2--
       (Log e1 e2) -> getVars e2--
       (Ln e1 e2) -> getVars e2--
       (Exp e1 e2) -> getVars e2--
       (NatExp e1 e2) -> getVars e2--
