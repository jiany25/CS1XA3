-- | Language extension. Should be put on the top of the .hs file.
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Module : ExprDiff
Description : 
  This section contains the following content:
  1. a class decleration using Embedded Domain Specific Language
    (1) Main Class Methods
    (2) Methods with Default Implementation
  2. Method Definition (Definition about the implement of the differentiation of (Expr a) dataType)
Copyright : (c) Yifan Jiang @2018
License : WTFPL
Maintainer : jiany25@mcmaster.ca
Stability : experimental
Portability : POSIX
-}
module ExprDiff where


import qualified Data.Map.Strict as Map 
import ExprType -- ^ This depends on the "ExprType" module

-- * Class Decleration
-- | Generalizing expressions into a Embedded Domain Specific Language using a Type Class
class DiffExpr a where
  -- ** Main Class Methods
  -- | The evaluation function that can evaluate the value of expressions of (Expr a) type
	eval :: Map.Map String a -> Expr a -> a
	-- |
	simplify :: Map.Map String a -> Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a
    -- ** Methods with Default Implementation
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  (!-) :: Expr a -> Expr a -> Expr a
  e1 !- e2 = simplify (Map.fromList []) $ Sub e1 e2
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  (!/) :: Expr a -> Expr a -> Expr a
  e1 !/ e2 = simplify (Map.fromList []) $ Div e1 e2

  val :: a -> Expr a
  val x = Const x
  var :: String -> Expr a
  var x = Var x

-- * Method Definition
instance (Num a) => DiffExpr a where
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Sub e1 e2)  = eval vrs e1 - eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Div e1 e2) = eval vrs e1 / eval vrs e2
  eval vrs (Sin e1 e2) = sin $ eval vrs e1 * eval vrs e2
  eval vrs (Cos e1 e2) = cos $ eval vrs e1 * eval vrs e2
  eval vrs (Log e1 e2) = logBase (eval vrs e1) (eval vrs e2)
  eval vrs (Ln e1 e2) = log (eval vrs e2)
  eval vrs (Exp e1 e2) = (eval vrs e1)^(eval vrs e2)
  eval vrs (NatExp e1 e2) = exp (eval vrs e2)
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"

  simplify _ e = e

  partDiff _ e = case e of
                Var _ -> Const 1 -- ^ x' = 1
                Const _ -> Const 0-- ^ a' = 0 (where a is a number)
                Add e1 e2 = Add (partDiff _ e1) (partDiff _ e2)-- ^ [f(x)+g(x)]' = f'(x)+g'(x)
                Sub e1 e2 = Sub (partDiff _ e1) (partDiff _ e2)-- ^ [f(x)-g(x)]' = f'(x)-g'(x)
                Mult e1 e2 = Add ((Mult (partDiff _ e1) e2)) (Mult e1 (partDiff _ e2))-- ^ [f(x)*g(x)]' = f'(x)*g(x) + g'(x)*f(x)
                Div e1 e2 = Div (Sub ((Mult (partDiff _ e1) e2)) (Mult e1 (partDiff _ e2))) (Mult e2 e2)-- ^ [f(x)/g(x)]'= [f'(x)*g(x) - g'(x)*f(x)]/[g(x)^2]
                Sin e1 e2 = Mult (Mult (Const (-1)) (Cos e1 e2)) (partDiff _ e2)
                Cos e1 e1 = Mult (Sin e1 e2) (partDiff _ e2)
                Log (Const a) e2 = Mult (Div (Const 1) (Mult (Const (ln a)) (e2))) (partDiff _ e2)
                Ln (Const a) e2 = Mult (Div (Const 1) e2) (partDiff _ e2)
                Exp (Const a) e2 = Mult (Mult (Const (ln a)) (Exp (Const a) e2)) (partDiff _ e2)
                NatExp (Const a) e2 = Mult (NatExp (Const a) e2) (partDiff _ e2)


 
