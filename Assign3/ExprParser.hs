{-|
Module : ExprParser
Description : 
  This section contains the following content:
    a Main parser function that can parse strings of numeric expression
    to the desired (Expr a) datatype
      (1) Auxiliary parser functions (would help in defining every single parser function for every aspect of the Expr a expression tree)
      (2) Subordinate parser functions (Each function parse one aspect of the Expr a expression tree)
      (3) Integrated parser function (Compose the seperated parser functions together so that it can parse any expression)
Copyright : (c) Yifan Jiang @2018
License : WTFPL
Maintainer : jiany25@mcmaster.ca
Stability : experimental
Portability : POSIX
-}
module ExprParser where

import ExprType-- ^ This depends on the "ExprType" module


import Text.Parsec-- ^ To use the Parsec Combinators to define the parsing functions for Expr a datatype
import Text.Parsec.String  -- ^ To use the Parsec.String Combinators to define the auxiliary parser functions.

-- | A function that takes a string of numeric expression and parses to the desired expression of (Expr double) datatype

-- * Main parser functions
parseExpr :: String -- ^ Input datatype to parse: String
             -> Expr Double -- ^ Parsing result datatype: Expr Double
{-| parse :: Stream s Data.Functor.Identity.Identity t =>
                  Parsec s () a -> SourceName -> s -> Either ParseError a
                  type Paser = Parsec String ()
                  so   Paser a = Parsec String () a
                  so   Paser (Expr Double) = Parsec String () (Expr Double)
                  Parsec String () (Expr Double) -> SourceName -> String -> Either ParseError (Expr Double)
                            Parser (Expr Double) -> String -> String -> Either ParseError (Expr Double)
                  exprD :: Parser (Expr Double)
                  so parse exprD "" ss = Either ParseError (Expr Double)
-}         
parseExpr ss = case parse exprA "" ss of
                  Left err -> error $ "Parse Error: "++show err -- ^ Left ParseError is the error message
                  Right expr -> expr -- ^ Right (Expr double) returns the expression of Expr Double

-- ** Auxiliary parser functions
-- | A function that parse certain symbols in a string expression
symbol :: String -> Parser String
symbol ss = let 
    symbol' :: Parser String
    symbol' = do {space;
                  ss' <- string ss;
                  space;
                  return ss'}
        in try symbol'

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }

decimalDigits :: Parser String
decimalDigits = do { d <- char '.' ;
                     rm <- digits ;
                     return $ d:rm }

integer :: Parser String
integer = fmap read $ try negDigits <|> digits

doubleDigits :: Parser String
doubleDigits = do { ds <- try negDigits <|> digits ;
                    rs <- try decimalDigits <|> return "" ;
                    return $ ds ++ rs }
-- | A function that reads the value of the string from the expression of Parser String type
double :: Parser Double
double = fmap read $ doubleDigits

-- | A function that wraps exprssion with parentheses
paren :: Parser a -> Parser a 
paren p = do { char '(';
               cs <- p;
               char ')';
               return cs}

-- ** Subordinate parser functions

myconst :: Parser (Expr Double)
myconst = do {xs <- double;
            return $ Const xs}

varX :: Parser (Expr a)
varX = do {x<- symbol "x";
           return $ Var x}

varY :: Parser (Expr a)
varY = do {y<- symbol "y";
           return $ Var y}

var :: Parser (Expr a)
var = varX <|> varY


addOp :: Parser (Expr a -> Expr a -> Expr a)
addOp = do { symbol "!+";
             return Add}

subOp :: Parser (Expr a -> Expr a -> Expr a)
subOp = do { symbol "!+";
             return Sub}

multOp :: Parser (Expr a -> Expr a -> Expr a)
multOp = do { symbol "!*";
             return Mult}

divOp :: Parser (Expr a -> Expr a -> Expr a)
divOp = do {symbol "!/";
             return Div}

trigOp :: Parser (Expr a->Expr a->Expr a)
trigOp = (do{symbol "sin"; return Sin})
  <|> (do{symbol "cos"; return Cos})

naleOP :: Parser (Expr a->Expr a->Expr a)
naleOP = (do{symbol "ln"; return Ln})
  <|>(do{symbol "e^"; return NatExp})

leOP :: Parser (Expr a->Expr a-> Expr a)
leOP = (do{symbol "log"; return Log})
  <|>(do{symbol "^"; return Exp})

basic :: Parser (Expr Double)
basic = myconst <|> var

factor :: Parser (Expr Double)
factor = (paren exprA) <|> basic

-- ** Integrated parser function
exprA :: Parser (Expr Double)
exprA = factor `chainl1` (addOp<|>
                       (subOp<|>
                       (multOp<|>
                       (divOp<|>
                       (trigOp<|>
                       (naleOP<|>leOP))))))
