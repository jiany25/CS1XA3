module ExprParser where

import ExprType-- ^ Import the Expr a datatype defined previously to define the parsers for them

import Text.Parsec-- ^ To use the Parsec Combinators to define the parsing functions for Expr a datatype
import Text.Parsec.String-- ^ 

parseExpr :: String -> Expr Double -- ^ take a string and parse to the desired expression of (Expr double) datatype
parseExpr ss = case parse exprA "" ss of
                  -- ^ parse :: Stream s Data.Functor.Identity.Identity t =>
                  --Parsec s () a -> SourceName -> s -> Either ParseError a
                  --type Paser = Parsec String ()
                  --so   Paser a = Parsec String () a
                  --so   Paser (Expr Double) = Parsec String () (Expr Double)
                  --Parsec String () (Expr Double) -> SourceName -> String -> Either ParseError (Expr Double)
                  --          Parser (Expr Double) -> String -> String -> Either ParseError (Expr Double)
                  --exprD :: Parser (Expr Double)
                  --so parse exprD "" ss = Either ParseError (Expr Double)
                  Left err -> error $ "Parse Error: "++show err -- ^ Left ParseError is the error message
                  Right expr -> expr -- ^ Right (Expr double) returns the expression of Expr Double

symbol :: String -> Parser String-- ^ A function that parse certain symbols in a string expression
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

double :: Parser Double
double = fmap read $ doubleDigits

paren :: Parser a -> Parser a -- ^ A function that wraps exprssion with parentheses
paren p = do { char '(';
               cs <- p;
               char ')';
               return cs}

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

exprA :: Parser (Expr Double)
exprA = factor `chainl1` (addOp<|>
                       (subOp<|>
	                     (multOp<|>
	                     (divOp<|>
	                     (trigOp<|>
	                     (naleOP<|>leOP))))))