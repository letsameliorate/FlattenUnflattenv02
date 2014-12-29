module Term where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language

data DTerm = DFreeApp String [DTerm] -- Free Variable Application
           | DBoundApp Int [DTerm] -- Bound Variable Application
           | DConApp String [DTerm] -- Constructor Application
           | DLambda String DTerm -- Lambda Abstraction
           | DLet String DTerm DTerm -- Let Expression
           | DCase DTerm [(String, [String], DTerm)] -- Case Expression
           | DFunApp String [DTerm] -- Function Application
           | DWhere DTerm [(String, DTerm)] -- Local Function Definition
  deriving (Show)

potDef = emptyDef
         { commentStart     = "{-|",
           commentEnd       = "|-}",
           commentLine      = "--",
           nestedComments   = True,
           identStart       = lower,
           identLetter      = do letter <|> oneOf "_'",
           reservedNames    = ["case", "of", "let", "in", "where"],
           caseSensitive    = True
         }

lexer = T.makeTokenParser potDef

symbol      = T.symbol lexer
bracks      = T.parens lexer
semic       = T.semi lexer
comm        = T.comma lexer
identifier  = T.identifier lexer
reserved    = T.reserved lexer
natural     = T.natural lexer


{-|
    Parser of Pot to DTerm
|-}

con = do
         c <- upper
         cs <- many letter
         return (c:cs)

makeWhere e [] = e
makeWhere e fs = let (fnames, _) = unzip fs
                 in makeFuns fnames (DWhere e fs)

makeFuns fnames (DFreeApp x dts) = if x `elem` fnames
                                   then DFunApp x (map (makeFuns fnames) dts)
                                   else DFreeApp x (map (makeFuns fnames) dts)
makeFuns fnames (DBoundApp i dts) = DBoundApp i (map (makeFuns fnames) dts)
makeFuns fnames (DConApp c dts) = DConApp c (map (makeFuns fnames) dts)
makeFuns fnames (DLambda x dt) = DLambda x (makeFuns fnames dt)
makeFuns fnames (DLet x dt0 dt1) = DLet x (makeFuns fnames dt0) (makeFuns fnames dt1)
makeFuns fnames (DCase csel bs) = DCase (makeFuns fnames csel) (map (\(c, xs, dt) -> (c, xs, makeFuns fnames dt)) bs)
makeFuns fnames (DFunApp f dts) = DFunApp f (map (makeFuns fnames) dts)
makeFuns fnames (DWhere dt ts) = DWhere (makeFuns fnames dt) (map (\(x, dt) -> (x, makeFuns fnames dt)) ts)


{-|
    Create parsers
|-}

fundef = do
            f <- identifier
            symbol "="
            e <- expr
            return(f, e)

expr = buildExpressionParser prec term

prec = []

term =     do
              f <- identifier
              as <- many atom
              fs <-     do
                           reserved "where"
                           fs <- sepBy1 fundef semic
                           return fs
                    <|> do
                           spaces
                           return []
              return (makeWhere (DFunApp f as) fs)
       <|> do
              x <- identifier
              as <- many atom
              return (DFreeApp x as)
       <|> do
              symbol "\\"
              xs <- many1 identifier
              symbol "."
              e <- expr
              return (foldr (\x t -> (DLambda x t)) e xs)
       <|> do
              reserved "case"
              e <- expr
              reserved "of"
              bs <- sepBy1 branch (symbol "|")
              return (DCase e bs)
       <|> do
              reserved "let"
              x <- identifier
              symbol "="
              e0 <- expr
              reserved "in"
              e1 <- expr
              return (DLet x e0 e1)

atom =     do
              x <- identifier
              return (DFreeApp x [])
       <|> do
              c <- con
              es <-     do
                           es <- bracks (sepBy1 expr comm)
                           return es
                    <|> do
                           spaces
                           return []
              return (DConApp c es)
       <|> do
              e <- bracks expr
              return e

branch =    do
               c <- con
               xs <-    do
                           xs <- bracks (sepBy1 identifier comm)
                           return xs
                    <|> do
                           spaces
                           return []
               symbol "->"
               e <- expr
               return (c, xs, e)
