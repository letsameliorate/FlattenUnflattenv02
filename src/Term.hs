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
           reservedNames    = ["case", "of", "where"],
--           reservedOpNames  = ["~", "/\\", "\\/", "<=>", "=>"],
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

prog = do
          e <- expr
          fs <-     do
                       reserved "where"
                       fs <- sepBy1 fundef semic
                       return fs
                <|> do
                       spaces
                       return []
          return (makeWhere e fs)

fundef = do
            f <- identifier
            symbol "="
            e <- expr
            return(f, e)

expr = buildExpressionParser prec term

prec = []
{-|
prec = [ [unop "~" (Fun)],
       ]
       where
       op o t assoc = Infix (do
                                reservedOp o
                                return (\x y -> Apply (t x) y)
                            ) assoc
       unop o t     = Prefix (do
                                 reservedOp o
                                 return (\x -> Apply t x)
                             )
|-}

term = do