module Term where

data DTerm = DFreeApp String [DTerm] -- Free Variable Application
           | DBoundApp Int [DTerm] -- Bound Variable Application
           | DConApp String [DTerm] -- Constructor Application
           | DLambda String DTerm -- Lambda Abstraction
           | DLet String DTerm DTerm -- Let Expression
           | DCase DTerm [(String, [String], DTerm)] -- Case Expression
           | DFunApp String [DTerm] -- Function Application
           | DWhere DTerm [(String, DTerm)] -- Local Function Definition
  deriving (Show)

