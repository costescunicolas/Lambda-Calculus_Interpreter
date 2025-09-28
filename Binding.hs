module Binding where

import Lambda
import Distribution.Utils.Structured (containerStructure)
import Distribution.Simple.Setup (ConfigFlags(configProgramPathExtra))
import Language.Haskell.TH (valD)
import Data.Void (vacuous)
import System.Console.Terminfo (restoreDefaultColors)
import Distribution.Simple.Utils (existsAndIsMoreRecentThan)

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.

replaceMacro :: Context -> Lambda -> Either String Lambda
replaceMacro ctx (Var x) = Right $ Var x
replaceMacro ctx (Macro s) = 
  case lookup s ctx of
    Nothing -> Left $ "Eroare: Nu gasim " ++ s
    Just x -> Right x
replaceMacro ctx (Abs x e) = 
  do
    e' <- replaceMacro ctx e
    return $ Abs x e'
replaceMacro ctx (App e1 e2) = 
  do
    e1' <- replaceMacro ctx e1
    e2' <- replaceMacro ctx e2
    return $ App e1' e2'



simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step lambda = 
  do
    newLambda <- replaceMacro ctx lambda
    return $ simplify step newLambda
 
    

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
