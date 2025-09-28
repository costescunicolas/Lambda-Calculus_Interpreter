module Lambda where

import Data.List (nub, (\\))
import Foreign.C (e2BIG)
import Distribution.Simple.Program.HcPkg (list)
import Foreign (free)


data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (Abs x e) = nub $ x : vars e
vars (App e1 e2) = nub $ vars e1 ++ vars e2

-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (Abs x e) = freeVars e \\ [x]
freeVars (App e1 e2) = nub $ freeVars e1 ++ freeVars e2
-- 1.3.
generateAllWords :: [String]
generateAllWords = concatMap wordOfLength [1..]
  where
    wordOfLength 0 = [""]
    wordOfLength n = [x : xs | x <- ['a'..'z'], xs <- wordOfLength(n - 1)]

newVar :: [String] -> String
newVar used = head $ filter (`notElem` used) generateAllWords

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (Abs _ e) = isNormalForm e
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x (Var y) e 
  | x == y = e
  | otherwise = Var y
reduce x (App e1 e2) e3 = App (reduce x e1 e3) (reduce x e2 e3)
reduce x (Abs y e1) e2
  | x == y = Abs y e1
  | y `notElem` freeVars e2 = Abs y $ reduce x e1 e2
  | otherwise = let z = newVar (freeVars e1 ++ freeVars e2)
                    e3 = Abs z $ reduce y e1 $ Var z
                in reduce x e3 e2
-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (Var x) = Var x
normalStep (Abs x e) = Abs x $ normalStep e
normalStep (App (Abs x e1) e2) = reduce x e1 e2
normalStep (App e1 e2) 
  | isNormalForm e1 = App e1 (normalStep e2)
  | otherwise = App (normalStep e1) e2


-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (Var x) = Var x
applicativeStep (Abs x e) = Abs x $ applicativeStep e
applicativeStep (App (Abs x e1) e2)
  | isNormalForm e2 = reduce x e1 e2
  | otherwise = App (Abs x e1) (applicativeStep e2)
applicativeStep (App e1 e2)
  | isNormalForm e1 = App e1 (applicativeStep e2)
  | otherwise = App (applicativeStep e1) e2
-- 1.8.

simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step lambda
  | isNormalForm lambda = [lambda]
  | otherwise = lambda : simplify step (step lambda)

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
