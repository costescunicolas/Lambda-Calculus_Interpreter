module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative

import Lambda
import Binding
import Data.Void (vacuous)
import Distribution.Simple.Utils (xargs)
import Default (pair)
import Data.Char (isLower, isDigit, isUpper)
import Text.XHtml.Transitional (white)
import System.Console.Terminfo (restoreDefaultColors)



newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- 2.1. / 3.2.

-- parse (Parser p) s = p s

-- parseLine ai grija la spatii

isMacroCharacter :: Char -> Bool
isMacroCharacter c = isUpper c || isDigit c

failParser :: Parser a
failParser = Parser $ \s -> Nothing

charParser :: Char -> Parser Char
charParser c = Parser $ \s ->
				case s of
					[] -> Nothing
					(x:xs) -> if (x == c) then Just(x, xs) else Nothing

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s -> 
						case s of
							[] -> Nothing
							(x:xs) -> if p x then Just(x, xs) else Nothing

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    mp >>= f = Parser $ \s ->
        case parse mp s of
            Nothing -> Nothing  
            Just (v, r) -> parse (f v) r  

    return x = Parser $ \s -> Just (x, s)  

instance Applicative Parser where
	(<*>) :: Parser(a -> b) -> Parser a -> Parser b
	af <*> mp = 
		do
			f <- af
			p <- mp
			return $ f p
	pure = return

instance Functor Parser where
	fmap :: (a -> b) -> Parser a -> Parser b
	fmap f mp = 
		do
			p <- mp
			return $ f p

instance Alternative Parser where
	empty = failParser
	p1 <|> p2 = Parser $ \s -> 
		case parse p1 s of
			Nothing -> parse p2 s
			Just(v, r) -> Just(v, r)


plusParser :: Parser a -> Parser [a]
plusParser p = 
	do
		x <- p
		xs <- starParser p
		return (x:xs)

starParser :: Parser a -> Parser [a]
starParser p = plusParser p <|> return []

varParser :: Parser String
varParser = plusParser $ predicateParser isLower

macroParser :: Parser String
macroParser = plusParser $ predicateParser isMacroCharacter

varLambdaParser :: Parser Lambda
varLambdaParser = Var <$> varParser

whitespaceParser :: Parser String
whitespaceParser = plusParser (charParser ' ')


absLambdaParser :: Parser Lambda
absLambdaParser = 
	do
		charParser '\\'
		x <- varParser
		charParser '.'
		e <- lambdaParser
		return $ Abs x e


appLambdaParser :: Parser Lambda
appLambdaParser = 
	do
		charParser '('
		e1 <- lambdaParser
		whitespaceParser
		e2 <- lambdaParser
		charParser ')'
		return $ App e1 e2


macroLambdaParser :: Parser Lambda
macroLambdaParser = Macro <$> macroParser

lambdaParser :: Parser Lambda
lambdaParser = appLambdaParser <|> absLambdaParser <|> varLambdaParser <|> macroLambdaParser

parseLambda :: String -> Lambda
parseLambda s = 
	case parse lambdaParser s of
		Just(e, _) -> e

-- 3.3.

splitOnChar :: Char -> String -> [String]
splitOnChar _ [] = [""]
splitOnChar c (x:xs)
	| x == c = "" : splitOnChar c xs
	| otherwise = let (y:ys) = splitOnChar c xs
				  in (x:y):ys				 


parseLine :: String -> Either String Line
parseLine s 
	| '=' `elem` s =
		let expr = splitOnChar '=' s
		    lhs = head expr
		    rhs = last expr
		in case parse lambdaParser rhs of
			Just(l, "") -> Right $ Binding lhs l
			_ -> Left "parse error"
	| otherwise = 
		case parse lambdaParser s of
			Just (l, "") -> Right $ Eval l
			_ -> Left "parse error"


