module BoolParser
    (
    ) where

import AParser
import Control.Applicative
import qualified Data.Map as M
import Data.Char (isDigit, isUpper, isSpace, isAlpha, isAlphaNum)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> (pure [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (++) (toListable p) (zeroOrMore p)

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

spacesOne :: Parser String
spacesOne = oneOrMore (satisfy isSpace)

type Ident = String
type Cnst = Bool

data Factor = V Ident | C Cnst | NOT Factor | BR BExpr
    deriving Show

data Term = AndTerm Factor [Factor]
    deriving Show

data BExpr = OrExpr Term [Term]
    deriving Show

ignoreLeftSpaces :: Parser a -> Parser a
ignoreLeftSpaces p = spaces *> p

ignoreRightSpaces :: Parser a -> Parser a
ignoreRightSpaces p = p <* spaces

ignoreSpaces :: Parser a -> Parser a
ignoreSpaces = ignoreLeftSpaces . ignoreRightSpaces

posBool :: Parser Bool
posBool = (fmap (\_ -> True) (stringParser "True")) <|> (fmap (\_ -> False) (stringParser "False"))

parseLP :: Parser Char
parseLP = ignoreSpaces (satisfy (== '('))

parseRP :: Parser Char
parseRP = ignoreSpaces (satisfy (== ')'))

parseCnst :: Parser Cnst
parseCnst = (fmap (\_ -> True) (stringParser "True")) <|> (fmap (\_ -> False) (stringParser "False"))

identParser :: Parser Ident
identParser = liftA2 (++) (oneOrMore (satisfy isAlpha)) (zeroOrMore (satisfy isAlphaNum))

parseFactor :: Parser Factor
parseFactor = C   <$> parseCnst
         <|> (V   <$> identParser)
         <|> (NOT <$> ((stringParser "!") *> spaces *> parseFactor))
         <|> (BR  <$> (parseLP *> parseBExpr <* parseRP))

parseTerm :: Parser Term
parseTerm = AndTerm <$> parseFactor <*> (zeroOrMore $ spaces *> (stringParser "and") *> spaces *> parseFactor)

parseBExpr :: Parser BExpr
parseBExpr = ignoreSpaces $
            OrExpr <$> parseTerm <*> (zeroOrMore $ spaces *> (stringParser "or") *> spaces *> parseTerm)

parse :: String -> Maybe BExpr
parse s = case runParser parseBExpr s of
    Just (a, "") -> Just a
    Just (a,  _) -> Nothing
    Nothing      -> Nothing

calcFactor :: M.Map String Bool -> Factor -> Bool
calcFactor _ (C   c) = c
calcFactor m (V   v) = M.findWithDefault True v m
calcFactor m (NOT f) = not $ calcFactor m f
calcFactor m (BR  e) = calcExpr m e

calcTerm :: M.Map String Bool -> Term -> Bool
calcTerm m (AndTerm f fs) = (calcFactor m f) && (and $ map (calcFactor m) fs)

calcExpr :: M.Map String Bool -> BExpr -> Bool
calcExpr m (OrExpr t ts) = (calcTerm m t) || (or $ map (calcTerm m) ts)
