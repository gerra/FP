import AParser
import Control.Applicative
import Data.Char (isDigit, isUpper, isSpace, isAlpha, isAlphaNum)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> (pure [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (++) (toListable p) (zeroOrMore p)

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = liftA2 (++) (oneOrMore (satisfy isAlpha)) (zeroOrMore (satisfy isAlphaNum))

type Ident = String

data Atom = N Integer | I Ident
    deriving Show

data SExpr = A Atom | Comb [SExpr]
    deriving Show

ignoreLeftSpaces :: Parser a -> Parser a
ignoreLeftSpaces p = spaces *> p

ignoreRightSpaces :: Parser a -> Parser a
ignoreRightSpaces p = p <* spaces

ignoreSpaces :: Parser a -> Parser a
ignoreSpaces = ignoreLeftSpaces . ignoreRightSpaces

parseN :: Parser Atom
parseN = fmap N posInt

parseI :: Parser Atom
parseI = fmap I ident

parseAtom :: Parser Atom
parseAtom = ignoreSpaces (parseN <|> parseI)

parseLP :: Parser Char
parseLP = ignoreSpaces (satisfy (== '('))

parseRP :: Parser Char
parseRP = ignoreSpaces (satisfy (== ')'))

parseP :: Parser [SExpr]
parseP = ((parseLP *> (zeroOrMore parseSExpr)) <* parseRP)

--parseNothing :: Parser a
--parseNothing = fm

parseSExpr :: Parser SExpr
parseSExpr = (fmap A parseAtom) <|> (fmap Comb parseP)