module AParser where

import Control.Applicative
import Data.Char (isDigit, isUpper)

newtype Parser a
    = Parser { runParser :: String -> Maybe (a, String) }

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

kk :: (a -> b) -> Maybe (a, c) -> Maybe (b, c)
kk f Nothing       = Nothing
kk f (Just (a, c)) = Just (f a, c)

instance Functor Parser where
    --fmap f (Parser rp) = Parser $ (kk f) . rp
    fmap f (Parser rp) = Parser $ fmap (first f) . rp

instance Applicative Parser where
    pure a = Parser (\x -> Just (a, x))
    p1 <*> p2 = Parser $ helper p1 p2 where
        helper p1 p2 s = case runParser p1 s of
            Nothing      -> Nothing
            Just (f, s2) -> case runParser p2 s2 of
                Nothing      -> Nothing
                Just (x, s3) -> Just (f x, s3)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f where
    f [] = Nothing
    f (x:xs)
        | p x = Just (x, xs)
        | otherwise = Nothing

oneCharParser :: Char -> Parser Char
oneCharParser c = satisfy (== c)

aParser :: Parser Char
aParser = oneCharParser 'a'

bParser :: Parser Char
bParser = oneCharParser 'b'

spaceParser :: Parser Char
spaceParser = oneCharParser ' '

parserToPairParser :: Parser a -> (Parser (b -> (a, b)))
parserToPairParser = fmap (\c -> (\x -> (c, x)))

abParser :: Parser (Char, Char)
abParser = (parserToPairParser aParser) <*> bParser

abParser_ :: Parser ()
abParser_ = fmap (\x -> ()) abParser

posInt :: Parser Integer
posInt = Parser f where
    f xs
        | null ns = Nothing
        | otherwise = Just (read ns, rest)
        where (ns, rest) = span isDigit xs

parserToIgnoreSnd :: Parser (Integer, Char) -> (Parser (Integer -> [Integer]))
parserToIgnoreSnd = fmap (\p -> (\i -> [fst p, i]))

intPair :: Parser [Integer]
intPair = parserToIgnoreSnd (((parserToPairParser posInt) <*> spaceParser)) <*> posInt

instance Alternative Parser where
    empty = Parser $ \s -> Nothing
    p1 <|> p2 = Parser $ \s -> case runParser p1 s of
        Nothing -> runParser p2 s
        res     -> res

intOrUppercase :: Parser ()
intOrUppercase = (fmap (\x -> ()) (satisfy isUpper)) <|> (fmap (\x -> ()) posInt)


--      helper f Nothing       = Nothing
--      helper f (Just (a, s)) = (f s) <*> (Just (a, s))
        
--helper :: (String -> Maybe (a -> b, String)) -> Maybe (a, String) -> Maybe (b, String)
--helper _ Nothing       = Nothing
--helper f (Just (a, s)) = helper2 (f s) (Just (a, s))

--helper2 :: (Maybe (a -> b, String)) -> Maybe (a, String) -> Maybe (b, String)
--helper2 Nothing       _       = Nothing
--helper2 _             Nothing = Nothing
--helper2 (Just (f, s)) (Just (a, _)) = Just (f a, s)