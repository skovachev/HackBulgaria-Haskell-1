module Parser where

import Control.Applicative
import Control.Arrow (first)


newtype Parser a =
  Parser { parse :: String -> Maybe (a, String) }

-- parse parserInstance = parserFunc

instance Functor Parser where
  -- fmap f (Parser parseFunc) = Parser (\str -> applyFunc f (parseFunc str))
  --   where applyFunc f (a, b) = Just (f a, b)

  fmap f (Parser parseFunc) = Parser $ \str -> case parseFunc str of
                                               Just (x, restStr) -> Just (f x, restStr)
                                               Nothing           -> Nothing

instance Applicative Parser where
  -- returns a parser whose func is 'the creation of a Maybe, unfinished tuple with "a" as its 1st element'
  pure a = Parser $ Just . (,) a

  -- return a parser by extracting the function (in place of 'a') from parserA
  -- and applying it to the 'a' of parserB if it's not Nothing
  Parser p <*> Parser k = Parser $ \s -> case p s of
                                         Just (func, restStr) -> first func <$> k restStr
                                         Nothing              -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser (\(x:xs) -> if f x then Just (x, xs) else Nothing)

char :: Char -> Parser Char
char c = satisfy (==c)

openingBrace :: Parser Char
-- openingBrace = Parser f
--     where f (x:y:xs) | x == '(' = Just (y, xs)
--                      | otherwise = Nothing
--           f _        = Nothing
openingBrace = char '('

closingBrace :: Parser Char
-- closingBrace = Parser f
--     where f (x:xs) | x == ')' = Just (x, xs)
--                    | otherwise = Nothing
--           f _      = Nothing
closingBrace = char ')'

inBraces :: Parser a -> Parser a
inBraces p = openingBrace *> p <* closingBrace

instance Alternative Parser where
  -- return parser whose parse function returns empty -> Nothing
  empty = Parser $ \str -> Nothing

  -- return a parse whose parse function returns the result of parserA's func if its not empty, 
  -- or the result of parserB's func otherwise
  -- Parser p <|> Parser k = Parser $ \str -> case p str of 
  --                                          Just (x, restStr) -> Just (x, restStr)
  --                                          Nothing           -> k str

  Parser p <|> Parser k = Parser $ \str -> (p str) <|> (k str)


-- psevdo kod
-- oneOrMore p -> p + zeroOrMore p
-- zeroOrMore -> empty | oneOrMore

oneOrMore, zeroOrMore  :: Parser a -> Parser [a]
-- try the parser once and then continue with zeroOrMore, all the while merging the results in a list using (:)
oneOrMore parser = liftA2 (:) parser $ zeroOrMore parser

-- either we have 1 item (or more), or we need to wrap an empty list with a functor -> use 'pure'
zeroOrMore parser = oneOrMore parser <|> pure []

