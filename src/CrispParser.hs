module CrispParser where

import Data.Char ( isDigit, isAlpha, isSpace, toLower, toUpper )
import Data.Maybe ( isNothing )
import GHC.Base ( join, Applicative(liftA2), Alternative(many, empty, (<|>)) )
import Data.Map.Strict ( Map, empty, insert )

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser g) = Parser h
    where
      h input = do
        (input', parsed) <- g input
        return (input', f parsed)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  Parser f <*> Parser g = Parser h
    where
      h input = do
        (input', fn) <- f input
        (input'', x) <- g input'
        return (input'', fn x)

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser f <|> Parser g = Parser h
    where
      h input = f input <|> g input

parseOne :: Parser Char
parseOne = Parser f
  where
    f [] = Nothing
    f (c:cs) = Just (cs, c)

parseConstChar :: Char -> Parser Char
parseConstChar x = Parser f
  where
    f [] = Nothing
    f (c:cs)
      | c == x = Just (cs, c)
      | otherwise = Nothing

parseCharNotEquals :: Char -> Parser ()
parseCharNotEquals x = Parser f
  where
    f [] = Nothing
    f stream@(c:cs)
      | c /= x = Just (stream, ())
      | otherwise = Nothing

parseConstString :: String -> Parser String
parseConstString = traverse parseConstChar

applyAny :: (Foldable t, Alternative f) => t (f a) -> f a
applyAny = foldl (<|>) GHC.Base.empty

parseWhile :: (Char -> Bool) -> Parser String
parseWhile pred = Parser f
  where
    f input = let (answer, rest) = span pred input
      in case answer of
        "" -> Nothing
        _ -> Just (rest, answer)

optional :: (Monoid a) => Parser a -> Parser a
optional (Parser f) = Parser g
  where
    g input =
      if isNothing $ f input
        then Just (input, mempty)
        else f input

parseWS :: Parser String
parseWS = parseWhile isSpace

parseEnd :: Parser String
parseEnd = Parser f
  where
    f "" = Just ("", "")
    f _ = Nothing

parseSeparator :: Parser String
parseSeparator = parseWS <|> parseEnd

-- TODO: parse more interesting literals, like binaries, octals and hex

parseInt :: Parser Int
parseInt = read <$> liftA2 (++) minus digits <* noDot
  where
    minus  = optional $ parseConstString "-"
    digits = parseWhile isDigit
    noDot  = parseCharNotEquals '.'

parseFloat :: Parser Float
parseFloat = read . join <$> sequenceA
  [ optional $ parseConstString "-"
  , parseWhile isDigit
  , parseConstString "."
  , parseWhile isDigit
  ]

parseBool :: Parser Bool
parseBool =
  True <$ parseConstString "true" <|>
  False <$ parseConstString "false"

-- TODO: have escaped characters into account (possibly UTF-8 as well)
parseChar :: Parser Char
parseChar = parseConstChar '\'' *> parseOne <* parseConstChar '\''

-- FIXME: this is non-exhaustive
parseIdentifier :: Parser String
parseIdentifier = parseWhile $ \c -> isAlpha c && not (isSpace c)

parseTag :: Parser String
parseTag = parseIdentifier <* parseConstChar ':' <* parseSeparator

parseKeyword :: String -> Parser String
parseKeyword kw = parseIdentifier <* parseSeparator

parseSection :: Parser String
parseSection = applyAny
  [ parseKeyword ".data"
  , parseKeyword ".text"
  ]

parseRegister :: Parser String
parseRegister = applyAny
  [ parseKeyword "$flags"
  , parseKeyword "$bp"
  , parseKeyword "$sp"
  , parseKeyword "$ip"
  ]