module Parser where

import Data.Char ( isDigit, isAlpha, isAlphaNum )
import Control.Applicative ( Applicative(liftA2), Alternative(some, many, empty, (<|>)) )
import AST ( Identifier (..), Type (..), AST(..) )

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

consumeChar :: Parser Char
consumeChar = Parser f
  where
    f [] = Nothing
    f (c:cs) = Just (cs, c)

validateWith :: (a -> Bool) -> Parser a -> Parser a
validateWith condition (Parser f) = Parser h
  where
    h input = do
      (input', result) <- f input
      if condition result
        then Just (input', result)
        else Nothing

parseChar :: Char -> Parser Char
parseChar char = validateWith (char ==) consumeChar
  where
    f [] = Nothing
    f (c:cs)
      | c == char = Just (cs, c)
      | otherwise = Nothing

parseAlpha :: Parser Char
parseAlpha = validateWith isAlpha consumeChar

parseAlphaNum :: Parser Char
parseAlphaNum = validateWith isAlphaNum consumeChar

parseDigit :: Parser Char
parseDigit = validateWith isDigit consumeChar

parseUnderscore :: Parser Char
parseUnderscore = parseChar '_'

insertInto :: Parser a -> Parser [a] -> Parser [a]
insertInto = liftA2 (:)

parseIdentifierString :: Parser String
parseIdentifierString = insertInto parseFirstIdentifierChar (many parseRestIdentifierChar)
  where
    parseFirstIdentifierChar = parseAlpha <|> parseUnderscore
    parseRestIdentifierChar = parseAlphaNum <|> parseUnderscore

parseIdentifier :: Parser Identifier
parseIdentifier = Identifier <$> parseIdentifierString

parseType :: Parser Type
parseType = Type <$> parseIdentifierString

parseColon :: Parser Char
parseColon = parseChar ':'

parseAssign :: Parser Char
parseAssign = parseChar '='

parseIntLiteral :: Parser AST
parseIntLiteral = IntLiteral . read <$> some parseDigit

parseDeclaration :: Parser AST
parseDeclaration = Declaration <$> parseIdentifier <*> (parseColon *> parseType) <*> (parseAssign *> parseIntLiteral)