module Parser (parseFormat) where

import Text.ParserCombinators.Parsec

import Format (Format(..), identifiers)

-- parsing special identifiers

leftBrace, rightBrace :: GenParser Char st Char
leftBrace = char '{'
rightBrace = char '}'

identifier :: GenParser Char st Format
identifier = Special <$> choice (map (try . string) identifiers)

braces :: GenParser Char st a -> GenParser Char st a
braces = between leftBrace rightBrace

special :: GenParser Char st Format
special = braces identifier

-- parsing the rest

constant :: GenParser Char st Format
constant = Const <$> many1 (noneOf "{}")

format :: GenParser Char st [Format]
format = many1 $ (special <|> constant)

parseFormat :: String -> Either ParseError [Format]
parseFormat = parse format "(unknown)"