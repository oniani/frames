-- The CSV reader is taken from the book Real World Haskell
-- It has minor tweaks and modifications

module ParseCSV
    (parseCSV)
    where

import Text.ParserCombinators.Parsec

cell    = quotedCell <|>  many       (noneOf ",\n\r")
line    = sepBy      cell (char ',')
csvFile = endBy      line eol

quotedCell = do
    char '"'
    content <- many quotedChar
    char '"' <?> "quote at end of cell"
    return content

quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"
