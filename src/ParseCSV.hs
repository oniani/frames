-- This CSV reader is the extended version of
-- that taken from the book Real World Haskell.
-- Link: http://book.realworldhaskell.org/read/using-parsec.html

module ParseCSV
    (readCSV)
    where

import Data.Either
import Text.ParserCombinators.Parsec

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

quotedCell = do char '"'
                content <- many quotedChar
                char '"' <?> "quote at end of cell"
                return content

cell    = quotedCell <|> many (noneOf ",\n\r")
line    = sepBy cell (char ',')
csvFile = endBy line eol

parseHelper :: String -> Either ParseError [[String]]
parseHelper = parse csvFile "(unknown)"

parseCSV :: String -> [[String]]
parseCSV input = fromRight [] (parseHelper input)

readCSV :: String -> IO [[String]]
readCSV filename = do contents <- readFile filename
                      return (parseCSV contents)