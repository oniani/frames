{- |
Module      :  Parse.hs
Description :  Parsing files
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Parse
    ( readCSV
    ) where

import Prelude hiding        (readFile)
import Data.ByteString.Lazy  (readFile)
import Data.ByteString.Char8 (unpack)
import Data.ByteString       (ByteString)
import Data.Vector           (Vector, toList)
import Data.Csv              (HasHeader(NoHeader), decode)
import Data.Char             (toLower)

-- Convert a vector of vectors of bytestrings to a list of lists of strings
bulkByteStringToStringConverter :: Vector (Vector ByteString) -> [[String]]
bulkByteStringToStringConverter vec = [ map unpack xs | xs <- map toList $ toList vec]

-- Read a CSV file
readCSV :: String -> IO [[String]]
readCSV filename
    | format /= ".csv" = error "Unknown file format"
    | otherwise        = do csvData <- readFile filename
                            case decode NoHeader csvData of
                                Left  err -> error err
                                Right vec -> return $
                                             bulkByteStringToStringConverter
                                             (vec::(Vector (Vector ByteString)))
      where
        format = map toLower $ drop (length filename - 4) filename
