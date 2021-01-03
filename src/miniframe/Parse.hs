{- |
Module      :  Parse.hs
Description :  Parsing files
Copyright   :  (c) David Oniani
License     :  MIT License

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

Parse the CSV files.
-}

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

-- Bulk conversion gadget
bulkConverter :: Vector (Vector ByteString) -> [[String]]
bulkConverter vec = [map unpack xs | xs <- map toList $ toList vec]

-- Read a CSV file
readCSV :: String -> IO [[String]]
readCSV fn
    | fmt /= ".csv" = error "Unknown file format"
    | otherwise     = do
        csv <- readFile fn
        case decode NoHeader csv of
            Left  err -> error err
            Right vec -> return $
                         bulkConverter
                         (vec :: (Vector (Vector ByteString)))
      where
        fmt = map toLower $ drop (length fn - 4) fn
