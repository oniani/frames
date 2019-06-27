{- |
Module      :  PrettyPrint.hs
Description :  Pretty-printing functions for tabular 2D data frames
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

An implementation of a data frame which comes with various handy operations.
-}

module PrettyPrint
    ( coloredPutStrLn
    , prettyPrint1D
    , prettyPrint2D
    ) where

import System.Console.ANSI
import Text.PrettyPrint.Boxes
import Data.List

-- | A helper function to get the colored output from putStr command
coloredPutStrHelper :: ColorIntensity -> Color -> String -> IO ()
coloredPutStrHelper fgci fgc str = do
    setSGR [SetColor Foreground fgci fgc]
    putStr str
    setSGR []

-- | A helper function to get the colored output from putStrLn command
coloredPutStrLnHelper :: ColorIntensity -> Color -> String -> IO ()
coloredPutStrLnHelper fgci fgc str = do
    setSGR [SetColor Foreground fgci fgc]
    putStrLn str
    setSGR []

-- | A helper function to get the colored output from putStrLn command
coloredPrintBoxHelper :: ColorIntensity -> Color -> Box -> IO ()
coloredPrintBoxHelper fgci fgc box = do
    setSGR [SetColor Foreground fgci fgc]
    printBox box
    setSGR []

-- | A function to get the colored output from putStr command
coloredPutStr :: String -> IO ()
coloredPutStr = coloredPutStrHelper Dull Green

-- | A function to get the colored output from putStrLn command
coloredPutStrLn :: String -> IO ()
coloredPutStrLn = coloredPutStrLnHelper Dull Green

-- | A function to get the colored output from printBox command
coloredPrintBox :: Box -> IO ()
coloredPrintBox = coloredPrintBoxHelper Dull Green

-- | A function to pretty-print the data in the 1D tabular format
prettyPrint1D :: [String] -> IO ()
prettyPrint1D row = coloredPrintBox $ hsep 4 left (map (vcat left . map text . (:[])) row)

-- | A function to pretty-print the data in the 2D tabular format
prettyPrint2D :: [[String]] -> IO ()
prettyPrint2D rows = coloredPrintBox $ hsep 4 left (map (vcat left . map text) (transpose rows))
