{- |
Module      :  PrettyPrint.hs
Description :  Pretty-printing functions for tabular 2D data frames
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

A module for pretty-printing.
-}

module PrettyPrint
    ( coloredPutStrLn
    , prettyPrint1D
    , prettyPrint2D
    ) where

import Data.List              (transpose)
import System.Console.ANSI
import Text.PrettyPrint.Boxes (Box, printBox, hsep, left, vcat, text)

-- | A function to get the colored output from putStrLn command
coloredPutStrLn :: String -> IO ()
coloredPutStrLn str = do
    setSGR [SetColor Foreground Dull Green]
    putStrLn str
    setSGR []

-- | A helper function to get the colored output from putStrLn command
coloredPrintBox :: Box -> IO ()
coloredPrintBox box = do
    setSGR [SetColor Foreground Dull Green]
    printBox box
    setSGR []

-- | A function to pretty-print the data in the 1D tabular format
prettyPrint1D :: [String] -> IO ()
prettyPrint1D row = coloredPrintBox $ hsep 4 left (map (vcat left . map text . (:[])) row)

-- | A function to pretty-print the data in the 2D tabular format
prettyPrint2D :: [[String]] -> IO ()
prettyPrint2D rows = coloredPrintBox $ hsep 4 left (map (vcat left . map text) (transpose rows))
