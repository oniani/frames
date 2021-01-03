{- |
Module      :  PrettyPrint.hs
Description :  Pretty-printing functions for miniframes
Copyright   :  (c) David Oniani
License     :  MIT License

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

A module for pretty-printing.
-}

module PrettyPrint
    ( coloredPutStrLn
    , prettyPrint1D
    , prettyPrint1DV
    , prettyPrint2D
    ) where

import Data.List (transpose)
import System.Console.ANSI
import Text.PrettyPrint.Boxes

-- | A function to get the colored output from putStrLn command
coloredPutStrLn :: String -> IO ()
coloredPutStrLn s = do
    setSGR [SetColor Foreground Dull Green]
    putStrLn s
    setSGR []

-- | A helper function to get the colored output from putStrLn command
coloredPrintBox :: Box -> IO ()
coloredPrintBox b = do
    setSGR [SetColor Foreground Dull Green]
    printBox b
    setSGR []

-- | A function to pretty-print the data in the 1D tabular format
prettyPrint1D :: [String] -> IO ()
prettyPrint1D r = coloredPrintBox $ hsep 4 left (map (vcat left . map text . (:[])) r)

-- | A function to pretty-print the data in the 1D tabular format
prettyPrint1DV :: [String] -> IO ()
prettyPrint1DV c = coloredPrintBox $ vsep 0 left (map (hcat left . map text . (:[])) c)

-- | A function to pretty-print the data in the 2D tabular format
prettyPrint2D :: [[String]] -> IO ()
prettyPrint2D rs = coloredPrintBox $ hsep 4 left (map (vcat left . map text) (transpose rs))
