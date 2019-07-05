{- |
Module      :  Util.hs
Description :  This is a description of the module
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

Various helper utilities.
-}

module Util
    ( constraintsRow
    , constraintsColumn
    ) where

import Optimize

import qualified Data.List as List

constraintsRow :: String -> [String] -> [[String]] -> a -> a
constraintsRow n h rs a
    | null h                                                 = error "Empty header"
    | length h /= length rs                                  = error "Header size mismatch"
    | length h /= length (head rs)                           = error "Header-row size mismatch"
    | ordNub h /= h                                          = error "Duplicate column name"
    | null rs                                                = error "Empty rows"
    | any null rs                                            = error "Empty row"
    | False `elem` map ((== (length . head) rs) . length) rs = error "Row size mismatch"
    | otherwise                                              = a

constraintsColumn :: String -> [String] -> [[String]] -> a -> a
constraintsColumn n h cs a
    | null h                                                 = error "Empty header"
    | length h /= length (List.transpose cs)                 = error "Header size mismatch"
    | length h /= length (head (List.transpose cs))          = error "Header-row size mismatch"
    | ordNub h /= h                                          = error "Duplicate column name"
    | null cs                                                = error "Empty columns"
    | any null cs                                            = error "Empty column"
    | False `elem` map ((== (length . head) cs) . length) cs = error "Column size mismatch"
    | otherwise                                              = a
