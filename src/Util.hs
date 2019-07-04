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

constraintsRow :: String -> [String] -> [[String]] -> Bool
constraintsRow n h rs
    | null h                                                 = error "Empty header"
    | ordNub h /= h                                          = error "Duplicate column name"
    | null rs                                                = error "Empty rows"
    | any null rs                                            = error "Empty row"
    | False `elem` map ((== (length . head) rs) . length) rs = error "Row size mismatch"
    | otherwise                                              = True

constraintsColumn :: String -> [String] -> [[String]] -> Bool
constraintsColumn n h cs
    | null h                                                 = error "Empty header"
    | ordNub h /= h                                          = error "Duplicate column name"
    | null cs                                                = error "Empty columns"
    | any null cs                                            = error "Empty column"
    | False `elem` map ((== (length . head) cs) . length) cs = error "Column size mismatch"
    | otherwise                                              = True
