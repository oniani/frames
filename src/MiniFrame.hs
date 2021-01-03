{- |
Module      :  MiniFrame.hs
Description :  Module imports
Copyright   :  (c) David Oniani
License     :  MIT License

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

Module imports for MiniFrame.
It includes MiniFrame.Frames and MiniFrame.Relational.
Other modules are not exported since they only serve
as "helper" modules and do not have much to do with
the functionalities of the actual package.
-}

module MiniFrame
    ( module MiniFrame.Frames
    , module MiniFrame.Relational
    ) where

import MiniFrame.Frames
import MiniFrame.Relational
