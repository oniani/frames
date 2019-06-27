module Colors
    ( coloredPutStr
    , coloredPutStrLn
    , coloredPrintBox
    ) where

import System.Console.ANSI
import Text.PrettyPrint.Boxes

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
