{- |
Module      :  MiniFrame.hs
Description :  Module implements the MiniFrame and its core functionalities
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable
-}

module MiniFrame
    ( newMiniFrame
    , getName
    , getHeader
    , getFields
    , printName
    , printHeader
    , printFields
    , printMF
    ) where

import Data.List

type Name   = String
type Header = [String]
type Fields = [[String]]

data MiniFrame = MiniFrame
    { name   ::  Name        -- Name of the MiniFrame
    , header ::  Header      -- Header columns of the MiniFrame
    , fields ::  Fields }    -- Fields of the MiniFrame
    deriving (Eq)


-- | MiniFrame
newMiniFrame :: MiniFrame
newMiniFrame = MiniFrame name header fields
    where
        name   = "Table name"
        header = ["ColumnOne","ColumnTwo","ColumnThree","ColumnFour"]
        fields = [["Row1-Col1","Row1-Col2","Row1-Col3","Row1-Col4"],["Row2-Col1","Row2-Col2","Row2-Col3","Row2-Col4"],
                  ["Row3-Col1","Row3-Col2","Row3-Col3","Row3-Col4"],["Row4-Col1","Row4-Col2","Row4-Col3","Row4-Col4"],
                  ["Row5-Col1","Row5-Col2","Row5-Col3","Row5-Col4"],["Row6-Col1","Row6-Col2","Row6-Col3","Row6-Col4"],
                  ["Row7-Col1","Row7-Col2","Row7-Col3","Row7-Col4"],["Row8-Col1","Row8-Col2","Row8-Col3","Row8-Col4"]]

-- | Get the name of the data frame
getName :: MiniFrame -> String
getName (MiniFrame name header fields) = name

-- | Get the header of the data frame
getHeader :: MiniFrame -> [String]
getHeader (MiniFrame name header fields) = header

-- | Get the fields of the data frame
getFields :: MiniFrame -> [[String]]
getFields (MiniFrame name header fields) = fields

-- | Print the name of the data frame
printName :: MiniFrame -> IO ()
printName (MiniFrame name header fields) = print name

-- | Print the header of the data frame
printHeader :: MiniFrame -> IO ()
printHeader (MiniFrame name header fields) = print header

-- | Print the fields of the data frame
printFields :: MiniFrame -> IO ()
printFields (MiniFrame name header fields) = mapM_ print fields

-- | Print the data frame
printMF :: MiniFrame -> IO ()
printMF (MiniFrame name header fields) = do
                                            putStrLn (" " ++ replicate (length name) '_' ++ "\n|" ++ name ++ "|\n " ++ replicate (length name) '-' ++ "\n")
                                            putStrLn (unwords header)
                                            putStrLn (replicate (maximum (map (length . show) fields)) '=')
                                            mapM_ putStrLn [intercalate " | " i | i <- fields]
                                            --
                                            -- MIGHT HAVE TO USE IT... THE TRANSPOSE FUNCTION SEEMS TO BE REALLY USEFUL
                                            -- ONCE WE FIND THE MAXIMUM LENGTH OF THE STRING FOR EACH COLUMN, FOR EACH
                                            -- STRING IN THE LIST, ADD (BIGGEST STRING SIZE - SIZE OF CURRENT STRING) NUMBER OF SPACES
                                            --
                                            -- putStrLn (" " ++ replicate (length name) '_' ++ "\n|" ++ name ++ "|\n " ++ replicate (length name) '-' ++ "\n")
                                            -- mapM_ putStr (map (++"|") (init headerForPrettyPrint) ++ [last header ++ "\n"])
                                            -- putStrLn (replicate (maximum (map (length . show) fields)) '=')
                                            -- mapM_ print fields
                                            -- where
                                            --     -- +3 since the first header has to account for ( ["" ) - three characters in total
                                            --     -- +2 since the rest of the headers have to account for just two characters - ( ," )
                                            --     -- We subtract the length of (fst i) since we have to account for the length of the field entry
                                            --     maxLengthPerColumn = transpose [map length i | i <- fields]
                                            --     spacesPerColumn = (maximum (head maxLengthPerColumn) + 3) : [maximum i + 2 | i <- tail maxLengthPerColumn]
                                            --     headerForPrettyPrint = [fst i ++ replicate (snd i - length (fst i)) ' ' | i <- zip header spacesPerColumn]

main = do
    -- putStrLn "Testing..."
    -- putStrLn "----------\n"
    
    let sampledf = newMiniFrame

    -- print $ getName   sampledf
    -- print $ getHeader sampledf
    -- print $ getFields sampledf

    -- printName   sampledf
    -- printHeader sampledf
    -- printFields sampledf
    printMF sampledf
