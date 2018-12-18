{- |
Module      :  MiniFrame.hs
Description :  Module implements the MiniFrame and its core functionalities
Copyright   :  (c) David Oniani, 2018
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable
-}

module MiniFrame
    ( sampleMiniFrame

    -- Retrieval
    , getName
    , getHeader
    , getFields
    , getFieldByIndex
    , getColumnFieldByIndex

    -- Modification
    , addField
    , addColumn
    , removeFieldByIndex
    , removeColumnFieldByIndex

    -- Pretty-printers
    , printName
    , printHeader
    , printFields
    , printMF

    -- Lengths
    , rowsNum
    , columnsNum
    ) where

import Data.List

type Name   = String
type Header = [String]
type Field  = [String]
type Fields = [Field]

data MiniFrame = MiniFrame
    { name   ::  Name        -- Name of the MiniFrame
    , header ::  Header      -- Header columns of the MiniFrame
    , fields ::  Fields }    -- Fields of the MiniFrame
    deriving (Eq)


-- | A sample MiniFrame
sampleMiniFrame :: MiniFrame
sampleMiniFrame = MiniFrame name header fields
    where
        name   = "Table name"
        header = ["First Column","Second Column","Third Column","Fourth Column"]
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

-- | Get the row at the particular index
getFieldByIndex :: MiniFrame -> Integer -> Field
getFieldByIndex (MiniFrame name header fields) index = fields !! fromInteger index

-- | Get the column at the particular index
getColumnFieldByIndex :: MiniFrame -> Integer -> Field
getColumnFieldByIndex (MiniFrame name header fields) index = transpose fields !! fromInteger index

-- | Add the field to the end of the data frame
addField :: MiniFrame -> Field -> MiniFrame
addField (MiniFrame name header fields) field = MiniFrame name header (fields ++ [field])

-- | Remove the field at the particular index
removeFieldByIndex :: MiniFrame -> Integer -> MiniFrame
removeFieldByIndex (MiniFrame name header fields) index = MiniFrame (name header (take (index - 1) fields ++ drop index field))

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
printMF (MiniFrame name header fields) = do putStrLn (" " ++ replicate (length name) '_' ++ "\n|" ++ name ++ "|\n " ++ replicate (length name) '-' ++ "\n")
                                            mapM_ putStr [fst i ++ replicate (snd i - length (fst i) + 3) ' ' | i <- zip header maxNumOfSpaces]
                                            putStrLn ("\n" ++ replicate (maximum (map (length . show) fields)) '=')
                                            mapM_ putStrLn [intercalate " | " i | i <- fieldsForPrettyPrint]
                                            where
                                                headerLengthList = map length header
                                                maxLengthOfFieldsPerColumn = map maximum (transpose [map length i | i <- fields])
                                                maxNumOfSpaces = [if uncurry (>) i then fst i else snd i | i <- zip headerLengthList maxLengthOfFieldsPerColumn]
                                                fieldsForPrettyPrint = transpose [map (\n -> n ++ replicate (snd i - length n) ' ') (fst i) | i <- zip (transpose fields) maxNumOfSpaces]

-- | Get the number of rows
rowsNum :: MiniFrame -> Integer
rowsNum (MiniFrame name header fields) = toInteger (length fields)

-- | Get the number of columns
columnsNum :: MiniFrame -> Integer
columnsNum (MiniFrame name header fields) = toInteger (length (transpose fields))


main = do
    -- putStrLn "Testing..."
    -- putStrLn "----------\n"

    let sampleMF = sampleMiniFrame

    -- print $ getName   sampleMF
    -- print $ getHeader sampleMF
    -- print $ getFields sampleMF

    -- printName   sampleMF
    -- printHeader sampleMF
    -- printFields sampleMF
    -- printMF sampleMF
    printMF (addField sampleMF ["This", "is", "a", "test MiniFrame"])
    printMF (removeFieldByIndex sampleMF 1)
