{- |
Module      :  MiniFrame.hs
Description :  Module implements the MiniFrame and its core functionalities
Copyright   :  (c) David Oniani, 2018
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

This is a minimal implementation of data frame which supports various operations
as well as some relational algebra goodness. It is licensed under MIT so feel free
to use/reuse the module or improve its functionalities.
-}

-- ====================================================================================================
-- 
-- TODO:
--
-- 1. Handle all the edge cases (heavy use of Data.Maybe [duplicates are especially a pain in ...])
--
-- 2. Implement relational algebra operators
-- 
-- 3. Optimize functions
--
-- ====================================================================================================

module MiniFrame
    (
    -- Types
      ID                          -- Int
    , Name                        -- String
    , Header                      -- [Name]
    , Field                       -- [String]
    , Fields                      -- [Field]

    -- Creation 
    , sampleMiniFrame             -- -> MiniFrame
    , fromCSV                     -- String -> IO MiniFrame

    -- Retrieval
    , getName                   -- MiniFrame -> Name
    , getHeader                 -- MiniFrame -> Header
    , getFields                 -- MiniFrame -> Fields
    , getFieldByID              -- MiniFrame -> ID -> Field
    , getColumnByName           -- MiniFrame -> ID -> Field

    -- Modification
    , rename                    -- MiniFrame -> Name -> MiniFrame
    , renameColumn              -- MiniFrame -> Name -> Name -> MiniFrame
    , addField                  -- MiniFrame -> Field -> MiniFrame
    , addColumn                 -- MiniFrame -> Field -> MiniFrame
    , removeFieldByID           -- MiniFrame -> ID -> MiniFrame
    , removeColumnByName

    -- Relational algebra
    -- , select                    --
    -- , join                      --
    , intersection              --  

    -- Pretty-printers
    , printName                 -- MiniFrame -> IO ()
    , printHeader               -- MiniFrame -> IO ()
    , printFields               -- MiniFrame -> IO ()
    , printMF                   -- MiniFrame -> IO ()

    -- Dimensions
    , rowsNum                   -- MiniFrame -> Int
    , columnsNum                -- MiniFrame -> Int
    ) where

import Data.List
import Data.Maybe

type ID     = Int               -- Row ID
type Name   = String            -- Name
type Header = [Name]            -- Header: 
type Field  = [String]          -- Field : a list of strings
type Fields = [Field]           -- Fields: a list of lists without the header

data MiniFrame = MiniFrame
    { name   ::  Name           -- Name of the MiniFrame
    , header ::  Header         -- Header columns of the MiniFrame
    , fields ::  Fields }       -- Fields of the MiniFrame
    deriving (Eq)

-- ====================================================================================================

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

-- Build a miniframe from the csv file
fromCSV :: String -> IO MiniFrame
fromCSV file = do
    contents <- readFile file
    let miniframe = [splitBy ',' i | i <- init (splitBy '\n' contents)]
    return (MiniFrame "MiniFrame" (head miniframe) (tail miniframe))
    where
        splitBy delimiter = foldr fun [[]]
            where
                fun c l@(x:xs)
                    | c == delimiter = []:l
                    | otherwise = (c:x):xs

-- ====================================================================================================

-- | Get miniframe name
getName :: MiniFrame -> Name
getName (MiniFrame name header fields) = name

-- | Get header
getHeader :: MiniFrame -> Header
getHeader (MiniFrame name header fields) = header

-- | Get all fields
getFields :: MiniFrame -> Fields
getFields (MiniFrame name header fields) = fields

-- | Get row by ID
getFieldByID :: MiniFrame -> ID -> Field
getFieldByID (MiniFrame name header fields) id = fields !! id

-- | Get column by name
getColumnByName :: MiniFrame -> Name -> Field
getColumnByName (MiniFrame name header fields) columnName
    | columnName `elem` header = transpose fields !! index
    | otherwise = []
    where
        index = fromJust (elemIndex columnName header)

-- ====================================================================================================

-- | Rename miniframe
rename :: MiniFrame -> Name -> MiniFrame
rename (MiniFrame name header fields) newName = MiniFrame newName header fields

-- | Rename column by name
renameColumn :: MiniFrame -> Name -> Name -> MiniFrame
renameColumn (MiniFrame name header fields) oldColumnName newColumnName
    | oldColumnName `elem` header = MiniFrame name newHeader fields
    | otherwise                   = MiniFrame name header fields
    where
        index     = fromJust (elemIndex oldColumnName header)
        newHeader = take index header ++ [newColumnName] ++ drop (index + 1) header

-- | Add field to the end of miniframe
addField :: MiniFrame -> Field -> MiniFrame
addField (MiniFrame name header fields) field = MiniFrame name header (fields ++ [field])

-- | Add column to the end of miniframe
addColumn :: MiniFrame -> Field -> MiniFrame
addColumn (MiniFrame name header fields) field = MiniFrame name (header ++ [head field]) (transpose (transpose fields ++ [tail field]))

-- ====================================================================================================

-- | Remove field by id
removeFieldByID :: MiniFrame -> ID -> MiniFrame 
removeFieldByID (MiniFrame name header fields) id
    | id <= 0 || id > length fields = MiniFrame name header fields
    | otherwise                     = MiniFrame name header (take (id - 1) fields ++ drop id fields)

-- | Remove column by name
removeColumnByName :: MiniFrame -> Name -> MiniFrame
removeColumnByName (MiniFrame name header fields) columnName
    | columnName `elem` header = MiniFrame name newHeader newFields
    | otherwise                = MiniFrame name header fields
    where
        newHeader = delete columnName header
        index     = fromJust (elemIndex columnName header)
        newFields = transpose (take index (transpose fields) ++ drop (index + 1) (transpose fields))

-- ====================================================================================================

-- Select operation from relational algebra
-- What if select multiple columns? Field of Fields? getColumnByName returns Field not MiniFrame!
-- select :: MiniFrame -> Name -> MiniFrame
-- select (MiniFrame name header fields) columnName
--     | columnName `elem` header = getColumnByName (MiniFrame name header fields) columnName 
--     | otherwise                = MiniFrame name header fields

-- Join operation from relational algebra
-- join :: MiniFrame -> MiniFrame -> Field -> MiniFrame
-- join (MiniFrame name header fields) (MiniFrame otherName otherHeader otherFields) onField
    -- | 

-- Intersect operation from relational algebra
-- This fails when headers do not match but columns do, vice versa
intersection :: MiniFrame -> MiniFrame -> MiniFrame
intersection (MiniFrame name header fields) (MiniFrame otherName otherHeader otherFields) = MiniFrame newName newHeader newFields
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header `intersect` otherHeader
        newFields = fields `intersect` otherFields

-- ====================================================================================================

-- | Print name of miniframe
printName :: MiniFrame -> IO ()
printName (MiniFrame name header fields) = print name

-- | Print header of miniframe
printHeader :: MiniFrame -> IO ()
printHeader (MiniFrame name header fields) = print header

-- | Print fields of miniframe
printFields :: MiniFrame -> IO ()
printFields (MiniFrame name header fields) = mapM_ print fields

-- | Print miniframe
printMF :: MiniFrame -> IO ()
printMF (MiniFrame name header fields) = do
    putStrLn (" " ++ replicate (length name + 2) '_' ++ "\n| " ++ name ++ " |\n " ++ replicate (length name + 2) '-' ++ "\n")
    mapM_ putStr ([fst i ++ " |" ++ replicate (snd i - length (fst i) + 1) ' ' | i <- init (zip newHeader maxNumOfSpaces)] ++ [last newHeader])
    putStrLn ("\n" ++ replicate (sum (map length newHeader) + 3 * (length newHeader - 1)) '=')
    mapM_ putStrLn [intercalate " | " i | i <- fieldsForPrettyPrint]
    where
        newHeader                  = "ID":header
        newFields                  = [show (i + 1) : fields !! i | i <- [0..length fields - 1]]
        headerLengthList           = map length newHeader
        maxLengthOfFieldsPerColumn = map maximum (transpose [map length i | i <- newFields])
        maxNumOfSpaces             = [if uncurry (>) i then fst i else snd i | i <- zip headerLengthList maxLengthOfFieldsPerColumn]
        fieldsForPrettyPrint       = transpose [map (\n -> n ++ replicate (snd i - length n) ' ') (fst i) | i <- zip (transpose newFields) maxNumOfSpaces]

-- ====================================================================================================

-- | Get number of rows
rowsNum :: MiniFrame -> Int
rowsNum (MiniFrame name header fields) = length fields

-- | Get number of columns
columnsNum :: MiniFrame -> Int
columnsNum (MiniFrame name header fields) = length (transpose fields)
