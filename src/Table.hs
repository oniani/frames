{- |
Module      :  Table.hs
Description :  Module implements the Table data type and its core functionalities
Copyright   :  (c) David Oniani, 2018
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

This is an implementation of a data frame which comes with various common-for-data frames operations
as well as some relational algebra goodness.

The project is licensed under MIT so feel free to use/reuse the module or improve its functionalities.
-}

-- ====================================================================================================
-- 
-- TODO:
--
-- 1. Handle all the edge cases (might require a heavy use of Data.Maybe)
--
-- 2. Implement relational algebra operators
-- 
-- 3. Optimize functions
-- 
-- 4. Relational algebra operations do not handle the case of duplicate columns
--
-- ====================================================================================================

module Table
    (
    -- Types
      ID                        -- Int
    , Name                      -- String
    , Header                    -- [Name]
    , Row                       -- [String]
    , Column                    -- [String]

    -- Creation 
    , sampleTable               -- -> Table
    , buildTable                -- Name -> Header -> [Row] -> Table
    , buildFromColumns          -- Name -> Header -> [Column] -> Table
    , fromCSV                   -- String -> IO Table

    -- Retrieval
    , getName                   -- Table -> Name
    , getHeader                 -- Table -> Header
    , getRows                   -- Table -> [Row]
    , getRowByID                -- Table -> ID -> Row
    , getColumnByName           -- Table -> ID -> Column

    -- Dimension
    , rowsNum                   -- Table -> Int
    , columnsNum                -- Table -> Int
    , entriesNum                -- Table -> Int

    -- Modification
    , renameTable               -- Table -> Name -> Table
    , renameColumn              -- Table -> Name -> Name -> Table
    , addRow                    -- Table -> Row -> Table
    , addColumn                 -- Table -> Column -> Table
    , insertRow                 -- Table -> Row -> ID -> Table
    , insertColumn              -- Table -> Name -> Name -> Column -> Table
    , removeRowByID             -- Table -> ID -> Table
    , removeColumnByName        -- Table -> Name -> Table

    -- Relational algebra
    -- , tableSelect            --
    -- , tableJoin              --
    , tableIntersect            -- Table -> Table -> Table
    , tableUnion                -- Table -> Table -> Table

    -- Pretty-printing
    , printName                 -- Table -> IO ()
    , printHeader               -- Table -> IO ()
    , printRows                 -- Table -> IO ()
    , printTable                -- Table -> IO ()
    ) where

import Data.List
import Data.Maybe

type ID     = Int               -- ID    : Int
type Name   = String            -- Name  : String
type Header = [String]          -- Header: [String]
type Row    = [String]          -- Row   : [String]
type Column = [String]          -- Column: [String]

data Table = Table
    { name   ::  Name           -- Name of the Table
    , header ::  Header         -- Header columns of the Table
    , roww   ::  [Row] }        -- Rows of the Table
    deriving (Eq)

-- ----------------------------------------------------------------------------------------------------

-- | A sample table
sampleTable :: Table
sampleTable = Table name header rows
    where
        name   = "Table name"
        header = ["First Column","Second Column","Third Column","Fourth Column"]
        rows   = [["Row1-Col1","Row1-Col2","Row1-Col3","Row1-Col4"],["Row2-Col1","Row2-Col2","Row2-Col3","Row2-Col4"],
                  ["Row3-Col1","Row3-Col2","Row3-Col3","Row3-Col4"],["Row4-Col1","Row4-Col2","Row4-Col3","Row4-Col4"],
                  ["Row5-Col1","Row5-Col2","Row5-Col3","Row5-Col4"],["Row6-Col1","Row6-Col2","Row6-Col3","Row6-Col4"],
                  ["Row7-Col1","Row7-Col2","Row7-Col3","Row7-Col4"],["Row8-Col1","Row8-Col2","Row8-Col3","Row8-Col4"]]

-- | Build a table
buildTable :: Name -> Header -> [Row] -> Table
buildTable name header rows
    | header == nub header = Table name header rows
    | otherwise            = Table "DUPLICATE COLUMN NAMES!" header rows

-- | Build a table from columns
buildFromColumns :: Name -> Header -> [Column] -> Table
buildFromColumns name header columns
    | header == nub header = Table name header (transpose columns)
    | otherwise            = Table "DUPLICATE COLUMN NAMES!" header (transpose columns)

-- Build the table from the csv file
fromCSV :: String -> IO Table
fromCSV file = do
    contents <- readFile file
    let table = [splitBy ',' i | i <- init (splitBy '\n' contents)]
    return (Table "Table" (head table) (tail table))
    where
        splitBy delimiter = foldr fun [[]]
            where
                fun c l@(x:xs)
                    | c == delimiter = []:l
                    | otherwise = (c:x):xs

-- ----------------------------------------------------------------------------------------------------

-- | Get the table name
getName :: Table -> Name
getName (Table name header rows) = name

-- | Get the header
getHeader :: Table -> Header
getHeader (Table name header rows) = header

-- | Get all the rows
getRows :: Table -> [Row]
getRows (Table name header rows) = rows

-- | Get a row by its ID
getRowByID :: Table -> ID -> Row
getRowByID (Table name header rows) id
    | id >= 0 && id <= length rows - 1 = rows !! id
    | otherwise                          = []

-- | Get a column by its name
getColumnByName :: Table -> Name -> Column
getColumnByName (Table name header rows) columnName
    | columnName `elem` header = transpose rows !! index
    | otherwise                = []
    where
        index = fromJust (elemIndex columnName header)

-- ----------------------------------------------------------------------------------------------------

-- | Get the number of rows
rowsNum :: Table -> Int
rowsNum (Table name header rows) = length rows

-- | Get the number of columns
columnsNum :: Table -> Int
columnsNum (Table name header rows) = length (transpose rows)

-- | Get the number of entries
entriesNum :: Table -> Int
entriesNum table = rowsNum table * columnsNum table

-- ----------------------------------------------------------------------------------------------------

-- | Rename the table
renameTable :: Table -> Name -> Table
renameTable (Table name header rows) newName = Table newName header rows

-- | Rename a column by its name
renameColumn :: Table -> Name -> Name -> Table
renameColumn table@(Table name header rows) oldColumnName newColumnName
    | oldColumnName `elem` header = Table name newHeader rows
    | otherwise                   = table
    where
        index     = fromJust (elemIndex oldColumnName header)
        newHeader = take index header ++ [newColumnName] ++ drop (index + 1) header

-- | Add a row to the end of the table
addRow :: Table -> Row -> Table
addRow (Table name header rows) newRow = Table name header (rows ++ [newRow])

-- | Add a column to the end of the table
addColumn :: Table -> Column -> Table
addColumn (Table name header rows) newColumn = Table name newHeader newRows
    where
        newHeader = header ++ [head newColumn]
        newRows = transpose (transpose rows ++ [tail newColumn])

-- | Insert a row at the given ID
-- Test this!!
insertRow :: Table -> Row -> ID -> Table
insertRow (Table name header rows) newRow id = Table name header newRows
    where
        splitID = splitAt (id - 1) rows
        newRows = fst splitID ++ [newRow] ++ snd splitID

-- | Insert a column between two column names
-- Test this!!
insertColumn :: Table -> Name -> Name -> Column -> Table
insertColumn (Table name header rows) leftColumnName rightColumnName newRow = Table name header newRows
    where
        leftIndex  = fromJust (elemIndex leftColumnName header)
        rightIndex = fromJust (elemIndex rightColumnName header)
        newRows  = take leftIndex (transpose rows) ++ [newRow] ++ drop rightIndex (transpose rows)

-- ----------------------------------------------------------------------------------------------------

-- | Remove a row by ID
removeRowByID :: Table -> ID -> Table 
removeRowByID table@(Table name header rows) id
    | id <= 0 || id > length rows = table
    | otherwise                   = Table name header (take (id - 1) rows ++ drop id rows)

-- | Remove a column by name
removeColumnByName :: Table -> Name -> Table
removeColumnByName table@(Table name header rows) columnName
    | columnName `elem` header = Table name newHeader newRows
    | otherwise                = table
    where
        newHeader = delete columnName header
        index     = fromJust (elemIndex columnName header)
        newRows = transpose (take index (transpose rows) ++ drop (index + 1) (transpose rows))

-- ----------------------------------------------------------------------------------------------------

-- Select operation from relational algebra
-- What if select multiple columns? [Name]? getColumnByName returns Column not Table!
-- select :: Table -> Name -> Table
-- select (Table name header rows) columnName
--     | columnName `elem` header = getColumnByName (Table name header rows) columnName 
--     | otherwise                = Table name header rows

-- Join operation from relational algebra
-- join :: Table -> Table -> Column -> Table
-- join (Table name header rows) (Table otherName otherHeader otherRows) onColumn
    -- | 

-- Intersect operation from relational algebra
tableIntersect :: Table -> Table -> Table
tableIntersect (Table name header rows) (Table otherName otherHeader otherRows) = Table newName newHeader newRows
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header `intersect` otherHeader
        newRows   = transpose (transpose rows `intersect` transpose otherRows)

-- Union operation from relation algebra
tableUnion :: Table -> Table -> Table
-- Test this!!
tableUnion (Table name header rows) (Table otherName otherHeader otherRows) = Table newName newHeader newRows
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header `union` otherHeader
        newRows = transpose (transpose rows `union` transpose otherRows)

-- ----------------------------------------------------------------------------------------------------

-- | Print the name of the table
printName :: Table -> IO ()
printName (Table name header rows) = print name

-- | Print the header of the table
printHeader :: Table -> IO ()
printHeader (Table name header rows) = print header

-- | Print the rows of the table
printRows :: Table -> IO ()
printRows (Table name header rows) = mapM_ print rows

-- | Print the table
printTable :: Table -> IO ()
printTable (Table name header rows) = do
    putStrLn (" " ++ replicate (length name + 2) '_' ++ "\n| " ++ name ++ " |\n " ++ replicate (length name + 2) '-' ++ "\n")
    mapM_ putStr ([fst i ++ "-+" ++ replicate (snd i - length (fst i) + 1) '-' | i <- init (zip dashesUnderHeader maxNumOfSpaces)] ++ [last dashesUnderHeader] ++ ["-+"])
    putStr "\n"
    mapM_ putStr ([fst i ++ " |" ++ replicate (snd i - length (fst i) + 1) ' ' | i <- init (zip newHeader maxNumOfSpaces)] ++ [last newHeader] ++ [" |"])
    putStr "\n"
    mapM_ putStr ([fst i ++ "-+" ++ replicate (snd i - length (fst i) + 1) '-' | i <- init (zip dashesUnderHeader maxNumOfSpaces)] ++ [last dashesUnderHeader] ++ ["-+"])
    putStr "\n"
    mapM_ putStrLn [intercalate " | " i | i <- rowsForPrettyPrint]
    mapM_ putStr ([fst i ++ "-+" ++ replicate (snd i - length (fst i) + 1) '-' | i <- init (zip dashesUnderHeader maxNumOfSpaces)] ++ [last dashesUnderHeader] ++ ["-+"])    
    putStr "\n"
    where
        newHeader                = "| ID":header

        dashesUnderHeaderHelper  = map ((`replicate` '-') . length) newHeader
        dashesUnderHeader        = ("+" ++ tail(head dashesUnderHeaderHelper)) : tail dashesUnderHeaderHelper

        headerLengthList         = map length newHeader

        newRows                  = [("| "++ show (i + 1)) : rows !! i | i <- [0..length rows - 1]]
        maxLengthOfRowsPerColumn = map maximum (transpose [map length i | i <- newRows])
        maxNumOfSpaces           = [if uncurry (>) i then fst i else snd i | i <- zip headerLengthList maxLengthOfRowsPerColumn]

        rowsForPrettyPrintHelper = transpose [map (\n -> n ++ replicate (snd i - length n) ' ') (fst i) | i <- zip (transpose newRows) maxNumOfSpaces]
        transposedRowsForPPH     = transpose rowsForPrettyPrintHelper
        rowsForPrettyPrint       = transpose (init transposedRowsForPPH ++ [map (++" |") (last transposedRowsForPPH)])
