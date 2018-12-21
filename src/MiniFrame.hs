{- |
Module      :  MiniFrame.hs
Description :  Module implements the MiniFrame data type and its core functionalities
Copyright   :  (c) 2018 David Oniani
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
-- 5. Simplify printMiniFrame (current approach is not easy to digest and does not handle edge cases) DONE
-- 
-- ====================================================================================================

module MiniFrame
    ( MiniFrame (..)
    -- Types
    , ID                        -- Int
    , Name                      -- String
    , Header                    -- [Name]
    , Row                       -- [String]
    , Column                    -- [String]

    -- Creation 
    , sampleMiniFrame           -- -> MiniFrame
    , fromRows                  -- Name -> Header -> [Row] -> MiniFrame
    , fromColumns               -- Name -> Header -> [Column] -> MiniFrame
    , fromCSV                   -- String -> IO MiniFrame

    -- Retrieval
    , getName                   -- MiniFrame -> Name
    , getHeader                 -- MiniFrame -> Header
    , getRows                   -- MiniFrame -> [Row]
    , getColumns                -- MiniFrame -> [Column]
    , getRowByID                -- MiniFrame -> ID -> Row
    , getColumnByName           -- MiniFrame -> ID -> Column

    -- Dimension
    , rowsNum                   -- MiniFrame -> Int
    , columnsNum                -- MiniFrame -> Int
    , entriesNum                -- MiniFrame -> Int

    -- Modification
    , renameMiniFrame           -- MiniFrame -> Name -> MiniFrame
    , renameColumn              -- MiniFrame -> Name -> Name -> MiniFrame
    , addRow                    -- MiniFrame -> Row -> MiniFrame
    , addColumn                 -- MiniFrame -> Name -> Column -> MiniFrame
    , insertRow                 -- MiniFrame -> ID -> Row -> MiniFrame
    , insertColumn              -- MiniFrame -> Name -> Name -> Column -> MiniFrame
    , removeRowByID             -- MiniFrame -> ID -> MiniFrame
    , removeColumnByName        -- MiniFrame -> Name -> MiniFrame

    -- Relational algebra
    -- , tableSelect            --
    -- , tableJoin              --
    , tableIntersect            -- MiniFrame -> MiniFrame -> MiniFrame
    , tableUnion                -- MiniFrame -> MiniFrame -> MiniFrame

    -- Pretty-printing
    , printName                 -- MiniFrame -> IO ()
    , printHeader               -- MiniFrame -> IO ()
    , printRows                 -- MiniFrame -> IO ()
    , printMiniFrame            -- MiniFrame -> IO ()
    ) where

import Data.List
import Data.Maybe

type ID     = Int               -- ID    : Int
type Name   = String            -- Name  : String
type Header = [String]          -- Header: [String]
type Row    = [String]          -- Row   : [String]
type Column = [String]          -- Column: [String]

data MiniFrame = MiniFrame
    { name   ::  Name           -- Name of the MiniFrame
    , header ::  Header         -- Header columns of the MiniFrame
    , rows   ::  [Row] }        -- Rows of the MiniFrame
    deriving (Eq)

-- ----------------------------------------------------------------------------------------------------

-- | A sample table
sampleMiniFrame :: MiniFrame
sampleMiniFrame = MiniFrame name header rows
    where
        name   = "MiniFrame"
        header = ["First Column","Second Column","Third Column","Fourth Column"]
        rows   = [["Row1-Col1","Anomaly-Row1-Col2","Row1-Col3","Row1-Col4"],["Row2-Col1","Row2-Col2","Row2-Col3","Row2-Col4"],
                  ["Row3-Col1","Row3-Col2","Row3-Col3","Row3-Col4"],["Row4-Col1","Row4-Col2","Row4-Col3","Row4-Col4"],
                  ["Row5-Col1","Row5-Col2","Row5-Col3","Row5-Col4"],["Row6-Col1","Row6-Col2","Row6-Col3","Row6-Col4"],
                  ["Row7-Col1","Row7-Col2","Row7-Col3","Row7-Col4"],["Row8-Col1","Row8-Col2","Row8-Col3","Row8-Col4"]]

-- | Build a table
fromRows :: Name -> Header -> [Row] -> MiniFrame
fromRows name header rows
    | header == nub header = MiniFrame name header rows
    | otherwise            = MiniFrame "DUPLICATE COLUMN NAMES!" header rows

-- | Build a table from columns
fromColumns :: Name -> Header -> [Column] -> MiniFrame
fromColumns name header columns
    | header == nub header = MiniFrame name header (transpose columns)
    | otherwise            = MiniFrame "DUPLICATE COLUMN NAMES!" header (transpose columns)

-- | Build a table from the CSV file
fromCSV :: String -> IO MiniFrame
fromCSV file = do
    contents <- readFile file
    let table = [splitBy ',' i | i <- init (splitBy '\n' contents)]
    return (MiniFrame "MiniFrame" (head table) (tail table))
    where
        splitBy delimiter = foldr fun [[]]
            where
                fun character list@(x:xs)
                    | character == delimiter = []:list
                    | otherwise              = (character:x):xs

-- ----------------------------------------------------------------------------------------------------

-- | Get the table name
getName :: MiniFrame -> Name
getName (MiniFrame name _ _) = name

-- | Get the header
getHeader :: MiniFrame -> Header
getHeader (MiniFrame _ header _) = header

-- | Get all the rows
getRows :: MiniFrame -> [Row]
getRows (MiniFrame _ _ rows) = rows

-- | Get all the columns
getColumns :: MiniFrame -> [Column]
getColumns (MiniFrame _ _ rows) = transpose rows

-- | Get a row by ID
getRowByID :: MiniFrame -> ID -> Row
getRowByID (MiniFrame _ _ rows) id
    | id >= 0 && id <= length rows - 1 = rows !! id
    | otherwise                        = []

-- | Get a column by name
getColumnByName :: MiniFrame -> Name -> Column
getColumnByName (MiniFrame _ header rows) columnName
    | columnName `elem` header = transpose rows !! index
    | otherwise                = []
    where
        index = fromJust (elemIndex columnName header)

-- ----------------------------------------------------------------------------------------------------

-- | Get the number of rows
rowsNum :: MiniFrame -> Int
rowsNum (MiniFrame _ _ rows) = length rows

-- | Get the number of columns
columnsNum :: MiniFrame -> Int
columnsNum (MiniFrame _ _ rows) = length (transpose rows)

-- | Get the number of entries
entriesNum :: MiniFrame -> Int
entriesNum table = rowsNum table * columnsNum table

-- ----------------------------------------------------------------------------------------------------

-- | Rename the table
renameMiniFrame :: MiniFrame -> Name -> MiniFrame
renameMiniFrame (MiniFrame name header rows) newName = MiniFrame newName header rows

-- | Rename a column by name
renameColumn :: MiniFrame -> Name -> Name -> MiniFrame
renameColumn miniframe@(MiniFrame name header rows) oldColumnName newColumnName
    | oldColumnName `elem` header = MiniFrame name newHeader rows
    | otherwise                   = miniframe
    where
        index     = fromJust (elemIndex oldColumnName header)
        newHeader = take index header ++ [newColumnName] ++ drop (index + 1) header

-- | Add a row to the end of the table
addRow :: MiniFrame -> Row -> MiniFrame
addRow (MiniFrame name header rows) newRow = MiniFrame name header (rows ++ [newRow])

-- | Add a column to the end of the table
addColumn :: MiniFrame -> Name -> Column -> MiniFrame
addColumn (MiniFrame name header rows) newColumnName newColumn = MiniFrame name newHeader newRows
    where
        newHeader = header ++ [newColumnName]
        newRows   = transpose (transpose rows ++ [newColumn])

-- | Insert a row at the given ID
insertRow :: MiniFrame -> ID -> Row -> MiniFrame
insertRow (MiniFrame name header rows) id newRow = MiniFrame name header newRows
    where
        splitID = splitAt id rows
        newRows = fst splitID ++ [newRow] ++ snd splitID

-- | Insert a column between two column names
-- Fix this!!
insertColumn :: MiniFrame -> Name -> Name -> Column -> MiniFrame
insertColumn (MiniFrame name header rows) leftColumnName rightColumnName newRow = MiniFrame name header newRows
    where
        leftIndex  = fromJust (elemIndex leftColumnName header)
        rightIndex = fromJust (elemIndex rightColumnName header)
        newRows    = take leftIndex (transpose rows) ++ [newRow] ++ drop rightIndex (transpose rows)

-- ----------------------------------------------------------------------------------------------------

-- | Remove a row by ID
removeRowByID :: MiniFrame -> ID -> MiniFrame 
removeRowByID miniframe@(MiniFrame name header rows) id
    | id < 0 || id > length rows - 1 = miniframe
    | otherwise                      = MiniFrame name header (take id rows ++ drop (id + 1) rows)

-- | Remove a column by name
removeColumnByName :: MiniFrame -> Name -> MiniFrame
removeColumnByName miniframe@(MiniFrame name header rows) columnName
    | columnName `elem` header = MiniFrame name newHeader newRows
    | otherwise                = miniframe
    where
        newHeader = delete columnName header
        index     = fromJust (elemIndex columnName header)
        newRows   = transpose (take index (transpose rows) ++ drop (index + 1) (transpose rows))

-- ----------------------------------------------------------------------------------------------------

-- Select operation from relational algebra
-- What if select multiple columns? [Name]? getColumnByName returns Column not MiniFrame!
-- select :: MiniFrame -> Name -> MiniFrame
-- select (MiniFrame name header rows) columnName
--     | columnName `elem` header = getColumnByName (MiniFrame name header rows) columnName 
--     | otherwise                = MiniFrame name header rows

-- Join operation from relational algebra
-- join :: MiniFrame -> MiniFrame -> Column -> MiniFrame
-- join (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows) onColumn
    -- | 

-- Intersect operation from relational algebra
tableIntersect :: MiniFrame -> MiniFrame -> MiniFrame
tableIntersect (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows) = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header `intersect` otherHeader
        newRows   = transpose (transpose rows `intersect` transpose otherRows)

-- Union operation from relation algebra
tableUnion :: MiniFrame -> MiniFrame -> MiniFrame
-- Test this!!
tableUnion (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows) = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header `union` otherHeader
        newRows   = transpose (transpose rows `union` transpose otherRows)

-- ----------------------------------------------------------------------------------------------------

-- | Print the name of the table
printName :: MiniFrame -> IO ()
printName (MiniFrame name _ _) = print name

-- | Print the header of the table
printHeader :: MiniFrame -> IO ()
printHeader (MiniFrame _ header _) = print header

-- | Print the rows of the table
printRows :: MiniFrame -> IO ()
printRows (MiniFrame _ _ rows) = mapM_ print rows

-- | Print the table
printMiniFrame :: MiniFrame -> IO ()
printMiniFrame (MiniFrame name header rows) = do
    putStrLn (" " ++ replicate (length name + 2) '_' ++ "\n| " ++ name ++ " |\n " ++ replicate (length name + 2) '-' ++ "\n")
    putStrLn (intercalate "-+-" formattedDashes)
    putStrLn (intercalate " | " formattedHeader)
    putStrLn (intercalate "-+-" formattedDashes)
    mapM_ (putStrLn . intercalate " | ") rowsForPrettyPrint
    putStrLn (intercalate "-+-" formattedDashes)
    where
        -- Header stuff
        newHeader                 = "| ID" : header
        headerLengthList          = map length newHeader
        -- Rows with ID numbers
        rowsWithID                = [("| "++ show i) : rows !! i | i <- [0..length rows - 1]]
        -- Longest strings per column
        maxLengthStringsPerColumn = map (maximum . map length) (transpose rowsWithID)
        -- Comparing maximum string lengths across the header and columns for even spacing
        maxNumOfSpaces            = map (\n -> if uncurry (>) n then fst n else snd n) (zip headerLengthList maxLengthStringsPerColumn)
        -- Dashes without pluses; we add 3 X columnsNum because `intercalate " | "` puts 3 characters, namely ' ', '|', and ' '
        formattedDashesHelper1    = [fst i ++ replicate (snd i - length (fst i)) '-' | i <- zip (map ((`replicate` '-') . length) newHeader) maxNumOfSpaces]
        formattedDashesHelper2    = init formattedDashesHelper1 ++ [last formattedDashesHelper1 ++ "-+"]
        formattedDashes           = ("+" ++ tail (head formattedDashesHelper2)) : tail formattedDashesHelper2
        -- Formatted header
        formattedHeaderHelper     = [fst i ++ replicate (snd i - length (fst i)) ' ' | i <- zip newHeader maxNumOfSpaces]
        formattedHeader           = init formattedHeaderHelper ++ [last formattedHeaderHelper ++ " |"]
        -- Formatted rows
        rowsForPrettyPrintHelper  = transpose [map (\n -> n ++ replicate (snd i - length n) ' ') (fst i) | i <- zip (transpose rowsWithID) maxNumOfSpaces]
        rowsForPrettyPrint        = transpose (init (transpose rowsForPrettyPrintHelper) ++ [map (++" |") (last (transpose rowsForPrettyPrintHelper))])
