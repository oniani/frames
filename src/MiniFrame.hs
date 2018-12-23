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
    -- name                     -- MiniFrame -> Name
    -- header                   -- MiniFrame -> Header
    -- rows                     -- MiniFrame -> [Row]
    , columns                   -- MiniFrame -> [Column]
    , rowByID                   -- MiniFrame -> ID -> Row
    , columnByName              -- MiniFrame -> Name -> Column

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
    -- , project                   -- MiniFrame -> MiniFrame -> MiniFrame
    , select                    -- MiniFrame -> MiniFrame -> MiniFrame
    -- , intersect                 -- MiniFrame -> MiniFrame -> MiniFrame
    , intersect                 -- MiniFrame -> MiniFrame -> MiniFrame
    , union                     -- MiniFrame -> MiniFrame -> MiniFrame

    -- Pretty-printing
    , printName                 -- MiniFrame -> IO ()
    , printHeader               -- MiniFrame -> IO ()
    , printRows                 -- MiniFrame -> IO ()
    , printMiniFrame            -- MiniFrame -> IO ()
    ) where

import qualified Data.List as List
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
    deriving (Eq, Show)

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

-- | Build a table from rows
fromRows :: Name -> Header -> [Row] -> MiniFrame
fromRows = MiniFrame

-- | Build a table from columns
fromColumns :: Name -> Header -> [Column] -> MiniFrame
fromColumns name header columns = MiniFrame name header (List.transpose columns)

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

-- | Get all the columns
columns :: MiniFrame -> [Column]
columns (MiniFrame _ _ rows) = List.transpose rows

-- | Get a row by ID
rowByID :: MiniFrame -> ID -> Row
rowByID (MiniFrame _ _ rows) id
    | id >= 0 && id <= length rows - 1 = rows !! id
    | otherwise                        = []

-- | Get a column by name
columnByName :: MiniFrame -> Name -> Column
columnByName (MiniFrame _ header rows) columnName
    | columnName `elem` header = List.transpose rows !! index
    | otherwise                = []
    where
        index = fromJust (List.elemIndex columnName header)

-- ----------------------------------------------------------------------------------------------------

-- | Get the number of rows
rowsNum :: MiniFrame -> Int
rowsNum (MiniFrame _ _ rows) = length rows

-- | Get the number of columns
columnsNum :: MiniFrame -> Int
columnsNum (MiniFrame _ _ rows) = length (List.transpose rows)

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
        index     = fromJust (List.elemIndex oldColumnName header)
        newHeader = take index header ++ [newColumnName] ++ drop (index + 1) header

-- | Add a row to the end of the table
addRow :: MiniFrame -> Row -> MiniFrame
addRow (MiniFrame name header rows) newRow = MiniFrame name header (rows ++ [newRow])

-- | Add a column to the end of the table
addColumn :: MiniFrame -> Name -> Column -> MiniFrame
addColumn (MiniFrame name header rows) newColumnName newColumn = MiniFrame name newHeader newRows
    where
        newHeader = header ++ [newColumnName]
        newRows   = List.transpose (List.transpose rows ++ [newColumn])

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
        leftIndex  = fromJust (List.elemIndex leftColumnName header)
        rightIndex = fromJust (List.elemIndex rightColumnName header)
        newRows    = take leftIndex (List.transpose rows) ++ [newRow] ++ drop rightIndex (List.transpose rows)

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
        newHeader = List.delete columnName header
        index     = fromJust (List.elemIndex columnName header)
        newRows   = List.transpose (take index (List.transpose rows) ++ drop (index + 1) (List.transpose rows))

-- ----------------------------------------------------------------------------------------------------

-- Select operation from relational algebra
select :: (Header -> Row -> Bool) -> MiniFrame -> MiniFrame
select function (MiniFrame name header rows) = MiniFrame ("select " ++ name) header (filter (function header) rows)

-- Project operation from relational algebra
-- project :: [Name] -> MiniFrame -> MiniFrame
-- project columns (MiniFrame name header rows) =

-- Join operation from relational algebra
-- join :: MiniFrame -> MiniFrame -> Column -> MiniFrame
-- join (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows) onColumn
    -- | 

-- Intersect operation from relational algebra
intersect :: MiniFrame -> MiniFrame -> MiniFrame
intersect (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header
        newRows   = rows `List.intersect` otherRows

-- Union operation from relation algebra
union :: MiniFrame -> MiniFrame -> MiniFrame
union (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header
        newRows   = rows `List.union` otherRows

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
    putStrLn (List.intercalate "-+-" formattedDashes)
    putStrLn (List.intercalate " | " formattedHeader)
    putStrLn (List.intercalate "-+-" formattedDashes)
    mapM_ (putStrLn . List.intercalate " | ") rowsForPrettyPrint
    putStrLn (List.intercalate "-+-" formattedDashes)
    where
        -- Header stuff
        newHeader                 = "| ID" : header
        headerLengthList          = map length newHeader
        -- Rows with ID numbers
        rowsWithID                = [("| "++ show i) : rows !! i | i <- [0..length rows - 1]]
        -- Longest strings per column
        maxLengthStringsPerColumn = map (maximum . map length) (List.transpose rowsWithID)
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
        rowsForPrettyPrintHelper  = List.transpose [map (\n -> n ++ replicate (snd i - length n) ' ') (fst i) | i <- zip (List.transpose rowsWithID) maxNumOfSpaces]
        rowsForPrettyPrint        = List.transpose (init (List.transpose rowsForPrettyPrintHelper) ++ [map (++" |") (last (List.transpose rowsForPrettyPrintHelper))])
