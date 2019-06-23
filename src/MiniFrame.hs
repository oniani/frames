{- |
Module      :  MiniFrame.hs
Description :  Module implements the MiniFrame data type and its core functionalities
Copyright   :  (c) 2018 David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

This is an implementation of a data frame which comes with various common-for-data frames operations
as well as some relational algebra goodness.

You can read more about relational algebra by following the link: https://en.wikipedia.org/wiki/Relational_algebra.
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
    , renameMF                  -- Name -> MiniFrame -> MiniFrame
    , addRow                    -- Row -> MiniFrame -> MiniFrame
    , addColumn                 -- Name -> Column -> MiniFrame -> MiniFrame
    , insertRow                 -- ID -> Row -> MiniFrame -> MiniFrame
    , insertColumn              -- Name -> Name -> Column -> MiniFrame -> MiniFrame
    , removeRowByID             -- MiniFrame -> ID -> MiniFrame
    , removeColumnByName        -- MiniFrame -> Name -> MiniFrame

    -- Relational algebra
    , union                     -- MiniFrame -> MiniFrame -> MiniFrame
    , (\\)                      -- MiniFrame -> MiniFrame -> MiniFrame
    , intersect                 -- MiniFrame -> MiniFrame -> MiniFrame
    , project                   -- [Name] -> MiniFrame -> MiniFrame
    , select                    -- (Header -> Row -> Bool) -> MiniFrame -> MiniFrame
    , rename                    -- Name -> Name -> MiniFrame -> MiniFrame
    , njoin                     -- MiniFrame -> MiniFrame -> MiniFrame
    , thetaJoin                 -- (Header -> Row -> Bool) -> MiniFrame -> MiniFrame -> MiniFrame
    , cartprod                  -- MiniFrame -> MiniFrame -> MiniFrame

    -- Pretty-printing
    , printName                 -- MiniFrame -> IO ()
    , printHeader               -- MiniFrame -> IO ()
    , printRows                 -- MiniFrame -> IO ()
    , prettyPrint               -- MiniFrame -> IO ()
    ) where

import Data.List.Split
import Data.Either

import qualified Data.List as List
import qualified Data.Maybe as Maybe

import ParseCSV (parseCSV)

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
        rows   = [["Row1-Col1","Row1-Col2","Row1-Col3","Row1-Col4"],["Row2-Col1","Row2-Col2","Row2-Col3","Row2-Col4"],
                  ["Row3-Col1","Row3-Col2","Row3-Col3","Row3-Col4"],["Row4-Col1","Row4-Col2","Row4-Col3","Row4-Col4"],
                  ["Row5-Col1","Row5-Col2","Row5-Col3","Row5-Col4"],["Row6-Col1","Row6-Col2","Row6-Col3","Row6-Col4"],
                  ["Row7-Col1","Row7-Col2","Row7-Col3","Row7-Col4"],["Row8-Col1","Row8-Col2","Row8-Col3","Row8-Col4"]]

-- | Build a table from rows
fromRows :: Name -> Header -> [Row] -> MiniFrame
fromRows = MiniFrame

-- | Build a table from columns
fromColumns :: Name -> Header -> [Column] -> MiniFrame
fromColumns name header columns = MiniFrame name header (List.transpose columns)


-- | Build a MiniFrame from the CSV file
fromCSV :: String -> IO MiniFrame
fromCSV file
  | format /= "csv" = error "Unknown file format!"
  | otherwise       = do contents <- readFile file
                         let csv    = fromRight [] $ parseCSV contents
                         let header = head csv
                         let rows   = tail csv
                         return (MiniFrame "MiniFrame" header rows)
    where
        format = drop (length file - 3) file

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
        index = Maybe.fromJust (List.elemIndex columnName header)

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
renameMF :: Name -> MiniFrame -> MiniFrame
renameMF newName (MiniFrame name header rows) = MiniFrame newName header rows

-- | Add a row to the end of the table
addRow :: Row -> MiniFrame -> MiniFrame
addRow newRow (MiniFrame name header rows) = MiniFrame name header (rows ++ [newRow])

-- | Add a column to the end of the table
addColumn :: Name -> Column -> MiniFrame -> MiniFrame
addColumn newColumnName newColumn (MiniFrame name header rows) = MiniFrame name newHeader newRows
    where
        newHeader = header ++ [newColumnName]
        newRows   = List.transpose (List.transpose rows ++ [newColumn])

-- | Insert a row at the given ID
insertRow :: ID -> Row -> MiniFrame -> MiniFrame
insertRow id newRow (MiniFrame name header rows) = MiniFrame name header newRows
    where
        splitID = splitAt id rows
        newRows = fst splitID ++ [newRow] ++ snd splitID

-- | Insert a column between two column names
-- Fix this!!
insertColumn :: Name -> Name -> Column -> MiniFrame -> MiniFrame
insertColumn leftColumnName rightColumnName newRow (MiniFrame name header rows) = MiniFrame name header newRows
    where
        leftIndex  = Maybe.fromJust (List.elemIndex leftColumnName header)
        rightIndex = Maybe.fromJust (List.elemIndex rightColumnName header)
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
        index     = Maybe.fromJust (List.elemIndex columnName header)
        newRows   = List.transpose (take index (List.transpose rows) ++ drop (index + 1) (List.transpose rows))

-- ----------------------------------------------------------------------------------------------------
--                                   Relational algebra
--
-- The operators include union, difference, intersect, project, select, rename, and join.
-- These implementations could be improved in various ways (more efficient, flexible etc).
-- Operations like natural join, theta join, equijoin, semijoin, antijoin, division, and cartesian
-- product will be implemented in the next few updates. Some of the operations such as left and right
-- outer joins, left and right inner joins, full outer and full inner joins are yet to be implemented.
-- ----------------------------------------------------------------------------------------------------

-- | Union operation from relational algebra
union :: MiniFrame -> MiniFrame -> MiniFrame
union (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " union " ++ otherName
        newHeader = header
        newRows   = rows `List.union` otherRows

-- | Difference operation from relational algebra
(\\) :: MiniFrame -> MiniFrame -> MiniFrame
(\\) (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " difference " ++ otherName
        newHeader = header
        newRows   = rows List.\\ otherRows

-- | Intersect operation from relational algebra
intersect :: MiniFrame -> MiniFrame -> MiniFrame
intersect (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header
        newRows   = rows `List.intersect` otherRows

-- Note that this won't work if the user does not pass the list of columns in the right order.
-- This is due to how `List.isSubsequenceOf` works. Will have to reimplement this operation so that
-- the order does not play any role in determining the output.

-- | Project operation from relational algebra
project :: [Name] -> MiniFrame -> MiniFrame
project columnNames miniframe@(MiniFrame name header rows)
    | columnNames `List.isSubsequenceOf` header = MiniFrame newName newHeader newRows
    | otherwise                                 = error "Header mismatch - put the columns in the right order!"
    where
        newName   = "Projected " ++ name
        newHeader = columnNames
        newRows   = List.transpose (map (columnByName miniframe) columnNames)

-- | Select operation from relational algebra
select :: (Header -> Row -> Bool) -> MiniFrame -> MiniFrame
select function (MiniFrame name header rows) = MiniFrame ("Selected " ++ name) header (filter (function header) rows)

-- | Rename operation from relational algebra
rename :: Name -> Name -> MiniFrame -> MiniFrame
rename oldColumnName newColumnName miniframe@(MiniFrame name header rows)
    | oldColumnName `elem` header = MiniFrame newName newHeader newRows
    | otherwise                   = miniframe
    where
        index     = Maybe.fromJust (List.elemIndex oldColumnName header)
        newName   = name
        newHeader = take index header ++ [newColumnName] ++ drop (index + 1) header
        newRows   = rows


-- The following is a proto version that needs to be tested

-- | Natural join operation from relational algebra
njoin :: MiniFrame -> MiniFrame -> MiniFrame
njoin miniframe@(MiniFrame name header rows) otherMiniframe@(MiniFrame otherName otherHeader otherRows)
    | null commonColumnNames  = error "No common column names"
    | columns                 /= otherColumns = error "No common columns"
    | otherwise               = MiniFrame newName newHeader newRows
    where
        commonColumnNames = header `List.intersect` otherHeader
        columns           = map (columnByName miniframe) commonColumnNames
        otherColumns      = map (columnByName otherMiniframe) commonColumnNames
        ----
        newName           = name ++ " njoin " ++ otherName
        newHeader         = List.nub (header ++ otherHeader)
        newRows           = List.nub (rows ++ otherRows)

-- | Theta join operation from relational algebra
thetaJoin :: (Header -> Row -> Bool) -> MiniFrame -> MiniFrame -> MiniFrame
thetaJoin function miniframe otherMiniframe = select function (njoin miniframe otherMiniframe)

-- | Cartesian product operation from relational algebra
cartprod :: MiniFrame -> MiniFrame -> MiniFrame
cartprod (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows)
    | header List.\\ otherHeader /= header = error "Cannot perform cartesian product on duplicate column names"
    | otherwise                            = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " cartprod " ++ otherName
        newHeader = header ++ otherHeader
        newRows   = [x ++ y | x <- rows, y <- otherRows]

-- ----------------------------------------------------------------------------------------------------
--                                  Printing and Pretty-printing
--
-- Currently, there are four printing tools provided with this package.
--
--         'printName'   : Prints the name of the MiniFrame
--         'printName'   : Prints the header of the MiniFrame
--         'printRows'   : Prints the rows of the MiniFrame
--         'prettyPrint' : Pretty-prints the MiniFrame
--
-- 'project' and 'pretty-print' could be used in conjunction to display chunks of the MiniFrame,
-- however, it would be a lot more convenient to have separate implementations for such printing.
--
-- Another idea is to have two functions 'show' and 'prettyPrint'. The first one would be the string
-- representation of the MiniFrame with the latter one being the IO.
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
prettyPrint :: MiniFrame -> IO ()
prettyPrint (MiniFrame name header rows) = do
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
        maxNumOfSpaces            = zipWith max headerLengthList maxLengthStringsPerColumn
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
