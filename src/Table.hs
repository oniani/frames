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
-- 4. Prettify the table (printTable)
--
-- 5. Current abstractions are somewhat ambiguous: type 'Field' refers to both rows and columns...
-- 
-- 6. Relational algebra operations do not handle the case of duplicate columns
--
-- ====================================================================================================

module Table
    (
    -- Types
      ID                        -- Int
    , Name                      -- String
    , Header                    -- [Name]
    , Field                     -- [String]
    , Fields                    -- [Field]

    -- Creation 
    , sampleTable               -- -> Table
    , buildTable                -- Name -> Header -> Fields -> Table
    , fromCSV                   -- String -> IO Table

    -- Retrieval
    , getName                   -- Table -> Name
    , getHeader                 -- Table -> Header
    , getFields                 -- Table -> Fields
    , getFieldByID              -- Table -> ID -> Field
    , getColumnByName           -- Table -> ID -> Field

    -- Dimension
    , rowsNum                   -- Table -> Int
    , columnsNum                -- Table -> Int
    , entriesNum                -- Table -> Int

    -- Modification
    , renameTable               -- Table -> Name -> Table
    , renameColumn              -- Table -> Name -> Name -> Table
    , addField                  -- Table -> Field -> Table
    , addColumn                 -- Table -> Field -> Table
    , insertField               -- Table -> Field -> ID -> Table
    , insertColumn              -- Table -> Name -> Name -> Field -> Table
    , removeFieldByID           -- Table -> ID -> Table
    , removeColumnByName        -- Table -> Name -> Table

    -- Relational algebra
    -- , tableSelect            --
    -- , tableJoin              --
    , tableIntersect            -- Table -> Table -> Table
    , tableUnion                -- Table -> Table -> Table

    -- Pretty-printing
    , printName                 -- Table -> IO ()
    , printHeader               -- Table -> IO ()
    , printFields               -- Table -> IO ()
    , printTable                -- Table -> IO ()
    ) where

import Data.List
import Data.Maybe

type ID     = Int               -- ID    : Int
type Name   = String            -- Name  : String
type Header = [Name]            -- Header: [String]
type Field  = [String]          -- Field : [String]
type Fields = [Field]           -- Fields: [[String]]

data Table = Table
    { name   ::  Name           -- Name of the Table
    , header ::  Header         -- Header columns of the Table
    , fields ::  Fields }       -- Fields of the Table
    deriving (Eq)

-- ----------------------------------------------------------------------------------------------------

-- | A sample table
sampleTable :: Table
sampleTable = Table name header fields
    where
        name   = "Table name"
        header = ["First Column","Second Column","Third Column","Fourth Column"]
        fields = [["Row1-Col1","Row1-Col2","Row1-Col3","Row1-Col4"],["Row2-Col1","Row2-Col2","Row2-Col3","Row2-Col4"],
                  ["Row3-Col1","Row3-Col2","Row3-Col3","Row3-Col4"],["Row4-Col1","Row4-Col2","Row4-Col3","Row4-Col4"],
                  ["Row5-Col1","Row5-Col2","Row5-Col3","Row5-Col4"],["Row6-Col1","Row6-Col2","Row6-Col3","Row6-Col4"],
                  ["Row7-Col1","Row7-Col2","Row7-Col3","Row7-Col4"],["Row8-Col1","Row8-Col2","Row8-Col3","Row8-Col4"]]

-- | Build a table
buildTable :: Name -> Header -> Fields -> Table
buildTable name header fields
    | header == nub header = Table name header fields
    | otherwise            = Table "DUPLICATE COLUMN NAMES!" header fields

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
getName (Table name header fields) = name

-- | Get the header
getHeader :: Table -> Header
getHeader (Table name header fields) = header

-- | Get all the fields
getFields :: Table -> Fields
getFields (Table name header fields) = fields

-- | Get a row by its ID
getFieldByID :: Table -> ID -> Field
getFieldByID (Table name header fields) id
    | id >= 0 && id <= length fields - 1 = fields !! id
    | otherwise                          = []

-- | Get a column by its name
getColumnByName :: Table -> Name -> Field
getColumnByName (Table name header fields) columnName
    | columnName `elem` header = transpose fields !! index
    | otherwise                = []
    where
        index = fromJust (elemIndex columnName header)

-- ----------------------------------------------------------------------------------------------------

-- | Get the number of rows
rowsNum :: Table -> Int
rowsNum (Table name header fields) = length fields

-- | Get the number of columns
columnsNum :: Table -> Int
columnsNum (Table name header fields) = length (transpose fields)

-- | Get the number of entries
entriesNum :: Table -> Int
entriesNum table = rowsNum table * columnsNum table

-- ----------------------------------------------------------------------------------------------------

-- | Rename the table
renameTable :: Table -> Name -> Table
renameTable (Table name header fields) newName = Table newName header fields

-- | Rename a column by its name
renameColumn :: Table -> Name -> Name -> Table
renameColumn table@(Table name header fields) oldColumnName newColumnName
    | oldColumnName `elem` header = Table name newHeader fields
    | otherwise                   = table
    where
        index     = fromJust (elemIndex oldColumnName header)
        newHeader = take index header ++ [newColumnName] ++ drop (index + 1) header

-- | Add a field to the end of the table
addField :: Table -> Field -> Table
addField (Table name header fields) field = Table name header (fields ++ [field])

-- | Add a column to the end of the table
addColumn :: Table -> Field -> Table
addColumn (Table name header fields) field = Table name newHeader newFields
    where
        newHeader = header ++ [head field]
        newFields = transpose (transpose fields ++ [tail field])

-- | Insert a field at the given ID
-- Test this!!
insertField :: Table -> Field -> ID -> Table
insertField (Table name header fields) newField id = Table name header newFields
    where
        splitID   = splitAt (id - 1) fields
        newFields = fst splitID ++ [newField] ++ snd splitID

-- | Insert a field between two table names
-- Test this!!
insertColumn :: Table -> Name -> Name -> Field -> Table
insertColumn (Table name header fields) leftColumnName rightColumnName newField = Table name header newFields
    where
        leftIndex  = fromJust (elemIndex leftColumnName header)
        rightIndex = fromJust (elemIndex rightColumnName header)
        newFields  = take leftIndex (transpose fields) ++ [newField] ++ drop rightIndex (transpose fields)

-- ----------------------------------------------------------------------------------------------------

-- | Remove a field by ID
removeFieldByID :: Table -> ID -> Table 
removeFieldByID table@(Table name header fields) id
    | id <= 0 || id > length fields = table
    | otherwise                     = Table name header (take (id - 1) fields ++ drop id fields)

-- | Remove a column by name
removeColumnByName :: Table -> Name -> Table
removeColumnByName table@(Table name header fields) columnName
    | columnName `elem` header = Table name newHeader newFields
    | otherwise                = table
    where
        newHeader = delete columnName header
        index     = fromJust (elemIndex columnName header)
        newFields = transpose (take index (transpose fields) ++ drop (index + 1) (transpose fields))

-- ----------------------------------------------------------------------------------------------------

-- Select operation from relational algebra
-- What if select multiple columns? Field of Fields? getColumnByName returns Field not Table!
-- select :: Table -> Name -> Table
-- select (Table name header fields) columnName
--     | columnName `elem` header = getColumnByName (Table name header fields) columnName 
--     | otherwise                = Table name header fields

-- Join operation from relational algebra
-- join :: Table -> Table -> Field -> Table
-- join (Table name header fields) (Table otherName otherHeader otherFields) onField
    -- | 

-- Intersect operation from relational algebra
tableIntersect :: Table -> Table -> Table
tableIntersect (Table name header fields) (Table otherName otherHeader otherFields) = Table newName newHeader newFields
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header `intersect` otherHeader
        newFields = transpose (transpose fields `intersect` transpose otherFields)

-- Union operation from relation algebra
tableUnion :: Table -> Table -> Table
-- Test this!!
tableUnion (Table name header fields) (Table otherName otherHeader otherFields) = Table newName newHeader newFields
    where
        newName = name ++ " intersect " ++ otherName
        newHeader = header `union` otherHeader
        newFields = transpose (transpose fields `union` transpose otherFields)

-- ----------------------------------------------------------------------------------------------------

-- | Print the name of the table
printName :: Table -> IO ()
printName (Table name header fields) = print name

-- | Print the header of the table
printHeader :: Table -> IO ()
printHeader (Table name header fields) = print header

-- | Print the fields of the table
printFields :: Table -> IO ()
printFields (Table name header fields) = mapM_ print fields

-- | Print the table
printTable :: Table -> IO ()
printTable (Table name header fields) = do
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
