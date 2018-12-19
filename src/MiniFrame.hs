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
    -- Creation 
      sampleMiniFrame             -- -> MiniFrame
    , fromCSV                     -- String -> IO MiniFrame

    -- Retrieval
    , getName                   -- MiniFrame -> Name
    , getHeader                 -- MiniFrame -> Header
    , getFields                 -- MiniFrame -> Fields
    , getFieldByID              -- MiniFrame -> ID -> Field
    , getColumnByName           -- MiniFrame -> ID -> Field

    -- Modification
    , rename                    -- MiniFrame -> Name -> MiniFrame
    , addField                  -- MiniFrame -> Field -> MiniFrame
    , addColumn                 -- MiniFrame -> Field -> MiniFrame
    , removeFieldByID           -- MiniFrame -> ID -> MiniFrame
    , removeColumnByName

    -- Relational algebra
    -- , select                    --
    -- , join                      --
    -- , intersect                 --  

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
        splitBy delimiter = foldr f [[]]
                            where
                                f c l@(x:xs)
                                    | c == delimiter = []:l
                                    | otherwise = (c:x):xs

-- | Get the name of the data frame
getName :: MiniFrame -> Name
getName (MiniFrame name header fields) = name

-- | Get the header of the data frame
getHeader :: MiniFrame -> Header
getHeader (MiniFrame name header fields) = header

-- | Get the fields of the data frame
getFields :: MiniFrame -> Fields
getFields (MiniFrame name header fields) = fields

-- | Get the row at the particular ID
getFieldByID :: MiniFrame -> ID -> Field
getFieldByID (MiniFrame name header fields) id = fields !! id

-- | Get the column at the particular index
getColumnByName :: MiniFrame -> ID -> Field
getColumnByName (MiniFrame name header fields) columnName = transpose fields !! id
    where
        index = fromJust (elemIndex header columnName)


-- | Rename the data frame
rename :: MiniFrame -> Name -> MiniFrame
rename (MiniFrame name header fields) newName = MiniFrame newName header fields

-- | Add the field to the end of the data frame
addField :: MiniFrame -> Field -> MiniFrame
addField (MiniFrame name header fields) field = MiniFrame name header (fields ++ [field])

-- | Add the column to the end of the data frame
-- addColumn MiniFrame ["HEADER NAME", <..DATA..>] ==> 
-- ==> MiniFrame ( ( name = same header = [... "HEADER NAME"] fields = [[... <..DATA..>],..,[... <..DATA..>]] ) )
addColumn :: MiniFrame -> Field -> MiniFrame
addColumn (MiniFrame name header fields) field = MiniFrame name (header ++ [head field]) (transpose (transpose fields ++ [tail field]))

-- | Remove the field by its id
removeFieldByID :: MiniFrame -> ID -> MiniFrame 
removeFieldByID (MiniFrame name header fields) id
    | id <= 0 || id > length fields = MiniFrame name header fields
    | otherwise                     = MiniFrame name header (take (id - 1) fields ++ drop id fields)

-- | Remove the column by its name
removeColumnByName :: MiniFrame -> Name -> MiniFrame
removeColumnByName (MiniFrame name header fields) columnName
    | columnName `elem` header = MiniFrame name newHeader newFields
    | otherwise                = MiniFrame name header fields
    where
        newHeader = delete columnName header
        index     = fromJust (elemIndex columnName header)
        newFields = transpose (take index (transpose fields) ++ drop (index + 1) (transpose fields))

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

-- | Get the number of rows
rowsNum :: MiniFrame -> Int
rowsNum (MiniFrame name header fields) = length fields

-- | Get the number of columns
columnsNum :: MiniFrame -> Int
columnsNum (MiniFrame name header fields) = length (transpose fields)


main = do
    -- putStrLn "Testing..."
    -- putStrLn "----------\n"

    -- let sampleMF = sampleMiniFrame
    -- printMF (rename sampleMF "Random")

    -- print $ getName   sampleMF
    -- print $ getHeader sampleMF
    -- print $ getFields sampleMF

    -- printName   sampleMF
    -- printHeader sampleMF
    -- printFields sampleMF
    -- printMF sampleMF
    -- printMF (addField sampleMF ["This", "is", "a", "test MiniFrame"])
    -- printMF (removeFieldByID sampleMF 0)
    -- printMF (removeFieldByID sampleMF 1)
    -- printMF (addColumn sampleMF ["This", "is", "a", "test MiniFrame"])
    miniframe <- fromCSV "test.csv"
    printMF miniframe
    putStrLn "\n"
    printMF (removeColumnByName miniframe "Mumble1")
