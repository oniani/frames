# Miniframe

Miniframe package provides a nice user-friendly interface for working with
datasets in a tabular format. Everything in a miniframe has to be of the `String`
(or `[Char]`) type. Yet, this is not a very inconvenient choice as we will see
in the future sections. Miniframe heavily utilizes Haskell's `List` data type
meaning that everything within it can be manipulated directly using Haskell's
built-in list operations such as `map`, `concatMap`, `foldl`, `foldr`, `scanl`,
`scanr`, and so forth.

## Data types

Miniframe has one primary data type, it is called `Miniframe`.
Below is its definition"

```haskell
data Miniframe = Miniframe
    { _name   :: {-# UNPACK #-} !Name     -- Name of the Miniframe
    , _header :: {-# UNPACK #-} !Header   -- Header columns of the Miniframe
    , _rows   :: {-# UNPACK #-} ![Row] }  -- Rows of the Miniframe
    deriving (Eq, Show)
```

Most of the functions operate on this data type. As it can seen from above, there are
auxiliary types, which are defined as follows:

```haskell
type ID     = Int
type Name   = String
type Header = [String]
type Row    = [String]
type Column = [String]
```

Note that because of these definitions, all of Haskell's built-in list manipulation
functions are available to the user!

## Usage

### Constructing a miniframe

| Function      | Description               | Signature                                 |
|---------------|---------------------------|-------------------------------------------|
| `sample`      | construct a sample        | `Miniframe`                               |
| `fromRows`    | construct out of rows     | `Name -> Header -> [Row] -> Miniframe`    |
| `fromColumns` | construct out of columns  | `Name -> Header -> [Column] -> Miniframe` |
| `fromCSV`     | construct out of CSV file | `String -> IO Miniframe`                  |

Example usage:

```haskell
import Miniframe

main :: IO ()
main = do
    -- A sample miniframe
    putStrLn "Sample miniframe\n"
    printMf sample

    -- Constructing a miniframe from rows...
    let rows = [ ["Bianca" , "21", "Apple" ]
               , ["Brian"  , "20", "Orange"]
               , ["Bethany", "19", "Banana"]
               ]

    putStrLn "\nMiniframe from the rows\n"
    printMf $ fromRows "Favorite fruits" ["Name", "Age", "Favorite Fruit"] rows

    -- Constructing a miniframe from columns...
    let columns = [ ["Walter", "John", "Eric"]
                  , ["500"   , "700" , "600" ]
                  , ["18"    , "20"  , "19"  ]
                  ]

    putStrLn "\nMiniframe from the columns\n"
    printMf $ fromColumns "Game scores" ["Player", "Score", "Age"] rows

    -- Constructing a miniframe from CSV file...
    mf <- fromCSV "schools.csv"

    putStrLn "\nMiniframe from the CSV file"
    printMf mf
```


### Getting values out of a miniframe

| Function       | Description       | Signature                     |
|----------------|-------------------|-------------------------------|
| `nameOf`       | get the name      | `Miniframe -> Name`           |
| `headerOf`     | get the header    | `Miniframe -> Header`         |
| `rowsOf`       | get the rows      | `Miniframe -> [Row]`          |
| `columnsOf`    | get the columns   | `Miniframe -> [Column]`       |
| `headOf`       | get the head      | `Miniframe -> Row`            |
| `tailOf`       | get the tail      | `Miniframe -> Row`            |
| `rowByID`      | get the row by id | `ID -> Miniframe -> Row`      |
| `columnByName` | get the row by id | `Name -> Miniframe -> Column` |

Example usage:

```haskell
import Miniframe

main :: IO ()
main = do
    -- Get the name
    putStrLn "Printing out the name...\n"
    putStrLn $ nameOf sample

    -- Get the header
    putStrLn "\nPrinting out the header...\n"
    print $ headerOf sample

    -- Get the rows
    putStrLn "\nPrinting out the rows...\n"
    print $ rowsOf sample

    -- Get the columns
    putStrLn "\nPrinting out the rows...\n"
    print $ columnsOf sample

    -- Get the first row
    putStrLn "\nPrinting out the first row...\n"
    print $ headOf sample

    -- Get the last row
    putStrLn "\nPrinting out the last row...\n"
    print $ tailOf sample

    -- Get the row by ID
    putStrLn "\nPrinting out the row by ID...\n"
    print $ rowByID 5 sample

    -- Get the column by name
    putStrLn "\nPrinting out the column by name...\n"
    print $ columnByname "C3" sample
```

### Getting values out of a miniframe

| Function      | Description               | Signature          |
|---------------|---------------------------|--------------------|
| `rowsNum`     | get the number of rows    | `Miniframe -> Int` |
| `columnsNum`  | get the number of columns | `Miniframe -> Int` |
| `entriesNum`  | get the number of cells   | `Miniframe -> Int` |

Example usage:

```haskell
import Miniframe

main :: IO ()
main = do
    -- Get the number of rows
    putStr "Number of rows: "
    print $ rowsNum sample

    -- Get the number of columns
    putStr "Number of columns: "
    print $ columnsNum sample

    -- Get the number of cells
    putStr "Number of cells: "
    print $ entriesNum sample
```

### Adding to a miniframe

| Function        | Description                         | Signature                                        |
|-----------------|-------------------------------------|--------------------------------------------------|
| `renameMf`      | rename a miniframe                  | `Name -> Miniframe -> Miniframe`                 |
| `prependRow`    | add a row to the beginning          | `Row -> Miniframe -> Miniframe`                  |
| `appendRow`     | add a row to the end                | `Row -> Miniframe -> Miniframe`                  |
| `prependColumn` | add a column to the beginning       | `Name -> Column -> Miniframe -> Miniframe`       |
| `appendColumn`  | add a column to the end             | `Name -> Column -> Miniframe -> Miniframe`       |
| `insertRow`     | add a row by given id               | `ID -> Row -> Miniframe -> Miniframe`            |
| `insertColumn`  | add a column by given column number | `ID -> Name -> Column -> Miniframe -> Miniframe` |

Example usage:

```haskell
import Miniframe

main :: IO ()
main = do
    -- Renaming a miniframe...
    putStrLn "Miniframe with the new name\n"
    printMf $ renameMf "New Name" sample

    -- Prepending a row to a miniframe...
    putStr "\nMiniframe with the new row at the beginning\n"
    printMf $ prependRow ["1","2","3","4"] sample

    -- Appending a row to a miniframe...
    putStr "\nMiniframe with the new row at the end\n"
    printMf $ appendRow ["1","2","3","4"] sample

    -- Inserting a new row to a miniframe...
    putStr "\nMiniframe with the new row at the given ID\n"
    printMf $ insertRow 1  ["1","2","3","4"] sample

    -- Prepending a new column to a miniframe...
    putStr "\nMiniframe with the new column at the beginning\n"
    printMf $ prependColumn "Nums" ["1","2","3","4","5","6","7","8"] sample

    -- Appending a new column to a miniframe...
    putStr "\nMiniframe with the new column at the end\n"
    printMf $ appendColumn "Nums" ["1","2","3","4","5","6","7","8"] sample

    -- Inserting a new column to a miniframe...
    putStr "\nMiniframe with the new column at the given index"
    printMf $ insertColumn 3  "Nums" ["1","2","3","4","5","6","7","8"] sample
```

### Removing from a miniframe

| Function             | Description                 | Signature                        |
|----------------------|-----------------------------|----------------------------------|
| `removeRowByID`      | remove a row by its ID      | `ID -> Miniframe -> Miniframe`   |
| `removeColumnByName` | remove a column by its name | `Name -> Miniframe -> Miniframe` |

Example usage:

```haskell
import Miniframe

main :: IO ()
main = do
    -- Removing a row by its ID
    putStrLn "Removing a row by its ID...\n"
    printMf $ removeRowByID 2 sample

    -- Removing a column by its name
    putStrLn "\nRemoving a column by its name..."
    printMf $ removeColumnByName "C4" sample
```


### Pretty-printing a miniframe

| Function       | Description              | Signature                    |
|----------------|--------------------------|------------------------------|
| `printName`    | print the name           | `Miniframe -> IO ()`         |
| `printHeader`  | print the header         | `Miniframe -> IO ()`         |
| `printRow`     | print the row by id      | `ID -> Miniframe -> IO ()`   |
| `printRows`    | print the rows           | `Miniframe -> IO ()`         |
| `printColumn`  | print the column by name | `Name -> Miniframe -> IO ()` |
| `printColumns` | print the columns        | `Miniframe -> IO ()`         |
| `printMf`      | print the miniframe      | `Miniframe -> IO ()`         |

Example usage:

```haskell
import Miniframe

main = do
    putStrLn "Pretty-printing the name of the miniframe...\n"
    printName sample

    putStrLn "\nPretty-printing the header of the miniframe...\n"
    printHeader sample

    putStrLn "\nPretty-printing the row with id 1 of the miniframe...\n"
    printRow 1 sample

    putStrLn "\nPretty-printing all rows of the miniframe...\n"
    printRows sample

    putStrLn "\nPretty-printing column C4 of the miniframe...\n"
    printColumn "C4" sample

    putStrLn "\nPretty-printing all columns of the miniframe...\n"
    printColumns sample

    putStrLn "\nPretty-printing the miniframe..."
    printMf sample
```

### Using built-in types to work with miniframes

Remember that miniframe is built on top of Haskell's list data type
which is arguably the most powerful data type in Haskell. We will
also solve the problem of everything being a string in a miniframe.

```haskell
import Miniframe

main = do
    let mf = Miniframe

             -- Name
             "Miniframe with numeric data"

             -- Header
             ["Product","Company","Price"]

             [ ["FP toolkit", "Haskell Enterprises", "1000.00"]
             , ["OO toolkit", "C++ Enterprises"    , "100.00" ]
             , ["PP toolkit", "C Enterprises"      , "10.00"  ]
             , ["LC toolkit", "Prolog Enterprises" , "1.00"   ]
             ]

    -- Print out the sum of all prices
    print $ sum $ map (\x -> read x::Double) $ columnByName "Price" mf
```

## License

[GNU General Public License v3.0](LICENSE)
