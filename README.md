# Miniframe

Miniframe provides a simple and user-friendly interface for working
with datasets in a tabular format. The idea of a miniframe comes from
R's Data Frame object. Miniframe is just a very stripped down version
of R's data frames. It provides just enough power and flexibility to
conveniently explore the data. The function names as well as their
functionalities are so simple that even those who are not familiar
with Haskell can easily use this package for their convenience.

For the sake of simplicity, everything in a miniframe has to be of the
type `String` (the same of `[Char]`). Yet, this does not make interacting
with miniframes very inconvenient, nor does it limit its flexibility.
A separate section in the documentation will be dedicated to this issue.
Miniframe heavily utilizes Haskell's `List` data type meaning that
everything within miniframes including the fundamental data types can
be manipulated directly using built-in list functions such as `map`,
`concatMap`, `foldl`, `foldr`, `scanl`, `scanr`, and so forth.

## Data types

Miniframe has one fundamental data type, it is called `Miniframe`.
Its definition is shown below.

```haskell
data Miniframe = Miniframe
    { _name   :: {-# UNPACK #-} !Name     -- Name
    , _header :: {-# UNPACK #-} !Header   -- Header
    , _rows   :: {-# UNPACK #-} ![Row] }  -- Rows
    deriving (Eq, Show)
```

Most of the functions operate on this data type. As it can be seen above,
there are auxiliary types, which are defined as follows:

```haskell
type ID     = Int
type Name   = String
type Header = [String]
type Row    = [String]
type Column = [String]
```

Note that the user does not need to be familiar with these types other
than knowing the fact that types `Header`, `Row`, and `Column` are just
the lists of the type `[String]`. These facts make it super simple
to navigate through and manipulate the dataset as well as to perform
numeric computations.

## Documentation

- [Construction](#construction)
- [Accessing the data](#accessing-the-data)
- [Counting the dimensions](#counting-the-dimensions)
- [Modifications](#modifications)
- [Removal](#removal)
- [Pretty-printing](#pretty-printing)
- [Leveraging Haskell's built-in goodness](#leveraging-haskell's-built-in-goodness)

### Construction

| Function      | Description               | Signature                                 |
| ------------- | ------------------------- | ----------------------------------------- |
| `sample`      | construct out of a sample | `Miniframe`                               |
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
    printMf $ fromColumns "Game scores" ["Player", "Score", "Age"] columns

    -- Constructing a miniframe from CSV file...
    mf <- fromCSV "schools.csv"

    putStrLn "\nMiniframe from the CSV file"
    printMf mf
```

### Accessing the data

| Function       | Description       | Signature                     |
| -------------- | ----------------- | ----------------------------- |
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

### Counting the dimensions

| Function     | Description               | Signature          |
| ------------ | ------------------------- | ------------------ |
| `rowsNum`    | get the number of rows    | `Miniframe -> Int` |
| `columnsNum` | get the number of columns | `Miniframe -> Int` |
| `entriesNum` | get the number of cells   | `Miniframe -> Int` |

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

### Modifications

| Function        | Description                         | Signature                                        |
| --------------- | ----------------------------------- | ------------------------------------------------ |
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

### Removal

| Function             | Description                 | Signature                        |
| -------------------- | --------------------------- | -------------------------------- |
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

### Pretty-printing

| Function       | Description              | Signature                    |
| -------------- | ------------------------ | ---------------------------- |
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

### Leveraging Haskell's built-in goodness

Recall that miniframe is built on top of Haskell's built-in list
data type which is arguably the most powerful data type in Haskell.
The "problem" of everything being of a `String` (same as `[Char]`)
data type is also addressed.

```haskell
import Miniframe

main = do
    let mf = Miniframe

             -- Name
             "Miniframe with numeric data"

             -- Header
             ["Product","Company","Price"]

             -- Rows
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
