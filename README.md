# Miniframe

Miniframe provides a simple and user-friendly interface for working
with datasets in a tabular format. The idea of a miniframe comes from
programming language R's `data.frame` object. Miniframe is just a very
stripped down version of R's data frames. It provides just enough power
and flexibility to conveniently explore the data. The function names as
well as their functionalities are so simple that even those who are not
familiar with Haskell can easily use this package for their convenience.

For the sake of simplicity, everything in a miniframe is of the type
`String` (the same as `[Char]`). Yet, this does not make interacting
with miniframes very inconvenient, nor does it limit its flexibility.
A separate section in the documentation will be dedicated to this issue.
Miniframe heavily utilizes Haskell's `List` data type meaning that
everything within miniframes including the fundamental data types can
be manipulated directly using built-in list functions such as `map`,
`concatMap`, `foldl`, `foldr`, `scanl`, `scanr`, and so forth.

## Documentation

- [Data types](#data-types)
- [Construction](#construction)
- [Accessing the data](#accessing-the-data)
- [Counting the dimensions](#counting-the-dimensions)
- [Modifications](#modifications)
- [Removal](#removal)
- [Type conversion and numeric computation](#type-conversion-and-numeric-computation)
- [Pretty-printing](#pretty-printing)
- [Additional operations](#additional-operations)
- [Leveraging Haskell's built-in goodness](#leveraging-haskell's-built-in-goodness)

### Data types

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
type Index  = Int
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

### Construction

| Function      | Description               | Signature                                 |
| ------------- | ------------------------- | ----------------------------------------- |
| `fromSample`  | construct out of a sample | `Miniframe`                               |
| `fromNull`    | construct out of nothing  | `Miniframe`                               |
| `fromRows`    | construct out of rows     | `Name -> Header -> [Row] -> Miniframe`    |
| `fromColumns` | construct out of columns  | `Name -> Header -> [Column] -> Miniframe` |
| `fromCSV`     | construct out of CSV file | `String -> IO Miniframe`                  |

NOTE: Do not let names `fromSample` and `fromNull` deceive you. The only thing
these two functions do is construction a miniframe from a sample name, header,
and rows and from nothing (resulting in an empty miniframe). For consistency,
all these functions have a prefix `from`.

Example usage:

```haskell
import Miniframe

main :: IO ()
main = do
    -- A sample miniframe
    printMf fromSample

    -- A null miniframe
    printMf fromNull

    -- Constructing a miniframe from rows
    let rows = [ ["Bianca" , "21", "Apple" ]
               , ["Brian"  , "20", "Orange"]
               , ["Bethany", "19", "Banana"]
               ]

    printMf $ fromRows "Favorite fruits" ["Name", "Age", "Favorite Fruit"] rows

    -- Constructing a miniframe from columns
    let columns = [ ["Walter", "John", "Eric"]
                  , ["500"   , "700" , "600" ]
                  , ["18"    , "20"  , "19"  ]
                  ]

    printMf $ fromColumns "Game scores" ["Player", "Score", "Age"] columns

    -- Constructing a miniframe from CSV file
    mf <- fromCSV "schools.csv"

    printMf mf
```

### Accessing the data

| Function    | Description     | Signature               |
| ----------- | --------------- | ----------------------- |
| `nameOf`    | get the name    | `Miniframe -> Name`     |
| `headerOf`  | get the header  | `Miniframe -> Header`   |
| `rowsOf`    | get the rows    | `Miniframe -> [Row]`    |
| `columnsOf` | get the columns | `Miniframe -> [Column]` |
| `headOf`    | get the head    | `Miniframe -> Row`      |
| `tailOf`    | get the tail    | `Miniframe -> [Row]`    |

Example usage:

```haskell
import Miniframe

main :: IO ()
main = do
    putStrLn $ nameOf    fromSample  -- Get the name
    print    $ headerOf  fromSample  -- Get the header
    print    $ rowsOf    fromSample  -- Get the rows
    print    $ columnsOf fromSample  -- Get the columns
    print    $ headOf    fromSample  -- Get the head
    print    $ tailOf    fromSample  -- Get the tail
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
    print $ rowsNum    fromSample  -- Get the number of rows
    print $ columnsNum fromSample  -- Get the number of columns
    print $ entriesNum fromSample  -- Get the number of cells
```

### Modifications

| Function        | Description                   | Signature                                           |
| --------------- | ----------------------------- | --------------------------------------------------- |
| `prependRow`    | add a row to the beginning    | `Row -> Miniframe -> Miniframe`                     |
| `prependColumn` | add a column to the beginning | `Name -> Column -> Miniframe -> Miniframe`          |
| `appendRow`     | add a row to the end          | `Row -> Miniframe -> Miniframe`                     |
| `appendColumn`  | add a column to the end       | `Name -> Column -> Miniframe -> Miniframe`          |
| `insertRow`     | add a row by given id         | `ID -> Row -> Miniframe -> Miniframe`               |
| `insertColumn`  | add a column by given index   | `Index -> Name -> Column -> Miniframe -> Miniframe` |

Example usage:

```haskell
import Miniframe

main :: IO ()
main = do
    let newRow    = map show [1..4]  -- New row
    let newColumn = map show [1..8]  -- New column

    printMf $ prependRow           newRow    fromSample  -- Prepending a row
    printMf $ prependColumn "Nums" newColumn fromSample  -- Prepending a column

    printMf $ appendRow           newRow    fromSample  -- Appending a row
    printMf $ appendColumn "Nums" newColumn fromSample  -- Appending a column

    printMf $ insertRow    1        newRow    fromSample  -- Inserting a row
    printMf $ insertColumn 3 "Nums" newColumn fromSample  -- Inserting a column

```

### Removal

| Function             | Description             | Signature                        |
| -------------------- | ----------------------- | -------------------------------- |
| `removeRowByID`      | remove a row by id      | `ID -> Miniframe -> Miniframe`   |
| `removeColumnByName` | remove a column by name | `Name -> Miniframe -> Miniframe` |

Example usage:

```haskell
import Miniframe

main :: IO ()
main = do
    printMf $ removeRowByID       2   fromSample  -- Removing a row by id
    printMf $ removeColumnByName "C4" fromSample  -- Removing a column by name
```

### Type conversion and numeric computation

| Function    | Description                                        | Signature           |
| ----------- | -------------------------------------------------- | ------------------- |
| `toInt`     | Convert a column of string to a column of integers | `Column -> [Int]`   |
| `toDecimal` | Convert a column of stings to a column of decimals | `Column -> [Float]` |

Example usage:

```haskell
import Miniframe

main :: IO ()
main = do
    let mf = fromColumns

             -- Name
             "Miniframe"

             -- Header
             ["Name","Quantity","Total Spending"]

             -- Columns
             [ ["Paul" , "Ryan", "Kim"  ]
             , ["10"   , "20"  , "30"   ]
             , ["100.0", "200" , "300.0"]
             ]

    -- Calculating the total quantity
    print $ sum $ toInt $ columnByName "Quantity" mf

    -- Calculating the average number of dollars spent per person
    print $ sum (toDecimal $ columnByName "Total Spending" mf) / 3
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
    printName        fromSample  -- Pretty-printing the name
    printHeader      fromSample  -- Pretty-printing the header
    printRow 1       fromSample  -- Pretty-printing the row by id
    printRows        fromSample  -- Pretty-printing all the rows
    printColumn "C4" fromSample  -- Pretty-printing the column C4
    printColumns     fromSample  -- Pretty-printing all the columns
    printMf          fromSample  -- Pretty-printing the miniframe
```

### Additional operations

| Function        | Description             | Signature                        |
| --------------- | ----------------------- | -------------------------------- |
| `rowByID`       | get the row by id       | `ID -> Miniframe -> Row`         |
| `columnByName`  | get the column by name  | `Name -> Miniframe -> Column`    |
| `columnByIndex` | get the column by index | `Index -> Miniframe -> Column`   |
| `renameMf`      | rename a miniframe      | `Name -> Miniframe -> Miniframe` |

Example usage:

```haskell
import Miniframe

main :: IO ()
main = do
    print $ rowByID       5         fromSample  -- Get the row by id
    print $ columnByname  "C3"      fromSample  -- Get the column by name
    print $ columnByIndex 1         fromSample  -- Get the column by index
    print $ renameMf "    New Name" fromSample  -- Rename the miniframe
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
             [ ["FP toolkit" , "Haskell Enterprises", "1000.00"]
             , ["OOP toolkit", "C++ Enterprises"    , "100.00" ]
             , ["PP toolkit" , "C Enterprises"      , "10.00"  ]
             , ["LP toolkit" , "Prolog Enterprises" , "1.00"   ]
             ]

    -- Print out the sum of all prices
    print $ sum $ map (\x -> read x::Double) $ columnByName "Price" mf

    -- Note that for this operation we could use the provided toDecimal
    -- function, but for arbitrary precision decimals, type Double should
    -- be used
```

## License

[GNU General Public License v3.0](LICENSE)
