# MiniFrame

MiniFrame provides a simple and user-friendly interface for working
with datasets in a tabular format. The idea of a miniframe comes from
programming language R's `data.frame` object. MiniFrame is just a very
stripped down version of R's data frames. It provides just enough power
and flexibility to conveniently explore the data. The function names as
well as their functionalities are so simple that even those who are not
familiar with Haskell can easily use this package for their convenience.

For the sake of simplicity, everything in a miniframe is of the type
`String` (the same as `[Char]`). Yet, this does not make interacting
with miniframes very inconvenient, nor does it limit its flexibility.
A separate section in the documentation will be dedicated to this issue.
MiniFrame heavily utilizes Haskell's `List` data type meaning that
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
- [Relational Algebra](#relational-algebra)
- [Leveraging Haskell's built-in goodness](#leveraging-haskells-built-in-goodness)

### Data types

MiniFrame has one fundamental data type, it is called `MiniFrame`.
Its definition is shown below.

```haskell
data MiniFrame = MiniFrame
    { _name   :: !Name     -- Name
    , _header :: !Header   -- Header
    , _rows   :: ![Row] }  -- Rows
    deriving (Eq, Show)
```

Most of the functions operate on this data type. As it can be seen above,
there are auxiliary types, which are defined as follows:

```haskell
type Index  = Int
type Name   = String
type Header = [Name]
type Row    = [String]
type Column = [String]
```

**NOTE: For those who are wondering, I WAS considering using `Text` type
in lieu of `String`, but I ended up preferring the `String` type. The reason
is that MiniFrame package is not designed for general _text processing_ tasks.
It is a package for manipulating datasets in a tabular format. Besides, it is
suitable only for the small to medium sized datasets and `String` does not have
too much of an overhead for this task. Furthermore, using `Text` type would
make one unable to use all the built-in `String` functions that come with
Haskell `Prelude` as well as would add an extra dependency to the package.**

**P.S. I am aware of the `pack/unpack` solution, but I find it too repetitive
and in this case, unnecessary.**

Note that the user does not need to be familiar with these types other
than knowing the fact that types `Header`, `Row`, and `Column` are just
the lists of the type `[String]`. These facts make it super simple
to navigate through and manipulate the dataset as well as to perform
numeric computations.

### Construction

| Function      | Description               | Signature                                 |
| ------------- | ------------------------- | ----------------------------------------- |
| `fromSample`  | construct out of a sample | `MiniFrame`                               |
| `fromNull`    | construct out of nothing  | `MiniFrame`                               |
| `fromRows`    | construct out of rows     | `Name -> Header -> [Row] -> MiniFrame`    |
| `fromColumns` | construct out of columns  | `Name -> Header -> [Column] -> MiniFrame` |
| `fromCSV`     | construct out of CSV file | `String -> IO MiniFrame`                  |

**NOTE: Do not let names `fromSample` and `fromNull` deceive you. The only thing
these two functions do is a construction of a miniframe from a sample name, header,
and rows and from nothing (resulting in an empty miniframe). Just for consistency,
all these functions have a prefix `from`.**

Example usage:

```haskell
import MiniFrame

main :: IO ()
main = do
    -- A sample miniframe
    printMF fromSample

    -- A null miniframe
    printMF fromNull

    -- Constructing a miniframe from rows
    let rows = [ ["Bianca" , "21", "Apple" ]
               , ["Brian"  , "20", "Orange"]
               , ["Bethany", "19", "Banana"]
               ]

    printMF $ fromRows "Favorite fruits" ["Name", "Age", "Favorite Fruit"] rows

    -- Constructing a miniframe from columns
    let columns = [ ["Walter", "John", "Eric"]
                  , ["500"   , "700" , "600" ]
                  , ["18"    , "20"  , "19"  ]
                  ]

    printMF $ fromColumns "Game scores" ["Player", "Score", "Age"] columns

    -- Constructing a miniframe from CSV file
    miniframe <- fromCSV "schools.csv"

    printMF miniframe
```

### Accessing the data

| Function    | Description     | Signature               |
| ----------- | --------------- | ----------------------- |
| `nameOf`    | get the name    | `MiniFrame -> Name`     |
| `headerOf`  | get the header  | `MiniFrame -> Header`   |
| `rowsOf`    | get the rows    | `MiniFrame -> [Row]`    |
| `columnsOf` | get the columns | `MiniFrame -> [Column]` |
| `headOf`    | get the head    | `MiniFrame -> Row`      |
| `tailOf`    | get the tail    | `MiniFrame -> [Row]`    |

Example usage:

```haskell
import MiniFrame

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
| `rowsNum`    | get the number of rows    | `MiniFrame -> Int` |
| `columnsNum` | get the number of columns | `MiniFrame -> Int` |
| `entriesNum` | get the number of cells   | `MiniFrame -> Int` |

Example usage:

```haskell
import MiniFrame

main :: IO ()
main = do
    print $ rowsNum    fromSample  -- Get the number of rows
    print $ columnsNum fromSample  -- Get the number of columns
    print $ entriesNum fromSample  -- Get the number of cells
```

### Modifications

| Function        | Description                   | Signature                                           |
| --------------- | ----------------------------- | --------------------------------------------------- |
| `prependRow`    | add a row to the beginning    | `Row -> MiniFrame -> MiniFrame`                     |
| `prependColumn` | add a column to the beginning | `Name -> Column -> MiniFrame -> MiniFrame`          |
| `appendRow`     | add a row to the end          | `Row -> MiniFrame -> MiniFrame`                     |
| `appendColumn`  | add a column to the end       | `Name -> Column -> MiniFrame -> MiniFrame`          |
| `insertRow`     | add a row by given index      | `Index -> Row -> MiniFrame -> MiniFrame`            |
| `insertColumn`  | add a column by given index   | `Index -> Name -> Column -> MiniFrame -> MiniFrame` |

Example usage:

```haskell
import MiniFrame

main :: IO ()
main = do
    let newRow    = map show [1..4]  -- New row
    let newColumn = map show [1..8]  -- New column

    printMF $ prependRow           newRow    fromSample  -- Prepending a row
    printMF $ prependColumn "Nums" newColumn fromSample  -- Prepending a column

    printMF $ appendRow           newRow    fromSample  -- Appending a row
    printMF $ appendColumn "Nums" newColumn fromSample  -- Appending a column

    printMF $ insertRow    1        newRow    fromSample  -- Inserting a row
    printMF $ insertColumn 3 "Nums" newColumn fromSample  -- Inserting a column

```

### Removal

| Function             | Description             | Signature                         |
| -------------------- | ----------------------- | --------------------------------- |
| `removeRowByIndex`   | remove a row by index   | `Index -> MiniFrame -> MiniFrame` |
| `removeColumnByName` | remove a column by name | `Name -> MiniFrame -> MiniFrame`  |

Example usage:

```haskell
import MiniFrame

main :: IO ()
main = do
    printMF $ removeRowByIndex   2    fromSample  -- Removing a row by index
    printMF $ removeColumnByName "C4" fromSample  -- Removing a column by name
```

### Type conversion and numeric computation

| Function       | Description                                                            | Signature             |
| -------------- | ---------------------------------------------------------------------- | --------------------- |
| `toInt`        | Convert a column of string to a column of fixed-precision integers     | `Column -> [Int]`     |
| `toDecimal`    | Convert a column of stings to a column of fixed-precision decimals     | `Column -> [Float]`   |
| `toBigInt`     | Convert a column of string to a column of arbitrary precision integers | `Column -> [Integer]` |
| `toBigDecimal` | Convert a column of stings to a column of arbitrary decimals           | `Column -> [Double]`  |

**NOTE: Word "arbitrary" here refers to a size that can be handled by the machine.**

Example usage:

```haskell
import MiniFrame

main :: IO ()
main = do
    let miniframe = fromColumns

                    -- Name
                    "MiniFrame"

                    -- Header
                    ["Name","Quantity","Total Spending"]

                    -- Columns
                    [ ["Paul" , "Ryan", "Kim"  ]
                    , ["10"   , "20"  , "30"   ]
                    , ["100.0", "200" , "300.0"]
                    ]

    -- Calculating the total quantity
    print $ sum $ toInt $ columnByName "Quantity" miniframe

    -- Calculating the average number of dollars spent per person
    print $ sum (toDecimal $ columnByName "Total Spending" miniframe) / 3

    -- Calculating the total quantity using arbitrary precision integers
    print $ sum $ toBigInt $ columnByName "Quantity" miniframe

    -- Calculating the average number of dollars spent per person using arbitrary precision decimals
    print $ sum (toBigDecimal $ columnByName "Total Spending" miniframe) / 3
```

### Pretty-printing

| Function       | Description              | Signature                     |
| -------------- | ------------------------ | ----------------------------- |
| `printName`    | print the name           | `MiniFrame -> IO ()`          |
| `printHeader`  | print the header         | `MiniFrame -> IO ()`          |
| `printRow`     | print the row by index   | `Index -> MiniFrame -> IO ()` |
| `printRows`    | print the rows           | `MiniFrame -> IO ()`          |
| `printColumn`  | print the column by name | `Name -> MiniFrame -> IO ()`  |
| `printColumns` | print the columns        | `MiniFrame -> IO ()`          |
| `printMF`      | print the miniframe      | `MiniFrame -> IO ()`          |

Example usage:

```haskell
import MiniFrame

main :: IO
main = do
    printName        fromSample  -- Pretty-printing the name
    printHeader      fromSample  -- Pretty-printing the header
    printRow 1       fromSample  -- Pretty-printing the row by index
    printRows        fromSample  -- Pretty-printing all the rows
    printColumn "C4" fromSample  -- Pretty-printing the column C4
    printColumns     fromSample  -- Pretty-printing all the columns
    printMF          fromSample  -- Pretty-printing the miniframe
```

### Additional operations

| Function        | Description             | Signature                        |
| --------------- | ----------------------- | -------------------------------- |
| `rowByIndex`    | get the row by index    | `Index -> MiniFrame -> Row`      |
| `columnByName`  | get the column by name  | `Name -> MiniFrame -> Column`    |
| `columnByIndex` | get the column by index | `Index -> MiniFrame -> Column`   |
| `renameMf`      | rename a miniframe      | `Name -> MiniFrame -> MiniFrame` |

Example usage:

```haskell
import MiniFrame

main :: IO ()
main = do
    print $ rowByIndex    5          fromSample  -- Get the row by index
    print $ columnByname  "C3"       fromSample  -- Get the column by name
    print $ columnByIndex 1          fromSample  -- Get the column by index
    print $ renameMf      "New Name" fromSample  -- Rename the miniframe
```

### Relational algebra

| Function    | Description                    | Signature                                |
| ----------- | ------------------------------ | ---------------------------------------- |
| `union`     | union of two miniframes        | `MiniFrame -> MiniFrame -> MiniFrame`    |
| `diff`      | difference of two miniframes   | `MiniFrame -> MiniFrame -> MiniFrame`    |
| `intersect` | intersection of two miniframes | `MiniFrame -> MiniFrame -> MiniFrame`    |
| `project`   | project a miniframe            | `[Name] -> MiniFrame -> MiniFrame`       |
| `rename`    | rename a column                | `Name -> Name -> MiniFrame -> MiniFrame` |

Example usage:

```haskell
import MiniFrame
import Relational

main :: IO ()
main = do
    print $ fromSample `union`     fromSample  -- Union
    print $ fromSample `diff`      fromSample  -- Difference
    print $ fromSample `intersect` fromSample  -- Intersection

    print $ project ["C2","C4"]         fromSample  -- Projection
    print $ rename  "C1" "First Column" fromSample  -- Rename
```

### Leveraging Haskell's built-in goodness

Recall that miniframe is built on top of Haskell's built-in list
data type which is arguably the most powerful data type in Haskell.
This means that we can use the built-in list manipulation functions
directly.

```haskell
import MiniFrame

main :: IO ()
main = do
    let miniframe = fromRows

                   -- Name
                   "MiniFrame with numeric data"

                   -- Header
                   ["Product","Company","Value"]

                   -- Rows
                   [ ["FP toolkit" , "Haskell Enterprises", "1000000000000000000000.00"]
                   , ["OOP toolkit", "C++ Enterprises"    , "100000000000000000000.00" ]
                   , ["PP toolkit" , "C Enterprises"      , "10000000000000000000.00"  ]
                   , ["LP toolkit" , "Prolog Enterprises" , "1000000000000000000.00"   ]
                   ]

    -- Print out the average of all prices (notice the built-in sum function)
    print $ sum $ toBigDecimal $ columnByName "Value" miniframe
```

## License

[GNU General Public License v3.0](LICENSE)
