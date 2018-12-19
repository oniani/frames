module Main where

import Table


main = do
    -- putStrLn "Testing..."
    -- putStrLn "----------\n"

    -- let sampleMF = sampleTable
    -- printTable (renameTable sampleMF "Random")

    -- print $ getName   sampleMF
    -- print $ getHeader sampleMF
    -- print $ getFields sampleMF

    -- printName   sampleMF
    -- printHeader sampleMF
    -- printFields sampleMF
    -- printTable sampleMF
    -- printTable (addField sampleMF ["This", "is", "a", "test table"])
    -- printTable (removeFieldByID sampleMF 0)
    -- printTable (removeFieldByID sampleMF 1)
    -- printTable (addColumn sampleMF ["This", "is", "a", "test table"])
    table <- fromCSV "test.csv"
    printTable table
    -- putStrLn "\n"
    -- printTable (removeColumnByName table "Mumble1")
    -- putStrLn "\n"
    let newTable = buildTable "A Table" ["Mumble12", "Mumble2", "Mumble5"] [["14","2","31"],["54","5","12"],["19","8","63"]]
    printTable newTable
    printTable (tableIntersect table newTable)
