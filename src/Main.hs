module Main where

import Table


main = do
    putStrLn "Testing...\n----------\n"

    -- ----------------------------------------------------------------------------------------------------

    let sampleMF = sampleTable
    printTable (renameTable sampleMF "Random")

    -- ----------------------------------------------------------------------------------------------------

    print $ getName   sampleMF
    print $ getHeader sampleMF
    print $ getRows sampleMF

    -- ----------------------------------------------------------------------------------------------------

    printName   sampleMF
    printHeader sampleMF
    printRows sampleMF
    printTable sampleMF
    printTable (addRow sampleMF ["This", "is", "a", "test table"])
    printTable (removeRowByID sampleMF 0)
    printTable (removeRowByID sampleMF 1)
    printTable (addColumn sampleMF ["This", "is", "a", "test table"])

    -- ----------------------------------------------------------------------------------------------------

    putStrLn "\n"

    -- ----------------------------------------------------------------------------------------------------
    
    table <- fromCSV "test.csv"
    printTable table

    -- ----------------------------------------------------------------------------------------------------
    
    putStrLn "\n"
    
    -- ----------------------------------------------------------------------------------------------------
    
    printTable (removeColumnByName table "Mumble1")
    
    -- ----------------------------------------------------------------------------------------------------

    putStrLn "\n"
    
    -- ----------------------------------------------------------------------------------------------------
    
    let newTable = fromRows "A Table" ["Mumble12", "Mumble2", "Mumble5"] [["14","2","31"],["54","5","12"],["19","8","63"]]
    printTable newTable
    printTable (tableIntersect table newTable)
