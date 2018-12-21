module Main where

import MiniFrame


main = do
    putStrLn "Testing...\n----------\n"

    -- -- ----------------------------------------------------------------------------------------------------

    let sampleMF = sampleMiniFrame
    printMiniFrame (renameMiniFrame sampleMF "Random")

    -- -- ----------------------------------------------------------------------------------------------------

    print $ getName sampleMF
    print $ getHeader sampleMF
    print $ getRows sampleMF

    -- ----------------------------------------------------------------------------------------------------

    printName   sampleMF
    printHeader sampleMF
    printRows sampleMF
    printMiniFrame sampleMF
    printMiniFrame (addRow sampleMF ["This", "is", "a", "test table"])
    printMiniFrame (removeRowByID sampleMF 0)
    printMiniFrame (removeRowByID sampleMF 1)
    printMiniFrame (addColumn sampleMF "New Column" ["Thasds", "iasdasds", "asdaa", "test jsdfjsdasdsadle", "Tasdashis", "isasdasd", "aasd", "Asdsad"])

    -- -- ----------------------------------------------------------------------------------------------------

    putStrLn "\n"

    -- -- ----------------------------------------------------------------------------------------------------
    
    table <- fromCSV "test.csv"
    printMiniFrame table

    -- -- ----------------------------------------------------------------------------------------------------
    
    putStrLn "\n"
    
    -- -- ----------------------------------------------------------------------------------------------------
    
    printMiniFrame (removeColumnByName table "Mumble1")
    
    -- -- ----------------------------------------------------------------------------------------------------

    putStrLn "\n"
    
    -- -- ----------------------------------------------------------------------------------------------------
    
    let newMiniFrame = fromRows "A MiniFrame" ["Mumble12", "Mumble2", "Mumble5"] [["14","2","31"],["54","5","12"],["19","8","63"]]
    printMiniFrame newMiniFrame
    printMiniFrame (tableIntersect table newMiniFrame)
    printMiniFrame (insertColumn newMiniFrame "Mumble2" "Mumble5" ["asdasd", "ASdasd", "ASdsad"])
    printMiniFrame (insertRow newMiniFrame ["asdasd", "ASdasd", "ASdsad"] 1)
    printMiniFrame (insertRow newMiniFrame ["asdasd", "ASdasd", "ASdsad"] 4)
    printMiniFrame (addColumn newMiniFrame "ASdas" ["asdasd", "ASdasd", "ASdsad"])

