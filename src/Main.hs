module Main where

import MiniFrame


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
    -- putStrLn "\n"
    -- printMF (removeColumnByName miniframe "Mumble1")
    -- putStrLn "\n"
    let newOne = renameColumn miniframe "Mumble1" "Mumble4"
    printMF newOne
    printMF (intersection newOne miniframe)
