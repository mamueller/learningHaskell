main=do
    line <- getLine
    if null line
        then return ()
        else do 
            putStrLn $ reversedWords line
            main

reversedWords :: String -> String
reversedWords = unwords . map reverse . words
