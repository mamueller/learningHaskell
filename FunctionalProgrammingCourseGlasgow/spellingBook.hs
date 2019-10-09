sentence :: String -> String
sentence xs = head xs : " is for " ++ xs

speller :: [String]->String
speller [] = []
speller (xl : [] )= sentence xl
speller (xbl :xl : [] )= (sentence xbl) ++", and " ++ (sentence xl)
speller (xbbl : xs )= (sentence xbbl) ++", " ++ speller xs
