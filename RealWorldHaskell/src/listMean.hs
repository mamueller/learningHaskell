listMean ::Floating a => [a]-> Maybe a
listMean [] = Nothing
listMean xs = Just (sum xs / (fromIntegral (length xs)))
