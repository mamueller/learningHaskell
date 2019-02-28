import Data.List 
av:: [Double]-> Double
av l = realToFrac (sum l) / (genericLength l)

middleEl::[a]->a
middleEl l =l !! (floor ((realToFrac (length [1,2,3])) / 2 ))
