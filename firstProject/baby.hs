doubleMe x=x+x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x= (if x >100 then x else x*2)+1
conanO'Brian = "It's a-me, Conan O'Brian!"
boomBangs xs=[if x <10 then "BOOM!" else "BANG!"| x<- xs,odd x]
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st =[ c |c<-st,c `elem` ['A'..'Z']]
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y +z
 

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "sorry"

sayMe :: (Integral a) => a -> String
sayMe 1 ="one"
sayMe 2 ="two" 
sayMe x ="not between 1 and 2"

factorial ::(Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (a1,b1) (a2,b2) = (a1+a2, b1 +b2)

first :: (a,b,c) ->a
first (x,_,_) = x
second:: (a,b,c) ->b
second (_,x,_)= x 
third:: (a,b,c) ->c
third (_,_,x) = x

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy"
head' (x:_) =x 

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:_) = "This list is long. The first element is: " ++ show x 

length' :: (Num b) => [a] -> b
langth' [] = 0
length' (_:xs) = 1 + length' xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital ""  = "an empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]



