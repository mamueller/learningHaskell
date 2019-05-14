data Animal = Ape Float| Cow Float deriving Show

f:: Animal->Float
f (Ape  x) =  x*x
