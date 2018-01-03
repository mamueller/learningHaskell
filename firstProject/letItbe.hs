
cylinder :: (RealFloat a ) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi *r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea

calcBmis :: (RealFloat a) => [(a,a)]-> [a]
calcBmis tuples = [ bmi |(w,h) <- tuples, let bmi = w / h^2 ] 

returnFatBmis :: (RealFloat a) => [(a,a)] -> [a]
returnFatBmis tuples = [ bmi |(w,h) <- tuples, let bmi = w / h^2, bmi>= 25.0 ] 

