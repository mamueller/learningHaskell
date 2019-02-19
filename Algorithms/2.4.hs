f:: Num a => [a]->[a]
f l = reverse ( f' l [])
    where   f' [] r = r
            f' (x:xs) r =(2*x) : (f' xs r)

main ::IO ()
main = do
    let x= f [1,2,3,4]
        in putStrLn ("x=" ++ (show x))
    putStrLn ("x=" ++ (show (f [1,2,3,4])) )

