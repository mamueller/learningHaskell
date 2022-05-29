tr_fib:: Integer -> Integer
tr_fib n =
   let (_,(_,l)) = go(n,(0,1))
   in l

go:: (Integer, (Integer,Integer)) -> (Integer, (Integer, Integer))
go (1, (bl,l)) = (1, (bl,l))
go (n, (bl,l)) = go( n - 1, (l,l+bl))
