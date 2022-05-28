fact:: Integer -> Integer
fact 1 = 1
fact n = n* fact (n-1)

tr_fact:: Integer -> Integer
tr_fact n = 
  let (_,r) = go (n,1) 
  in r

go:: (Integer , Integer) ->  (Integer , Integer)
go (1, m)  = (1, m)
go (n, m)  = go (n - 1, m*n)
