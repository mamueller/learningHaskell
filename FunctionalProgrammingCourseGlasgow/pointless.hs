f :: Num(a) => a ->a->a
f x y = x +3*y

g::Num(a) =>[a]->a
g= foldr f 0

