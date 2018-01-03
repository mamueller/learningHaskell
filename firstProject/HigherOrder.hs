applyTwice :: (a->a) -> a ->a
applyTwice f x = f (f x )


make_fmultOne::(Num a) => a -> a -> a
make_fmultOne y z = ( * y) z

multTwo::(Num a) => a ->a ->a
--multTwo y z = (make_fmultOne y) z
multTwo y = (make_fmultOne y) 

make_fmultTwo::(Num a) => a -> a -> a ->a 
make_fmultTwo x y z = x * multTwo y z

multThree ::(Num a) => a -> a ->a ->a
--multThree x y z = (make_fmultTwo x) y z
multThree x = (make_fmultTwo x) 

