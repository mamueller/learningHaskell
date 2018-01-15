data Vector a = Vector a a a deriving (Show)
vplus ::(Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vecMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vecMult` m = Vector (m*i)(m*j)(m*k)

scalarMult :: (Num t) => Vector t -> Vector t ->t 
(Vector i j k ) `scalarMult`(Vector l m n) = i*l + j*m + k*n
