data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show) --,Read,Eq)
freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' EmptyTree EmptyTree)
                (Node 'T' EmptyTree EmptyTree)
            )
            (Node 'Y'
                (Node 'S' EmptyTree EmptyTree)
                (Node 'A' EmptyTree EmptyTree)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' EmptyTree EmptyTree)
                (Node 'R' EmptyTree EmptyTree)
            )
            (Node 'A'
                (Node 'A' EmptyTree EmptyTree)
                (Node 'C' EmptyTree EmptyTree)
            )
        )

-- manual change example with hardcoded pahth
-- pattern matchin is very hard to read 
--changeToP :: Tree Char -> Tree Char
--changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L|R deriving (Show)
type Directions = [Direction]

-- now we give a list of direction and change the destination to P
changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r) 
changeToP [] (Node _ l r) = Node 'P' l r
-- example:
-- changeToP [L,L] freeTree

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x


type Breadcrumbs = [Direction] --Type synonym for Direction but reversed order

goLeft :: (Tree a , Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _,bs) = (l,L:bs)

goRight :: (Tree a , Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r,bs) = (r,R:bs)

x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
