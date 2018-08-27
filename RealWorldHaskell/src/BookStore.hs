module BookStore
    ( BookInfo(..)
    , MagazineInfo(..)
    , myInfo
    ) where
data BookInfo = Book Int String [String]
                deriving (Show)
data MagazineInfo = Magazine Int String [String]
                deriving (Show)
type CustomerID = Int
data BookReview = BookReview BookInfo CustomerID String                
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]
