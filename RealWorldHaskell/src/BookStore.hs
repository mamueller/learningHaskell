
module BookStore
    ( BookInfo(..)
    , MagazineInfo(..)
    , BillingInfo(..)
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

type ReviewBody = String
data BetterReview = BetterReview BookInfo CustomerID ReviewBody


type BookRecord =(BookInfo, BookReview)

type CardHolder=String
type CardNumber=String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
