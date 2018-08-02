type CardHolder = String
type CardNumber = String
type Address=[String]

data BillingInfo= CreditCard CardNumber CardHolder Address
		| CashOnDelivery
		| Invoice CustumerID
		  deriving (Show)
