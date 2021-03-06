-- file: ch03/BookStore.hs
data Book = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

data StoreSign = Sign String [Int]
                    deriving (Show)

-- The type BookReview also has a value constructor named BookReview.
-- Having both the type constructor the same name as the value constructor
-- is the convention in Haskell.
data BookReview = BookReview Book String String

myInfo = Book 9801 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

storeSign = Sign "Zach's Store" [12, 14]


-- In all the above examples, the BookInfo/Review/Magazine types all
-- have implicit value meanings. What does the String in MagazineInfo
-- represent??
-- We can be more explicit with Type Synonyms:
type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview Book CustomerID ReviewBody
                    deriving (Show)

-- Type Synonyms are purely for making code more readable.


-- Algebraic Data Type's value constructors can take zero or more arguments.
type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                   | CashOnDelivery
                   | Invoice CustomerID
                   deriving (Show)


-- We can pattern match on an algebraic data type using its value constructors.
-- Recall the BookInfo type we defined earlier: we can extract the values from a
-- BookInfo as follows:
bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

-- We can indicate that we don't care what is present in part of a pattern. The
-- notation for this is the underscore character “_”, which we call a wild card
nicerID      (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors

-- ^^ above code is referred to as 'accessor functions'
-- accessor functions are bulky and boilerplate and there is an easier way to do
-- it. Fortunately, the language addresses this particular boilerplate problem:
-- we can define a data type, and accessors for each of its components,
-- simultaneously
-- vv below code is nice and concise
data Customer = Customer {
    customerID      :: CustomerID,
    customerName    :: String,
    customerAddress :: Address
} deriving (Show)

-- same syntax for creating values of Customer type
customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]

-- below is an example of 'Record Syntax'
customer2 = Customer {
              customerID = 271828
            , customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            , customerName = "Jane Q. Citizen"
            }
