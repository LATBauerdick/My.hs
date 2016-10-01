-- file: myDrop.hs
module My where
  myDrop :: Int -> [a] -> [a]
  myDrop n xs = if n <= 0 || null xs
                then xs
                else myDrop (n-1) (tail xs)

  type CustomerID = Int
  type ReviewBody = String
  data BetterView = BetterReview BookInfo CustomerID ReviewBody                
  data BookInfo = Book Int String [String]
                  deriving (Show)
  data MagazineInfo = Magazine Int String [String]
  data BookReview = BookReview BookInfo CustomerID String
  type BookRecord = (BookInfo, BookReview)
  
  myInfo = Book 97812313029813 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]


  isOdd n = mod n 2 == 1

  newOr a b = if a then a else b
  
  sayHello :: String -> IO ()
  sayHello x = putStrLn ("Hello, " ++ x ++ "!")
  