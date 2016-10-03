-- file types.hs
{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where

data Mood = Blah | Woot
instance Show Mood where
  show Blah = "Blah"
  show Woot = "Woot"

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah


data TisAnInteger =
  TisAn Integer deriving Show
instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'

data EitherOr a b = Hello a | Goodbye b deriving Show
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v) (Hello v') = v == v'
  (==) _ _ = False


data Which a =
  ThisOne a | ThatOne a deriving Show
instance Eq a => Eq (Which a) where
  (==) (ThisOne v) (ThisOne v')  = True
  (==) (ThisOne v) (ThatOne v')  = v == v'
  (==) (ThatOne v) (ThatOne v')  = True
  (==) (ThatOne v) (ThisOne v')  = v == v'

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Ord, Show)
data Date =
  Date DayOfWeek Int deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _  = False

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

data Trivial = Trivial'
instance Eq Trivial where
  Trivial' == Trivial' = True


example = 1
ex2 = (* 9) 6
ex3 = head [(0,"doge"),(1,"kitteh")] --Num a => (a, [Char])

z y = y * 10 where x = 5; y = x + 5

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a,b) -> b
functionS (x, y) = y

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x < y) then fstString x else sndString y
                  where x = "Singin"
                        y = "Somewhere"

main :: IO ()
main = do
  print $ 1 + 2 + sqrt (3^2 + 4^2)
  putStrLn "10"
  print $ negate (-1)
  print ((+) 0 blah) where
   blah = negate 1

