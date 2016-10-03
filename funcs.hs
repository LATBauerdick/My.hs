-- file funcs.hs
-- haskell book mostly chapter 7

module Funcs where

comp :: (b -> c) -> ((a -> b) -> (a -> c))
comp g f x = g (f x) -- f . g $ x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read . show $ a
main2 :: IO ()
main2 = do
  print (roundTrip 4)
  print (id 4)

tensDigit :: Integral a => a -> a
tensDigit x = d where
  xLast = x `div` 10
  d = xLast `mod` 10

hundredsDigit :: Integral r => r -> r
hundredsDigit x = (`mod` 10) . fst . (`divMod` 100) $ x

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main1 :: IO ()
main1 = do
  print (0 :: Int)
  print (add 1 0)
  print (addOne 0)
  print (addOnePF 0)
  print ((addOne . addOne) 0)
  print ((addOnePF . addOne) 0)
  print ((addOne . addOnePF) 0)
  print ((addOnePF . addOnePF) 0)
  print (negate (addOne 0))
  print ((negate . addOne) 0)
  print ((addOne . addOne . addOne . negate . addOne) 0)

f :: Int -> [Int] -> Int
f z xs = foldr (+) z xs


myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (-x)
  | otherwise = x


returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d
returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

mflip'' :: (a -> b -> c) -> b -> a -> c
mflip'' f = \ x y -> f y x

mflip :: (t1 -> t2 -> t) -> t2 -> t1 -> t
mflip f = \x -> \y -> f y x

mflip' :: (t1 -> t2 -> t) -> t2 -> t1 -> t
mflip' f x y = f y x


functionC x y =
  case x > y of
    True ->  x
    False -> y

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True -> putStrLn "eyyyyy. What's shakin'?"
    False -> putStrLn "pshhhh."
  where cool = coolness == "downright frosty yo"

pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs



data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' = case f e e' of
                      GT -> reportBoss e e'
                      EQ -> putStrLn "Neither employee is the boss"
                      LT -> (flip reportBoss) e e'

data WherePenguinsLive =
      Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)
data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

-- is it South Africa? If so, return True

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica Galapagos = False
isSouthAfrica Antarctica = False
isSouthAfrica Australia = False
isSouthAfrica SouthAmerica = False

isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = (galapagosPenguin p)
                       || (antarcticPenguin p)

--------------------------------------------------


newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum
myUser = (Username "callen")
myAcct = (AccountNumber 10456)
-- printUser $ RegisteredUser myUser myAcct

main :: IO ()
main = do
  print $ mflip (++) "First " "Second "
