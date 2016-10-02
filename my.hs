module My where

curry f a b = f (a, b)
uncurry f (a, b) = f a b

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + (nonsense b)

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i,b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + (nonsense b)


data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah


greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness
    then
      putStrLn "eyyy. What's shakin'?"
    else
      putStrLn "pshhh."
  where
    cool :: String -> Bool
    cool v = v == "downright frosty yo"
