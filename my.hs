module My where
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
