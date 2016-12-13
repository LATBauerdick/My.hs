{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Het where

foo :: (forall a. a -> a) -> (Char,Bool)
foo f = (f 'c', f True)

type List a = forall x. (a -> x -> x) -> x -> x
example :: List Int
example = \ cons nil -> cons 1 (cons 2 (cons 3 nil))

data Listt a = End | (:) a (Listt a) deriving Show
is :: Listt Int
is = 2 : ( 6 : End)


data ShowBox = forall s. Show s => SB s

hetList :: [ShowBox]
hetList = [SB (), SB 5, SB True]

instance Show ShowBox where
  show (SB s) = show s

f :: [ShowBox] -> IO ()
f xs = mapM_ print xs

main = do
  f hetList
  print is

