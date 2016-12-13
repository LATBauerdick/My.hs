{-# LANGUAGE  GADTs
            , TypeOperators
            , DataKinds
            , PolyKinds
            , StandaloneDeriving
            #-}

--{-# LANGUAGE ExistentialQuantification #-}

module HList where

data HList tys where
  Nil :: HList '[]
  (:>) :: h -> HList t -> HList (h ': t)
infixr 5 :>

-- get :: Int -> HList tys -> _
-- get 0 (x :> xs) = x

data Elem list elt where
  EZ :: Elem (x ': xs) x
  ES :: Elem xs x -> Elem (y ': xs) x

get :: Elem tys ty -> HList tys -> ty
get EZ (x :> _) = x
get (ES e) (_ :> xs) = get e xs

-- HList l
-- l = (True :> () :> [Just 'x'] :> "hi" :> Nil)
-- get (ES (ES (ES EZ)))  (True :> () :> [Just 'x'] :> "hi" :> Nil)

data Object a where
  Number :: Integral a => a -> Object a
  Character :: Char -> Object Char

--deriving instance Show (Object a)

-- Number expressed as Existential
-- data ObjectE = forall a. Integral a => NumberE a
data ObjectE where
  NumberE :: Integral a => a -> ObjectE

f :: a -> Object a -> a
f a x = case x of
          Character _ -> a
          Number n -> a + n

main = do
  print $ get (ES (ES (ES EZ)))  (True :> () :> [Just 'x'] :> "hi" :> Nil)
  print $ foldl (+) 0 [1..1000000 :: Int]
  print $ foldl f 0 [ Number x | x <- [1..1000000 :: Int] ]
  print $ foldl (\ a (NumberE b) -> a + fromIntegral b) 0 [NumberE x | x <- [1..1000000 :: Int] ]
