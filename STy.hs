{-# LANGUAGE  GADTs
            , StandaloneDeriving
            , ScopedTypeVariables
            #-}

module STy where

{-data Maybe a = Nothing | Just a -}

newtype Wrap a = Wrap a deriving Show

-- | A type-indexed representation of a type
{-data STy ty = (ty ~ Int) => SInt
            | (ty ~ Bool) => SBool
            | forall a. (ty ~ Maybe a) => SMaybe (STy a)
            | forall a. (ty ~ Wrap a) => SWrap (STy a)
            | forall a. (ty ~ [a]) => SList (STy [a])
            | (ty ~ ()) => SUnit
            | forall a b. (ty ~ (a -> b)) => SArrow (STy a) (STy b)
-}
data STy ty where
  SInt :: STy Int
  SBool :: STy Bool
  SMaybe :: STy a -> STy (Maybe a)
  SWrap :: STy a -> STy (Wrap a)
  SList :: STy a -> STy [a]
  SUnit :: STy ()
  SArrow :: STy a -> STy b -> STy (a -> b)

deriving instance Show (STy ty)

zero :: STy ty -> ty
zero SInt = 0
zero SBool = False
zero (SMaybe _) = Nothing
zero (SWrap ty) = Wrap (zero ty)
zero (SList _) = []
zero SUnit = ()
zero (SArrow _ res) = const (zero res)

eqSTy :: STy ty -> STy ty -> Bool
eqSTy SInt SInt = True
eqSTy SBool SBool = True
eqSTy (SMaybe a) (SMaybe b) = eqSTy a b

