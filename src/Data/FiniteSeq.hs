
{-# LANGUAGE DataKinds #-}

module Data.FiniteSeq (FiniteSeq, parseList, toList) where

import Data.Proxy
import GHC.TypeNats

-- | A sequence of at most @n@ elements.
newtype FiniteSeq (n :: Nat) a = UnsafeFiniteSeq {_getUnsafeFiniteSeq :: [a]}
  deriving (Functor, Foldable, Traversable)

parseList :: forall (n :: Nat) a .
  KnownNat n =>
  [a] -> Maybe (FiniteSeq n a)
parseList xs
  | length xs <= fromIntegral (natVal (Proxy :: Proxy n))
    = Just $ UnsafeFiniteSeq xs
  | otherwise = Nothing

fromMaybe :: Maybe a -> FiniteSeq 1 a
fromMaybe Nothing = UnsafeFiniteSeq []
fromMaybe (Just x) = UnsafeFiniteSeq [x]

singleton :: a -> FiniteSeq 1 a
singleton x = UnsafeFiniteSeq [x]

toList :: FiniteSeq n a -> [a]
toList (UnsafeFiniteSeq xs) = xs



