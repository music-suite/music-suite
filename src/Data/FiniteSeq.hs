
{-# LANGUAGE DataKinds #-}

module Data.FiniteSeq (FiniteSeq, parseList, toList) where

import Data.Proxy
import GHC.TypeNats

-- | A sequence of @x@ elements where @0 <= x <= n@.
newtype FiniteSeq (n :: Nat) a = UnsafeList0To4 {_unsafeGetList0To4 :: [a]}
  deriving (Functor, Foldable, Traversable)

parseList :: forall (n :: Nat) a .
  KnownNat n =>
  [a] -> Maybe (FiniteSeq n a)
parseList xs
  | length xs <= fromIntegral (natVal (Proxy :: Proxy n))
    = Just $ UnsafeList0To4 xs
  | otherwise = Nothing

toList :: FiniteSeq n a -> [a]
toList (UnsafeList0To4 xs) = xs



