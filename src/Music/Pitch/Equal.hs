
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Equal temperament pitch of any size.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Equal (
    -- Equal,
    -- toEqual,
    -- Equal6,
    -- Equal12,
    -- Equal17,
    -- Equal24,
    -- Equal36,
    -- -- toEqual',
    -- unsafeToEqual,
    -- value,
    -- size,
    -- cast,
)
where

-- {-
-- 
-- TODO *avoid* mixing things up here
-- 
--   This type implements limited values (useful for interval *steps*)
--   An ET-interval is just an int, with a type-level size (divMod is "separate")
-- -}

import Data.Maybe
import Data.Either
import Data.Semigroup
import Control.Monad
import Control.Applicative
import Music.Pitch.Absolute

import TypeUnary.Nat

-- Based on Data.Fixed

  
-- 
-- import Data.TypeLevel.Num hiding ((<), (>), (+), (<=), (*))
-- import qualified Data.TypeLevel.Num as TL
-- 
-- data Foo = Foo Int deriving Show
-- 

newtype Equal a = Equal { getEqual :: Int }

deriving instance {-IsNat a =>-} Eq (Equal a)
deriving instance {-IsNat a =>-} Ord (Equal a)

instance {-IsNat a =>-} Show (Equal a) where
  showsPrec d (Equal x) = showParen (d > app_prec) $
       showString "Equal " . showsPrec (app_prec+1) x
    where app_prec = 10

-- | Convenience to avoid ScopedTypeVariables etc    
getSize :: IsNat a => Equal a -> Nat a
getSize _ = nat 

-- 
-- | Size of this type (value not evaluated).
-- >>> size (undefined :: Equal N2)
-- 2
-- 
-- >>> size (undefined :: Equal N12)
-- 12
-- 
size :: IsNat a => Equal a -> Int
size = natToZ . getSize

-- toEqual :: forall a . IsNat a => Int -> Maybe (Equal a)
-- toEqual n = Just r
--   where
--     r = Equal n
--     s = size (Equal 0 :: Equal a)

-- toEqual' :: IsNat a => Nat a -> Int -> Maybe (Equal a)
-- toEqual' s n = if 0 <= n && n < natToZ s then Just (Equal n) else Nothing
-- 
-- unsafeToEqual :: Int -> Equal a
-- unsafeToEqual n = Equal n
-- 
-- type Equal6  = Equal D6
-- type Equal12 = Equal D12
-- type Equal17 = Equal D17
-- type Equal24 = Equal D24
-- type Equal36 = Equal D36
-- 
-- -- unsafemapValue :: (Int -> Int) -> Equal a -> Equal b
-- -- mapValue
-- 
-- -- b = 24, a = 12, mul = 24/12
-- -- | Safely cast a tempered value to a larger size.
-- --
-- -- >>> cast (unsafeToEqual 1 :: Equal12) :: Equal24
-- -- Equal 2 :: Equal24
-- --
-- cast :: (Pos a, Pos b, Pos c, Div b a c) => Equal a -> Equal b
-- cast = cast' undefined
-- 
-- cast' :: (Pos a, Pos b, Pos c, Div b a c) => Equal c -> Equal a -> Equal b
-- cast' cDummy (Equal a) = Equal (a * size cDummy)
-- 
-- 
-- 
