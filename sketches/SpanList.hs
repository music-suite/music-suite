
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad

-- 
-- SpanList s a
--   = SpanList
--     Int                -- element count
--     Int -> [([Int],s)] -- spanners
--     Int -> a           -- elements
-- 
-- Functor -- trivial
-- Foldable -- trivial
-- Traversable -- trivial
-- 
-- Applicative/Monad? 
-- 



newtype SpanList s a = SL [([Spanners s], a)]
  deriving (Functor, Foldable, Traversable)
newtype Spanners s   = S ([Int],s) -- (relative positions: -1 for previous 1 for next etc, value)

-- Spanners are copied to each relative index be *valid*, i.e. this is valid
--
-- >>> SL [([S ([1],'s')],'a'),([S ([-1],'s')],'a')]  ::  SpanList Char Char
--
-- this is not
--
-- >>> SL [([S ([1],'s')],'a'),([S ([0],'s')],'a')]  ::  SpanList Char Char

-- | Throw away invalid spanners 
pruneSpanners :: SpanList s a -> SpanList s a
pruneSpanners = undefined

-- Applicative/Monad?
-- join should be possible. Main challenge is to offset the spanner indices of the *outer* SpanList
-- (the inner ones can not refer outside the local list and are automatically safe).
-- (The above applies to list/multiplicative Monad/Ap. What about zipList/representable Monad/Ap?)
  
  
