{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Functor.Context
  ( Ctxt (..),
    toCtxt,
    mapCtxt,
    extractCtxt,
    addCtxt,
  )
where

import Control.Applicative
import Control.Comonad
import Control.Lens.Iso
import Control.Lens.Wrapped

-- | A value with a possible predecessor and successor.
--
--   This can be thought of as a limited version of a list zipper,
--   showing only the direct neighbours.
newtype Ctxt a = Ctxt {getCtxt :: (Maybe a, a, Maybe a)}
  deriving (Functor, Eq, Ord, Show)

instance Wrapped (Ctxt a) where

  type Unwrapped (Ctxt a) = (Maybe a, a, Maybe a)

  _Wrapped' = iso getCtxt Ctxt

instance Rewrapped (Ctxt a) (Ctxt b)

toCtxt :: (Maybe a, a, Maybe a) -> Ctxt a
toCtxt = Ctxt

mapCtxt :: (a -> b) -> Ctxt a -> Ctxt b
mapCtxt = fmap

extractCtxt :: Ctxt a -> a
extractCtxt (Ctxt (_, x, _)) = x

-- | Add context to a sequence.
--
-- >>> addCtxt []
-- []
--
-- >>> addCtxt [1..3]
-- [Ctxt {getCtxt = (Nothing,1,Just 2)},Ctxt {getCtxt = (Just 1,2,Just 3)},Ctxt {getCtxt = (Just 2,3,Nothing)}]
addCtxt :: [a] -> [Ctxt a]
addCtxt = fmap Ctxt . withPrevNext
  where
    withPrevNext :: [a] -> [(Maybe a, a, Maybe a)]
    withPrevNext xs = zip3 (pure Nothing ++ fmap Just xs) xs (fmap Just (tail xs) ++ repeat Nothing)
