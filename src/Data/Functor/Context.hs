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
  deriving (Functor, Eq, Ord)

instance Show a => Show (Ctxt a) where
  show (Ctxt vs) = "toCtxt " ++ show vs

instance Wrapped (Ctxt a) where

  type Unwrapped (Ctxt a) = (Maybe a, a, Maybe a)

  _Wrapped' = iso getCtxt Ctxt

instance Rewrapped (Ctxt a) (Ctxt b)

-- instance Applicative Ctxt where
--   pure x = Ctxt (Nothing, x, Nothing)
--   Ctxt (b,x,a) <*> Ctxt (b',x',a') = Ctxt (b <*> b', x x', a <*> a')

-- instance Comonad Ctxt where
-- extract (Ctxt (b,x,a)) = x

-- duplicate (Ctxt (Nothing,x,Nothing)) = Ctxt (Nothing, Ctxt (Nothing, x, Nothing), Nothing)
-- duplicate (Ctxt (Just b,x,Nothing)) = Ctxt (Ctxt Just b, Ctxt (Just b, x, Nothing), Nothing)
-- duplicate (Ctxt (Nothing,x,Just a)) = Ctxt (Nothing, Ctxt (Nothing, x, Just a), Just a)
-- duplicate (Ctxt (Just b,x,Just a)) = Ctxt (b, Ctxt (b, x, Just a), Just a)

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
-- [Ctxt (Nothing, 1, Just 2), Ctxt (Just 1, 2, Just 3), Ctxt (Just 2, 3, Nothing)]
addCtxt :: [a] -> [Ctxt a]
addCtxt = fmap Ctxt . withPrevNext
  where
    withPrevNext :: [a] -> [(Maybe a, a, Maybe a)]
    withPrevNext xs = zip3 (pure Nothing ++ fmap Just xs) xs (fmap Just (tail xs) ++ repeat Nothing)
