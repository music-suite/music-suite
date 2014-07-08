
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Functor.Context (
        Ctxt(..),
        mapCtxt,
        extractCtxt,
  ) where

import Control.Comonad
import Control.Applicative
import Control.Lens.Wrapped
import Control.Lens.Iso

-- TODO use newtype and derivice Functor, Comonad etc
newtype Ctxt a = Ctxt { getCtxt :: (Maybe a, a, Maybe a) }
  deriving (Functor)

instance Wrapped (Ctxt a) where
  type Unwrapped (Ctxt a) = (Maybe a, a, Maybe a)
  _Wrapped' = iso getCtxt Ctxt

instance Rewrapped (Ctxt a) (Ctxt b)

instance Applicative Ctxt where
  pure x = Ctxt (Nothing, x, Nothing)
  Ctxt (b,x,a) <*> Ctxt (b',x',a') = Ctxt (b <*> b', x x', a <*> a')
  
-- instance Comonad Ctxt where
  -- extract (Ctxt (b,x,a)) = x

  -- duplicate (Ctxt (Nothing,x,Nothing)) = Ctxt (Nothing, Ctxt (Nothing, x, Nothing), Nothing)
  -- duplicate (Ctxt (Just b,x,Nothing)) = Ctxt (Ctxt Just b, Ctxt (Just b, x, Nothing), Nothing)
  -- duplicate (Ctxt (Nothing,x,Just a)) = Ctxt (Nothing, Ctxt (Nothing, x, Just a), Just a)
  -- duplicate (Ctxt (Just b,x,Just a)) = Ctxt (b, Ctxt (b, x, Just a), Just a)

mapCtxt :: (a -> b) -> Ctxt a -> Ctxt b
mapCtxt = fmap

extractCtxt :: Ctxt a -> a
extractCtxt (Ctxt (_,x,_)) = x

