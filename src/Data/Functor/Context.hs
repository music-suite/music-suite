
module Data.Functor.Context (
        Ctxt(..),
        mapCtxt,
        extractCtxt,
  ) where

-- TODO use newtype and derivice Functor, Comonad etc
type Ctxt a = (Maybe a, a, Maybe a)


mapCtxt :: (a -> b) -> Ctxt a -> Ctxt b
mapCtxt f (a,b,c) = (fmap f a, f b, fmap f c)

extractCtxt :: Ctxt a -> a
extractCtxt (_,x,_) = x

