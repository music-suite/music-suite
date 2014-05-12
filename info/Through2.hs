
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Lens
import Control.Applicative


{-
TODO when does this satisfy the lens laws?

-}

thr :: Applicative f => Lens' s a -> Lens' (f s) (f a)
thr lens1 = lens (fmap $ view lens1) (flip $ liftA2 $ set lens1)

through :: Applicative f => Lens' s a -> Lens s t a b -> Lens (f s) (f t) (f a) (f b)
through lens1 lens2 = lens (fmap $ view lens1) (flip $ liftA2 $ set lens2)
{-# INLINE through #-}



lensLaws :: (Eq s, Eq a) => Lens' s a -> s -> a -> a -> Bool
lensLaws = lensLaws' (==) (==)

lensLaws' :: (s -> s -> Bool) -> (a -> a -> Bool) -> Lens' s a -> s -> a -> a -> Bool
lensLaws' eqS eqA l a b c =
  (view l (set l b a) `eqA` b)
    &&
  (set l (view l a) a `eqS` a)
    &&
  (set l c (set l b a) `eqS` set l c a)

goodEx1 = lensLaws l a b c
  where
    a = [(31,32)]
    b = [33]
    c = [34]
    l = thr (lens fst (\(_,y) x -> (x,y)))

goodEx2 = lensLaws l a b c
  where
    a = ((), (31,32))
    b = ((), 33)
    c = ((), 34)
    l = thr (lens fst (\(_,y) x -> (x,y)))

-- goodEx3 = lensLaws' (\a b -> a () == b ()) (==) l a b c
--   where
--     a = (const (31,32))
--     b = (const 33)
--     c = (const 34)
--     l = thr (lens fst (\(_,y) x -> (x,y)))
