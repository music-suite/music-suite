
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Semigroup
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse, sequenceA), fmapDefault, foldMapDefault)
import Control.Applicative
import Control.Monad
import Control.Monad.Plus

type Trans a       = () -- Monoid
newtype Pred a     = Pred { getPred :: a -> All } -- Monoid
    deriving (Monoid)

-- instance Functor Pred where
    -- fmap f (Pred p) = 
-- type Pred a = ()

data Score a
    = SEmpty
    | SNote a    
    | STrans (Trans a) (Score a)
    | SPred (Score a) (Score a)
    | SComp (Score a) (Score a)
    -- deriving (Functor, Foldable, Traversable)
    -- Note *all* of these are wrong, we need to render before
    -- we traverse. TODO implement traverse in a rendered form.
instance Foldable Score where
    foldMap = foldMapDefault
instance Functor Score where
    fmap = fmapDefault
instance Applicative Score where
    pure = return
    (<*>) = ap
instance Traversable Score where
    sequenceA = s
        where
            s SEmpty        = pure SEmpty
            s (SNote x)     = SNote <$> x
            s (STrans t x)  = STrans t <$> sequenceA x
            s (SPred x y)   = SPred <$> sequenceA x <*> sequenceA y
            s (SComp x y)   = SComp <$> sequenceA x <*> sequenceA y
instance Monad Score where
    return = SNote
    x >>= f = (j . fmap f) x
        where
            j SEmpty        = SEmpty
            j (SNote x)     = x
            j (STrans t x)  = STrans t (join x)
            j (SPred _ x)   = SPred mzero (join x)
            j (SComp x y)   = SComp (join x) (join y)

instance MonadPlus Score where
    mzero = SEmpty
    mplus = SComp

class MonadPlus m => Filterable m where
    filter :: (a -> Bool) -> m a -> m a
    filter = mfilter



{-


    SPred p (SPred q x) = STrans (p `o` q) x
    STrans t (STrans u x) = STrans (t `o` u) x

                                                           
    SPred  p (SComp x y) = SComp (p `pred` x) (p `pred` y)
    STrans t (SComp x y) = SComp (t `t` x) (t `t` y)

    
    > Predicates can get really efficient if we can transform them too, i.e.
    
    STrans t (SPred p x) = SPred (t `t` p) (t `t` x)

-}
