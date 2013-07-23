


data Time
    VectorSpace
    AffineSpace
    InnerSpace
    AdditiveGroup
data Duration
    VectorSpace
    AffineSpace
    InnerSpace
    AdditiveGroup

-- Terminology:
-- pre-onset (attack) onset (decay) post-onset (sustain) offset (release) post-offset

class HasOnset a where
    onset :: a -> V a
class HasOffset a where
    offset :: a -> V a
class HasDuration a where
    duration :: a -> Diff (V a)
durationDefault x = offset x .-. offset x
onsetDefault    x = offset x .-. duration x
offsetDefault   x = onset x .+^ duration x

class Delayable a where
    delay :: V a -> a -> a
class Stretchable where
    stretch :: Scalar (V a) -> a -> a





newtype Era = (Time, Time) -- or (Time,Duration)
    instance VectorSpace Era where
    instance Semigroup Era where -- smallest era that contains both eras
    instance HasOnset where
    instance HasOffset where
    instance HasDuration where
    instance Delayable where
    instance Stretchable where






newtype TimeT a = (Time, a)
    instance HasOnset where
    instance Delayable where
    -- Lifted Default (time 0)
    -- Semigroup/Monoid (max/min?)

newtype DurationT a = (Duration, a)
    instance HasDuration where
    instance Stretchable where
    -- lifted Default (duration 1)
    -- lifted Semigroup/Monoid (max/min duration?)

newtype Note a  = (Era, a)
    instance HasOnset where
    instance HasOffset where
    instance HasDuration where
    instance Delayable where
    instance Stretchable where





newtype Voice a = [DurationT a]
    instance HasDuration where
    instance Stretchable where
    -- No Monoid!

newtype Track a = [TimeT a]
    instance HasOnset where
    instance Delayable where
    instance Monoid where

newtype Score a = [Note a]
    instance HasOnset where
    instance HasOffset where
    instance HasDuration where
    instance Delayable where
    instance Stretchable where
    instance Monoid where







class HasMeta a where
    type Meta a
    getMeta :: a -> Maybe (Meta a)
instance HasMeta (Maybe a) where
    type Meta (Maybe a) = Void
    getMeta Nothing  = Nothing
    getMeta (Just _) = Nothing
instance HasMeta (Either m a) where
    type Meta (Either m a) = m
    getMeta (Left m) = Just m
    getMeta (Just _) = Nothing
    
    
    
    


