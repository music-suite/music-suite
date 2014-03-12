
newtype Dur  -- Semigroup, Monoid (product)
newtype Time -- Semigroup, Monoid (sum)
newtype Span -- Semigroup, Monoid, AdditiveGroup (composition)

spans :: Iso (Time^2) (Time, Dur)
spans = iso (\(t,d) -> (t,t.+^d)) (\(t,u) -> (t,u.-.t))




newtype Note a      = (Span, a)
newtype Delayed a   = (Time, a)
newtype Stretched a = (Dur, a)

newtype Segment a = Duration -> a
-- Defined 0-1
            
newtype Behavior a  = Time -> a
-- Defined throughout, "focused" on 0-1
newinstance Functor Behavior
-- Distributive?
-- XXX potentially slow, improve by memoization/const optimization
instance Representable Behavior where
    type Rep = Time
instance Applicative Behavior
instance Monad Behavior
instance Monoid a => Monoid (Behavior a)

newtype Track a = [(Time, a)]
    -- XXX Start time, laziness
    -- Functor, Monad
newtype Score a = [(Span, a)]
-- XXX Start time, laziness
    -- Functor, Monad


newtype Reactive a = (Time -> (a, Duration^2))
-- XXX Start time, laziness
-- Distributive?
instance Representable Reactive where
    type Rep = Time
instance Applicative Reactive
instance Monad Reactive
instance Monoid a => Monoid (Reactive a)












class Reverse where
class Split where
class HasDuration where
class HasPosition where
class Transformable where
-- Monoid/Semigroup    


-- Has... Pitch Dynamics Articulation Part Chord?? Clef Slide Tremolo Text Harmonic Meta
-- Has+Is ... Midi/MusicXml
-- Is ... Pitch Interval Dynamic



reverse
split
    take
    drop
duration
position
    onset
    offset

transform
    delay
    stretch
scat
pcat

-- a `lead`   b  moves a so that (offset a' == onset b)
-- a `follow` b  moves b so that (offset a  == onset b')
lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b

