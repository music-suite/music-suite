

type Dur  = Rational
type Time = Point Dur
type Span = (Time, Time)
instance Monoid Span
  mempty = (0,1)
  (<>)   = -- transformative composition
-- TODO note alternative monoids are {empty,hull}

type Transformation = Span

class Transformable a where
  transform :: Transformation -> a -> a
class HasPosition a where
  position  :: a -> Span
  position' :: a -> (Scalar Duration -> Time)
  position x    = (position' 0, position' 1)
  position' x l = alerp a b l where (a,b) = position x

-- If we break this out to a separate class, assure that duration is "unchanged"
-- when transforming say a Voice (duration no position) to a Score (both)
duration :: HasPosition a -> a -> Duration
duration x = position' 1 .-. position' 0

-- Law[TransformMM1]: transform s . transform t = transform (s <> t)
-- Law[TransformMM2]: id                        = transform mempty

-- TOO GENERAL?
-- Law[TransformDistPosition]: transform s . position = position . transform s


-- Then go on as usual:
type Stretched a = (Dur, a)
type Delayed   a = (Time, a)
type Note      a = (Span, a)
instance Transform (Stretched a) where -- trivial
instance Transform (Delayed a) where -- trivial
instance Transform (Note a) where -- trivial
instance HasPosition Span where
  position = id
instance Transformable Span where
  tranform = (<>)

type Voice a = [Stretched a]
-- Free Monoid
instance Transformable (Voice a) where
  transform s = fmap (transform s)

type PlacedVoice a = (Scalar Duration, Voice a)
-- (0,...) for onset alignment (1...) for offset alignment etc
-- Alignment outside [0..1] also possible
-- Therefore we have (PlacedVoice a `Iso Delayed (Voice a))
-- We also have (PlacedVoice a `Iso` Reactive (Maybe a))
-- We have (Prism (Score a) (PlacedVoice a)), matching a score of contingous, non-overlapping notes starting at an arbitrary point

type Score a = [Note a]
-- Free Monoid
instance Transformable (Score a) where
  transform s = fmap (transform s)
-- TODO what is the Position instance?
-- We need a Monoid to fold the spans of each note into a single span
-- Can not use the transform monoid, instead we need a hull
-- Breaks TCMs! Another problem is that there is no hull for Spans (as *ordered* pairs)


-- Both PlacedVoice and Score support various notions of position

-- For the simplest case we simply disregard negative durations (and so non-forward spans)
-- as well as nominal/actual position (so no anacrusis) – both cases trivial.

-- To deal with anacrusis etc., both PlacedVoice and Score must include an extra Span, 
-- defining logical position (as opposed to actual position of its components). Either
-- define this as a separate value (defaulting to same as actual position) and transform,
-- or define as a transformation from actual to logical time (or vice versa) and default
-- to mempty. This would give us the same power as TPTM.




