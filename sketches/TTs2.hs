
{-
TODO REDUCE API!!

  Fairbairn threshold

-}

class Transformable a where
  transform :: Span -> a -> a
class Transformable a => Reversible a where
  rev :: a -> a
class HasDuration a where
  _duration :: a -> Duration
class HasDuration a => HasPosition a where
  _position :: a -> Duration -> Time
  _onset :: a -> Time
  _offset :: a -> Time
class Splittable a where
  split :: Duration -> a -> (a, a)
  beginning :: Duration -> a -> a
  ending :: Duration -> a -> a


newtype Time
Enum, Eq, Fractional, Num, Ord, Real, RealFrac, Show, VectorSpace, AffineSpace, AdditiveGroup, Semigroup, HasPosition, Transformable, HasDuration, Monoid
newtype Duration
newtype Span

newtype Past a
newtype Future a
newtype Bound a

newtype Delayed a
newtype Note a
newtype Event a

newtype Chord a
newtype Voice a
newtype Track a
newtype Track a

newtype Reactive a
newtype Spline a
newtype Behavior a


newtype NoReverse a
newtype Graces (f :: * -> *) a
newtype Nominal (f :: * -> *) a


-- For behaviors etc. Is an op because it resembles and generalizes Prelude.(!!)
(!)                   :: Representable f => f a -> Rep f -> a


------------------------------------------------------------------------------------------
-- Time/Duration
------------------------------------------------------------------------------------------

data Time
data Duration

-- TODO rename:
offsetPoints          :: AffineSpace a => a -> [Diff a] -> [a]

-- toTime
-- toDuration
toAbsoluteTime        :: [Duration] -> [Time]
toRelativeTime        :: [Time] -> [Duration]
toRelativeTimeN       :: [Time] -> [Duration]
-- toRelativeTimeN'      :: Time -> [Time] -> [Duration]

------------------------------------------------------------------------------------------
-- Span
------------------------------------------------------------------------------------------

type Span = (Time, Duration)

-- Span is common enough to have a literal. Must be 3 to avoid privileging one (though the Show instance does that anyway)
(<-<)                 :: Duration -> Time -> Span
(<->)                 :: Time -> Time -> Span
(>->)                 :: Time -> Duration -> Span

delta                 :: Iso' Span (Time, Duration)
codelta               :: Iso' Span (Duration, Time)
range                 :: Iso' Span (Time, Time)

fixedDurationSpan     :: Prism' Span Time
fixedOnsetSpan        :: Prism' Span Duration

normalizeSpan         :: Span -> Span
reverseSpan           :: Span -> Span
reflectSpan           :: Time -> Span -> Span

isEmptySpan           :: Span -> Bool
-- isForwardSpan         :: Span -> Bool
isBackwardSpan        :: Span -> Bool
inside                :: Time -> Span -> Bool
encloses              :: Span -> Span -> Bool
properlyEncloses      :: Span -> Span -> Bool
overlaps              :: Span -> Span -> Bool
-- isBefore              :: Span -> Span -> Bool
-- isProper              :: Span -> Bool

-- strictlyAfterOffset   :: Time -> Span -> Bool
-- strictlyAfterOnset    :: Time -> Span -> Bool
-- strictlyBeforeOffset  :: Time -> Span -> Bool
-- strictlyBeforeOnset   :: Time -> Span -> Bool
-- afterOffset           :: Time -> Span -> Bool
-- afterOnset            :: Time -> Span -> Bool
-- beforeOffset          :: Time -> Span -> Bool
-- beforeOnset           :: Time -> Span -> Bool
-- startsBefore          :: Span -> Span -> Bool
-- startsLater           :: Span -> Span -> Bool
-- startsWhenStarts      :: Span -> Span -> Bool
-- startsWhenStops       :: Span -> Span -> Bool
-- stopsAtTheSameTime    :: Span -> Span -> Bool
-- stopsBefore           :: Span -> Span -> Bool
-- stopsLater            :: Span -> Span -> Bool
-- stopsWhenStarts       :: Span -> Span -> Bool
-- stopsWhenStops        :: Span -> Span -> Bool

showRange             :: Span -> String
showDelta             :: Span -> String
showCodelta           :: Span -> String

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- + Transformable

-- itransform            :: Transformable a => Span -> a -> a
transformed           :: (Transformable a, Transformable b) => Span -> Iso a b a b
-- TODO stop using Delayed/Stretch etc for type names, so we can use these names
-- for specifications of transformed (delayed/stretched etc)

delaying              :: Duration -> Span
undelaying            :: Duration -> Span
stretching            :: Duration -> Span
compressing           :: Duration -> Span
delay                 :: Transformable a => Duration -> a -> a
undelay               :: Transformable a => Duration -> a -> a
stretch               :: Transformable a => Duration -> a -> a
compress              :: Transformable a => Duration -> a -> a
-- delayTime             :: Transformable a => Time -> a -> a

-- + HasDuration
-- TODO stretchTo OK? does it really stretch notes without affecting onset
duration              :: (Transformable a, HasDuration a) => Lens' a Duration
stretchTo             :: (Transformable a, HasDuration a) => Duration -> a -> a

-- + HasPosition

revDefault            :: (HasPosition a, Transformable a) => a -> a
startAt               :: (HasPosition a, Transformable a) => Time -> a -> a
stopAt                :: (HasPosition a, Transformable a) => Time -> a -> a

-- TODO remove some
era                   :: (HasPosition a, Transformable a) => Lens' a Span
placeAt               :: (HasPosition a, Transformable a) => Duration -> Time -> a -> a

position              :: (HasPosition a, Transformable a) => Duration -> Lens' a Time
-- onset                 :: (HasPosition a, Transformable a) => Lens' a Time
-- offset                :: (HasPosition a, Transformable a) => Lens' a Time
-- midpoint              :: (HasPosition a, Transformable a) => Lens' a Time
-- postOffset            :: (HasPosition a, Transformable a) => Lens' a Time
-- postOnset             :: (HasPosition a, Transformable a) => Lens' a Time
-- preOnset              :: (HasPosition a, Transformable a) => Lens' a Time

-- TODO during = sustain?
-- during                :: (HasPosition a, HasPosition b, Transformable a, Transformable b) => a -> b -> a
-- sustain               :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
-- lead                  :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
-- follow                :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
-- after                 :: (HasPosition a, Transformable a, Semigroup a) => a -> a -> a
-- before                :: (HasPosition a, Transformable a, Semigroup a) => a -> a -> a

-- + Reversible
reversed              :: Reversible a => Iso' a a

-- + Splittable
chunks                :: (Splittable a, HasDuration a) => Duration -> a -> [a]
splitAbs              :: (HasPosition a, Splittable a) => Time -> a -> (a, a)
-- palindrome            :: (Semigroup a, Reversible a, HasPosition a) => a -> a
times                 :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => Int -> a -> a

-- Composition ops (need to change). There are more, but these are fundamental enough.
(<|)                  :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(|>)                  :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
-- pcat/scat:
together              :: (Semigroup a, Monoid a) => [a] -> a
sequential            :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => [a] -> a







------------------------------------------------------------------------------------------
-- Future/Past/Bound
------------------------------------------------------------------------------------------

type Future a = (Min Time, a)
type Past a = (Max Time, a)

past                  :: Past a -> Time -> Maybe a
-- Project a spline (backwards) from a given point
pastSeg               :: Past (Spline a) -> Behavior (Maybe a)
-- Given list of past values ordered in time, find the value at the lowest switching time that is (>= given time)
-- This is Reactive indexing without the final value
indexPast             :: [Past a] -> Time -> Maybe a

future                :: Future a -> Time -> Maybe a
futureSeg             :: Future (Spline a) -> Behavior (Maybe a)
-- Given list of past values ordered in time, find the value at the highest switching time that is (<= given time)
-- This is Reactive indexing without the initial value
indexFuture           :: [Future a] -> Time -> Maybe a 

-- Event that both pastSeg and futureSet do not specify an order for simultaneous occurences
-- (more than one value may be the lowest/highest at a given point)

-- However futureSet and pastSeg differ "in which spant the switching point is placed" – forward or backward

bounding              :: Span -> a -> Bound a
bounds                :: Time -> Time -> a -> Bound a

------------------------------------------------------------------------------------------

-- Split a list of futures into notes and the final future
-- > length (fst foo) = length' foo - 1
foo :: NonEmpty (Future a) -> ([Event a], Future a)
foo :: NonEmpty (Past a)   -> (Past a, [Event a])

------------------------------------------------------------------------------------------
-- Reactive
------------------------------------------------------------------------------------------

type Reactive a = (a, [Future a]) = ([Past a], a)

indexReactive         :: Reactive a -> Time -> a
-- No tabulate!

initial               :: Reactive a -> a
final                 :: Reactive a -> a
-- intermediate          :: Transformable a => Reactive a -> [Event a]
occs                  :: Reactive a -> [Time]
-- ?
updates               :: Reactive a -> [(Time, a)]
switchR               :: Time -> Reactive a -> Reactive a -> Reactive a

-- TODO a "double-ended comonad"

------------------------------------------------------------------------------------------
-- Spline
------------------------------------------------------------------------------------------

-- Just 0 to 1
type Spline a = Duration -> a where {d : Duration, 0 <= d <= 1}
spline                :: Iso (Duration -> a) (Duration -> b) (Spline -> a) (Spline -> b)

type Segment a = Voice (Spline a)
segment               :: Iso (Duration -> a) (Duration -> b) (Spline -> a) (Spline -> b)

-- Spline is a Semigroup, Segment is a Monoid

-- Join given percentage of new spline taken up by first spline
join                  :: Duration -> Spline a -> Spline a -> Spline a
-- Join with no relative time (both takes up half of the new spline)
join'                 :: Spline a -> Spline a -> Spline a

instant               :: Spline a
append                :: Spline a -> Spline a -> Spline a

trimR                 :: Monoid a => Span -> Reactive a -> Reactive a
-- splitReactive         :: Reactive a -> Either a ((a, Time), [Event a], (Time, a))

------------------------------------------------------------------------------------------
-- Behavior
------------------------------------------------------------------------------------------

type Behavior a = Time -> a = Reactive (Spline a) -- ?

behavior              :: Iso (Time -> a) (Time -> b) (Behavior a) (Behavior b)
view behavior         :: (Time -> a) -> Behavior a
indexBehavior         :: Behavior a -> Time -> a
tabulateBehavior      :: (Time -> a) -> Behavior a
-- view behavior         :: (Time -> a) -> Behavior a
-- view (from behavior)  :: Behavior a -> Time -> a

discrete              :: Reactive a -> Behavior a
continous             :: Reactive (Spline a) -> Behavior a
continousWith         :: Spline (a -> b) -> Reactive a -> Behavior b

sample                :: [Time] -> Behavior a -> Reactive a
sample'               :: Time -> [Time] -> Behavior a -> Reactive a

bounded               :: Iso (Event (Spline a)) (Event (Spline b)) (Bound (Behavior a)) (Bound (Behavior b))


focusing              :: Functor f => (Spline a -> f (Spline a)) -> Behavior a -> f (Behavior a)

switch                :: Time -> Behavior a -> Behavior a -> Behavior a
switch'               :: Time -> Behavior a -> Behavior a -> Behavior a -> Behavior a
trim                  :: Monoid b => Bound (Behavior b) -> Behavior b
trimAfter             :: Monoid a => Time -> Behavior a -> Behavior a
trimBefore            :: Monoid a => Time -> Behavior a -> Behavior a

impulse               :: Num a => Behavior a
turnOff               :: Num a => Behavior a
turnOn                :: Num a => Behavior a
splice                :: Behavior a -> Bound (Behavior a) -> Behavior a
unit                  :: Fractional a => Behavior a      
line                  :: Fractional a => Behavior a
-- cosine                :: Floating a => Behavior a
-- sine                  :: Floating a => Behavior a
-- sawtooth              :: RealFrac a => Behavior a

------------------------------------------------------------------------------------------
-- Delayed/Event/Note
------------------------------------------------------------------------------------------

-- event                 :: Iso (Event a) (Event b) (Time, Duration, a) (Time, Duration, b)
event                 :: Iso (Span, a) (Span, b) (Event a) (Event b)
eventee               :: (Transformable a, Transformable b) => Lens (Event a) (Event b) a b
delayed               :: Iso (Time, a) (Time, b) (Delayed a) (Delayed b)
delayee               :: (Transformable a, Transformable b) => Lens (Delayed a) (Delayed b) a b
note                  :: Iso (Duration, a) (Duration, b) (Note a) (Note b)
notee                 :: (Transformable a, Transformable b) => Lens (Note a) (Note b) a b

rest                  :: Iso' Duration (Rest a)
-- notee                 :: (Transformable a, Transformable b) => Lens (Rest a) (Rest b) a b

rest                  :: Applicative f => f (Maybe a)

------------------------------------------------------------------------------------------
-- Chord
------------------------------------------------------------------------------------------

-- Composition/rendering?

chord                 :: Getter [Delayed a] (Chord a)
unchord               :: Lens (Chord a) (Chord b) [Delayed a] [Delayed b]
unsafeChord           :: Iso (Chord a) (Chord b) [Delayed a] [Delayed b]
        
------------------------------------------------------------------------------------------
-- Voice
------------------------------------------------------------------------------------------

-- Composition/rendering?

voice                 :: Getter [Note a] (Voice a)
notes                 :: Lens (Voice a) (Voice b) [Note a] [Note b]
unsafeNotes           :: Iso (Voice a) (Voice b) [Note a] [Note b]
-- notes'                :: Lens (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
-- unsafeNotes'          :: Iso (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]

-- Was: voiceLens
insideVoice           :: Lens s t a b -> Lens (Voice s) (Voice t) (Voice a) (Voice b)

-- durationsV            :: Functor f => ([Duration] -> f [Duration]) -> Voice a -> f (Voice a)
-- valuesV               :: Functor f => ([a] -> f [b]) -> Voice a -> f (Voice b)
-- withDurations         :: ([Duration] -> [Duration]) -> Voice a -> Voice a
-- withValues            :: ([a] -> [b]) -> Voice a -> Voice b
-- reverseDurations      :: Voice a -> Voice a
-- reverseValues         :: Voice a -> Voice a
-- rotateDurations       :: Int -> Voice a -> Voice a
-- rotateValues          :: Int -> Voice a -> Voice a

-- TODO rename
withContext           :: Voice a -> Voice (Ctxt a)

fuse                  :: Eq a => Voice a -> Voice a
fuseBy                :: (a -> a -> Bool) -> Voice a -> Voice a
fuseRests             :: Voice (Maybe a) -> Voice (Maybe a)
coverRests            :: Voice (Maybe a) -> Maybe (Voice a)

-- TODO contrast these zips to the Applicative instance and Reactive
unzipVoice            :: Voice (a, b) -> (Voice a, Voice b)
zipVoice              :: Voice a -> Voice b -> Voice (a, b)
-- zipVoice3             :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
-- zipVoice4             :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoiceNoScale       :: Voice a -> Voice b -> Voice (a, b)
-- zipVoiceNoScale3      :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
-- zipVoiceNoScale4      :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoiceWith          :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith'         :: (Duration -> Duration -> Duration) -> (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWithNoScale   :: (a -> b -> c) -> Voice a -> Voice b -> Voice c

------------------------------------------------------------------------------------------
-- TODO chords of voices etc

Reactive a 

-- Homohphonic to polyphonic, always safe
foo :: Voice (Chord a) -> Chord (Voice a)
foo :: Chord (Voice a) -> Maybe (Voice (Chord a))
foo :: Prism' (Chord (Voice a)) (Voice (Chord a))

foo :: Note a -> Track a
foo :: Voice a -> Track a
foo :: Chord a -> Track a -- TODO how?

------------------------------------------------------------------------------------------
-- Track (Was: Track)
------------------------------------------------------------------------------------------

score                 :: Getter [Event a] (Track a)
events                :: Lens (Track a) (Track b) [Event a] [Event b]
unsafeEvents          :: Iso (Track a) (Track b) [(Time, Duration, a)] [(Time, Duration, b)]
-- events'               :: Lens (Track a) (Track b) [(Time, Duration, a)] [(Time, Duration, b)]
-- unsafeEvents'         :: Iso (Track a) (Track b) [Event a] [Event b]
-- eras                  :: Traversal' (Track a) Span

mapWithSpan           :: (Span -> a -> b) -> Track a -> Track b
filterWithSpan        :: (Span -> a -> Bool) -> Track a -> Track a
mapFilterWithSpan     :: (Span -> a -> Maybe b) -> Track a -> Track b
mapEvents'            :: (Time -> Duration -> a -> b) -> Track a -> Track b
filterEvents'         :: (Time -> Duration -> a -> Bool) -> Track a -> Track a
mapFilterEvents'      :: (Time -> Duration -> a -> Maybe b) -> Track a -> Track b

-- TODO one has to go
simult                :: Transformable a => Lens (Track a) (Track b) (Track [a]) (Track [b])
simultaneous          :: (Transformable a, Semigroup a) => Track a -> Track a


-- TODO generalize
normalizeTrack        :: Track a -> Track a
concatB               :: Monoid a => Track (Behavior a) -> Behavior a
printEras             :: Track a -> IO ()
