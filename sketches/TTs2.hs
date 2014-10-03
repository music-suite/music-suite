
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
newtype Stretched a
newtype Note a

newtype Chord a
newtype Voice a
newtype Track a
newtype Score a

newtype Reactive a
newtype Segment a
newtype Behavior a


newtype NoReverse a
newtype Graces (f :: * -> *) a
newtype Nominal (f :: * -> *) a


-- For behaviors etc. Is an op because it resembles and generalizes Prelude.(!!)
(!)                   :: Representable f => f a -> Rep f -> a


------------------------------------------------------------------------------------------
-- Time/Duration
------------------------------------------------------------------------------------------

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

-- Span is common enough to have a literal. Must be 3 to avoid privileging one (though the Show instance does that anyway)
(<-<)                 :: Duration -> Time -> Span
(<->)                 :: Time -> Time -> Span
(>->)                 :: Time -> Duration -> Span

delta                 :: Iso' Span (Time, Duration)
codelta               :: Iso' Span (Duration, Time)
range                 :: Iso' Span (Time, Time)

normalizeSpan         :: Span -> Span
reverseSpan           :: Span -> Span
reflectSpan           :: Time -> Span -> Span

fixedDurationSpan     :: Prism' Span Time
fixedOnsetSpan        :: Prism' Span Duration

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

past                  :: Past a -> Time -> Maybe a
pastSeg               :: Past (Segment a) -> Behavior (Maybe a)
indexPast             :: [Past a] -> Time -> Maybe a

future                :: Future a -> Time -> Maybe a
futureSeg             :: Future (Segment a) -> Behavior (Maybe a)

bounding              :: Span -> a -> Bound a
bounds                :: Time -> Time -> a -> Bound a


------------------------------------------------------------------------------------------
-- Reactive
------------------------------------------------------------------------------------------

atTime                :: Reactive a -> Time -> a
final                 :: Reactive a -> a
initial               :: Reactive a -> a
intermediate          :: Transformable a => Reactive a -> [Music.Time.Note.Note a]
occs                  :: Reactive a -> [Time]
switchR               :: Time -> Reactive a -> Reactive a -> Reactive a
updates               :: Reactive a -> [(Time, a)]

------------------------------------------------------------------------------------------
-- Segment
------------------------------------------------------------------------------------------

segment               :: Iso (Duration -> a) (Duration -> b) (Segment -> a) (Segment -> b)
apSegments            :: Voice (Segment a) -> Stretched (Segment a)
apSegments'           :: Stretched (Segment a) -> Stretched (Segment a) -> Stretched (Segment a)
splitReactive         :: Reactive a -> Either a ((a, Time), [Note a], (Time, a))

------------------------------------------------------------------------------------------
-- Behavior
------------------------------------------------------------------------------------------

behavior              :: Iso (Time -> a) (Time -> b) (Behavior a) (Behavior b)
cosine                :: Floating a => Behavior a
discrete              :: Reactive a -> Behavior a
impulse               :: Num a => Behavior a
line                  :: Fractional a => Behavior a
sample                :: [Time] -> Behavior a -> Reactive a
sawtooth              :: RealFrac a => Behavior a
bounded               :: Iso (Note (Segment a)) (Note (Segment b)) (Bound (Behavior a)) (Bound (Behavior b))
continous             :: Reactive (Segment a) -> Behavior a
continousWith         :: Segment (a -> b) -> Reactive a -> Behavior b
focusing              :: Functor f => (Segment a -> f (Segment a)) -> Behavior a -> f (Behavior a)
switch                :: Time -> Behavior a -> Behavior a -> Behavior a
switch'               :: Time -> Behavior a -> Behavior a -> Behavior a -> Behavior a
trim                  :: Monoid b => Bound (Behavior b) -> Behavior b
trimAfter             :: Monoid a => Time -> Behavior a -> Behavior a
trimBefore            :: Monoid a => Time -> Behavior a -> Behavior a
trimR                 :: Monoid a => Span -> Reactive a -> Reactive a
turnOff               :: Num a => Behavior a
turnOn                :: Num a => Behavior a
unit                  :: Fractional a => Behavior a      
sine                  :: Floating a => Behavior a
splice                :: Behavior a -> Bound (Behavior a) -> Behavior a

------------------------------------------------------------------------------------------
-- Note/Delayed/Stretched
------------------------------------------------------------------------------------------

event                 :: Iso (Note a) (Note b) (Time, Duration, a) (Time, Duration, b)
note                  :: Iso (Span, a) (Span, b) (Note a) (Note b)
notee                 :: (Transformable a, Transformable b) => Lens (Note a) (Note b) a b
delayed               :: Iso (Time, a) (Time, b) (Delayed a) (Delayed b)
delayee               :: (Transformable a, Transformable b) => Lens (Delayed a) (Delayed b) a b
stretched             :: Iso (Duration, a) (Duration, b) (Stretched a) (Stretched b)
stretchee             :: (Transformable a, Transformable b) => Lens (Stretched a) (Stretched b) a b

rest                  :: Applicative f => f (Maybe a)

------------------------------------------------------------------------------------------
-- Chord
------------------------------------------------------------------------------------------

chord                 :: Getter [Delayed a] (Chord a)
unchord               :: Lens (Chord a) (Chord b) [Delayed a] [Delayed b]
unsafeChord           :: Iso (Chord a) (Chord b) [Delayed a] [Delayed b]
        
------------------------------------------------------------------------------------------
-- Voice
------------------------------------------------------------------------------------------

voice                 :: Getter [Stretched a] (Voice a)
voiceAsList           :: Iso (Voice a) (Voice b) [a] [b]
voiceLens             :: (s -> a) -> (b -> s -> t) -> Lens (Voice s) (Voice t) (Voice a) (Voice b)
stretcheds            :: Lens (Voice a) (Voice b) [Stretched a] [Stretched b]
unsafeStretcheds      :: Iso (Voice a) (Voice b) [Stretched a] [Stretched b]
eventsV               :: Lens (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
unsafeEventsV         :: Iso (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]

durationsV            :: Functor f => ([Duration] -> f [Duration]) -> Voice a -> f (Voice a)
valuesV               :: Functor f => ([a] -> f [b]) -> Voice a -> f (Voice b)
reverseDurations      :: Voice a -> Voice a
reverseValues         :: Voice a -> Voice a
rotateDurations       :: Int -> Voice a -> Voice a
rotateValues          :: Int -> Voice a -> Voice a

withContext           :: Voice a -> Voice (Ctxt a)
withDurations         :: ([Duration] -> [Duration]) -> Voice a -> Voice a
withValues            :: ([a] -> [b]) -> Voice a -> Voice b     coverRests            :: Voice (Maybe a) -> Maybe (Voice a)

fuse                  :: Eq a => Voice a -> Voice a
fuseBy                :: (a -> a -> Bool) -> Voice a -> Voice a
fuseRests             :: Voice (Maybe a) -> Voice (Maybe a)

unzipVoice            :: Voice (a, b) -> (Voice a, Voice b)
zipVoice              :: Voice a -> Voice b -> Voice (a, b)
zipVoice3             :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
zipVoice4             :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoiceNoScale       :: Voice a -> Voice b -> Voice (a, b)
zipVoiceNoScale3      :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
zipVoiceNoScale4      :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoiceWith          :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith'         :: (Duration -> Duration -> Duration) -> (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWithNoScale   :: (a -> b -> c) -> Voice a -> Voice b -> Voice c

------------------------------------------------------------------------------------------
-- Score (= Track)
------------------------------------------------------------------------------------------

score                 :: Getter [Note a] (Score a)
concatB               :: Monoid a => Score (Behavior a) -> Behavior a
eras                  :: Applicative f => (Span -> f Span) -> Score a -> f (Score a)
events                :: Lens (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
filterEvents          :: (Time -> Duration -> a -> Bool) -> Score a -> Score a
filterWithSpan        :: (Span -> a -> Bool) -> Score a -> Score a
mapEvents             :: (Time -> Duration -> a -> b) -> Score a -> Score b
mapFilterEvents       :: (Time -> Duration -> a -> Maybe b) -> Score a -> Score b
mapFilterWithSpan     :: (Span -> a -> Maybe b) -> Score a -> Score b
mapWithSpan           :: (Span -> a -> b) -> Score a -> Score b
normalizeScore        :: Score a -> Score a
notes                 :: Lens (Score a) (Score b) [Note a] [Note b]
printEras             :: Score a -> IO ()
simult                :: Transformable a => Lens (Score a) (Score b) (Score [a]) (Score [b])
simultaneous          :: (Transformable a, Semigroup a) => Score a -> Score a
unsafeEvents          :: Iso (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
unsafeNotes           :: Iso (Score a) (Score b) [Note a] [Note b]

