
class HasDuration a where
  _duration :: a -> Duration
class HasDuration a => HasPosition a where
  _position :: a -> Duration -> Time
  _onset :: a -> Time
  _offset :: a -> Time
class Transformable a => Reversible a where
  rev :: a -> a
class Splittable a where
  split :: Duration -> a -> (a, a)
  beginning :: Duration -> a -> a
  ending :: Duration -> a -> a
class Transformable a where
  transform :: Span -> a -> a

newtype Behavior a
newtype Bound a
newtype Chord a
newtype Delayed a
newtype Duration
newtype Future a
newtype Graces (f :: * -> *) a
newtype NoReverse a
newtype Nominal (f :: * -> *) a
newtype Music.Time.Note.Note a
newtype Past a
newtype Reactive a
newtype Score a
newtype Segment a
newtype Span
newtype Stretched a
newtype Time
newtype Track a
newtype Voice a


-- For behaviors etc. Is an op because it resembles and generalizes Prelude.(!!)
(!)                   :: Representable f => f a -> Rep f -> a

-- Span is common enough to have a literal. Must be 3 to avoid privilege
(<-<)                 :: Duration -> Time -> Span
(<->)                 :: Time -> Time -> Span
(>->)                 :: Time -> Duration -> Span

-- Composition ops (need to change). There are more, but these are fundamental enough.
(<|)                  :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(|>)                  :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a

------------------------------------------------------------------------------------------
-- Span
------------------------------------------------------------------------------------------

delta                 :: Iso' Span (Time, Duration)
codelta               :: Iso' Span (Duration, Time)
range                 :: Iso' Span (Time, Time)

normalizeSpan         :: Span -> Span
reverseSpan           :: Span -> Span
reflectSpan           :: Time -> Span -> Span

fixedDurationSpan     :: Prism' Span Time
fixedOnsetSpan        :: Prism' Span Duration
-- stretchComponent      :: Span -> Duration
-- delayComponent        :: Span -> Time

inside                :: Time -> Span -> Bool
encloses              :: Span -> Span -> Bool
overlaps              :: Span -> Span -> Bool
properlyEncloses      :: Span -> Span -> Bool
isBackwardSpan        :: Span -> Bool

isBefore              :: Span -> Span -> Bool
isEmptySpan           :: Span -> Bool
isForwardSpan         :: Span -> Bool
isProper              :: Span -> Bool

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

showCodelta           :: Span -> String
showDelta             :: Span -> String
showRange             :: Span -> String

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------


bounding              :: Span -> a -> Bound a
bounds                :: Time -> Time -> a -> Bound a



after                 :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
before                :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a




chunks                :: (Splittable a, HasDuration a) => Duration -> a -> [a]

compress              :: Transformable a => Duration -> a -> a
compressing           :: Duration -> Span

delay                 :: Transformable a => Duration -> a -> a

delayTime             :: Transformable a => Time -> a -> a
delayed               :: Iso (Time, a) (Time, b) (Delayed a) (Delayed b)
delayee               :: (Transformable a, Transformable b) => Lens (Delayed a) (Delayed b) a b
delayeds              :: Lens (Track a) (Track b) [Delayed a] [Delayed b]
delaying              :: Duration -> Span

duration              :: (Transformable a, HasDuration a) => Lens' a Duration
during                :: (HasPosition a, HasPosition b, Transformable a, Transformable b) => a -> b -> a
era                   :: (HasPosition a, Transformable a) => Lens' a Span

event                 :: Iso (Note a) (Note b) (Time, Duration, a) (Time, Duration, b)

follow                :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
future                :: Future a -> Time -> Maybe a
futureSeg             :: Future (Segment a) -> Behavior (Maybe a)

indexPast             :: [Past a] -> Time -> Maybe a



itransform            :: Transformable a => Span -> a -> a
lead                  :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a

midpoint              :: (HasPosition a, Transformable a) => Lens' a Time

note                  :: Iso (Span, a) (Span, b) (Note a) (Note b)
notee                 :: (Transformable a, Transformable b) => Lens (Note a) (Note b) a b

offset                :: (HasPosition a, Transformable a) => Lens' a Time
offsetPoints          :: AffineSpace a => a -> [Diff a] -> [a]
onset                 :: (HasPosition a, Transformable a) => Lens' a Time

palindrome            :: (Semigroup a, Reversible a, HasPosition a) => a -> a
past                  :: Past a -> Time -> Maybe a
pastSeg               :: Past (Segment a) -> Behavior (Maybe a)
pcat                  :: (Semigroup a, Monoid a) => [a] -> a
placeAt               :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
position              :: (HasPosition a, Transformable a) => Duration -> Lens' a Time
postOffset            :: (HasPosition a, Transformable a) => Lens' a Time
postOnset             :: (HasPosition a, Transformable a) => Lens' a Time
preOnset              :: (HasPosition a, Transformable a) => Lens' a Time



rest                  :: Applicative f => f (Maybe a)
revDefault            :: (HasPosition a, Transformable a) => a -> a
reversed              :: Reversible a => Iso' a a

scat                  :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => [a] -> a

segment               :: Iso (Duration -> a) (Duration -> b) (Segment -> a) (Segment -> b)
apSegments            :: Voice (Segment a) -> Stretched (Segment a)
apSegments'           :: Stretched (Segment a) -> Stretched (Segment a) -> Stretched (Segment a)



stretchTo             :: (Transformable a, HasDuration a) => Duration -> a -> a
stretched             :: Iso (Duration, a) (Duration, b) (Stretched a) (Stretched b)
stretchee             :: (Transformable a, Transformable b) => Lens (Stretched a) (Stretched b) a b
stretching            :: Duration -> Span

sustain               :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a

times                 :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => Int -> a -> a
toAbsoluteTime        :: [Duration] -> [Time]
toRelativeTime        :: [Time] -> [Duration]
toRelativeTimeN       :: [Time] -> [Duration]
toRelativeTimeN'      :: Time -> [Time] -> [Duration]
transformed           :: (Transformable a, Transformable b) => Span -> Iso a b a b

undelay               :: Transformable a => Duration -> a -> a
undelaying            :: Duration -> Span






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

sine                  :: Floating a => Behavior a
splice                :: Behavior a -> Bound (Behavior a) -> Behavior a
splitAbs              :: (HasPosition a, Splittable a) => Time -> a -> (a, a)
splitReactive         :: Reactive a -> Either a ((a, Time), [Music.Time.Note.Note a], (Time, a))
startAt               :: (Transformable a, HasPosition a) => Time -> a -> a
stopAt                :: (Transformable a, HasPosition a) => Time -> a -> a
stretch               :: Transformable a => Duration -> a -> a

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

