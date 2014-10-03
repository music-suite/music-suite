
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

after                 :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
afterOffset           :: Time -> Span -> Bool
afterOnset            :: Time -> Span -> Bool
apSegments            :: Voice (Segment a) -> Stretched (Segment a)
apSegments'           :: Stretched (Segment a) -> Stretched (Segment a) -> Stretched (Segment a)
atTime                :: Reactive a -> Time -> a
before                :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
beforeOffset          :: Time -> Span -> Bool
beforeOnset           :: Time -> Span -> Bool
behavior              :: Iso (Time -> a) (Time -> b) (Behavior a) (Behavior b)
bounded               :: Iso (Note (Segment a)) (Note (Segment b)) (Bound (Behavior a)) (Bound (Behavior b))
bounding              :: Span -> a -> Bound a
bounds                :: Time -> Time -> a -> Bound a
chord                 :: Getter [Delayed a] (Chord a)
chunks                :: (Splittable a, HasDuration a) => Duration -> a -> [a]
codelta               :: Iso' Span (Duration, Time)
compress              :: Transformable a => Duration -> a -> a
compressing           :: Duration -> Span
concatB               :: Monoid a => Score (Behavior a) -> Behavior a
continous             :: Reactive (Segment a) -> Behavior a
continousWith         :: Segment (a -> b) -> Reactive a -> Behavior b
cosine                :: Floating a => Behavior a
coverRests            :: Voice (Maybe a) -> Maybe (Voice a)
delay                 :: Transformable a => Duration -> a -> a
delayComponent        :: Span -> Time
delayTime             :: Transformable a => Time -> a -> a
delayed               :: Iso (Time, a) (Time, b) (Delayed a) (Delayed b)
delayee               :: (Transformable a, Transformable b) => Lens (Delayed a) (Delayed b) a b
delayeds              :: Lens (Track a) (Track b) [Delayed a] [Delayed b]
delaying              :: Duration -> Span
delta                 :: Iso' Span (Time, Duration)
discrete              :: Reactive a -> Behavior a
duration              :: (Transformable a, HasDuration a) => Lens' a Duration
durationsV            :: Functor f => ([Duration] -> f [Duration]) -> Voice a -> f (Voice a)
during                :: (HasPosition a, HasPosition b, Transformable a, Transformable b) => a -> b -> a
encloses              :: Span -> Span -> Bool
era                   :: (HasPosition a, Transformable a) => Lens' a Span
eras                  :: Applicative f => (Span -> f Span) -> Score a -> f (Score a)
event ::
  (Profunctor p, Functor f) =>
  p (Time, Duration, a) (f (Time, Duration, b))
  -> p (Music.Time.Note.Note a) (f (Music.Time.Note.Note b))
events                :: Lens (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
eventsV               :: Lens (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
filterEvents          :: (Time -> Duration -> a -> Bool) -> Score a -> Score a
filterWithSpan        :: (Span -> a -> Bool) -> Score a -> Score a
final                 :: Reactive a -> a
firstTrue             :: [Maybe a] -> Maybe a
fixedDurationSpan     :: Prism' Span Time
fixedOnsetSpan        :: Prism' Span Duration
focusing              :: Functor f => (Segment a -> f (Segment a)) -> Behavior a -> f (Behavior a)
follow                :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
fuse                  :: Eq a => Voice a -> Voice a
fuseBy                :: (a -> a -> Bool) -> Voice a -> Voice a
fuseRests             :: Voice (Maybe a) -> Voice (Maybe a)
future                :: Future a -> Time -> Maybe a
futureSeg             :: Future (Segment a) -> Behavior (Maybe a)
impulse               :: Num a => Behavior a
indexPast             :: [Past a] -> Time -> Maybe a
initial               :: Reactive a -> a
inside                :: Time -> Span -> Bool
intermediate          :: Transformable a => Reactive a -> [Music.Time.Note.Note a]
isBackwardSpan        :: Span -> Bool
isBefore              :: Span -> Span -> Bool
isEmptySpan           :: Span -> Bool
isForwardSpan         :: Span -> Bool
isProper              :: Span -> Bool
itransform            :: Transformable a => Span -> a -> a
lead                  :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
line                  :: Fractional a => Behavior a
mapEvents             :: (Time -> Duration -> a -> b) -> Score a -> Score b
mapFilterEvents       :: (Time -> Duration -> a -> Maybe b) -> Score a -> Score b
mapFilterWithSpan     :: (Span -> a -> Maybe b) -> Score a -> Score b
mapWithSpan           :: (Span -> a -> b) -> Score a -> Score b
midpoint              :: (HasPosition a, Transformable a) => Lens' a Time
normalizeScore        :: Score a -> Score a
normalizeSpan         :: Span -> Span
note                  :: Iso (Span, a) (Span, b) (Note a) (Note b)
notee                 :: (Transformable a, Transformable b) => Lens (Note a) (Note b) a b
notes                 :: Lens (Score a) (Score b) [Note a] [Note b]
occs                  :: Reactive a -> [Time]
offset                :: (HasPosition a, Transformable a) => Lens' a Time
offsetPoints          :: AffineSpace a => a -> [Diff a] -> [a]
onset                 :: (HasPosition a, Transformable a) => Lens' a Time
overlaps              :: Span -> Span -> Bool
palindrome            :: (Semigroup a, Reversible a, HasPosition a) => a -> a
past                  :: Past a -> Time -> Maybe a
pastSeg               :: Past (Segment a) -> Behavior (Maybe a)
pcat                  :: (Semigroup a, Monoid a) => [a] -> a
placeAt               :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
position              :: (HasPosition a, Transformable a) => Duration -> Lens' a Time
postOffset            :: (HasPosition a, Transformable a) => Lens' a Time
postOnset             :: (HasPosition a, Transformable a) => Lens' a Time
preOnset              :: (HasPosition a, Transformable a) => Lens' a Time
printEras             :: Score a -> IO ()
properlyEncloses      :: Span -> Span -> Bool
range                 :: Iso' Span (Time, Time)
reflectSpan           :: Time -> Span -> Span
rest                  :: Applicative f => f (Maybe a)
revDefault            :: (HasPosition a, Transformable a) => a -> a
reverseDurations      :: Voice a -> Voice a
reverseSpan           :: Span -> Span
reverseValues         :: Voice a -> Voice a
reversed              :: Reversible a => Iso' a a
rotateDurations       :: Int -> Voice a -> Voice a
rotateValues          :: Int -> Voice a -> Voice a
sample                :: [Time] -> Behavior a -> Reactive a
sawtooth              :: RealFrac a => Behavior a
scat                  :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => [a] -> a
score                 :: Getter [Note a] (Score a)
segment               :: Iso (Duration -> a) (Duration -> b) (Segment -> a) (Segment -> b)
showCodelta           :: Span -> String
showDelta             :: Span -> String
showRange             :: Span -> String
simult                :: Transformable a => Lens (Score a) (Score b) (Score [a]) (Score [b])
simultaneous          :: (Transformable a, Semigroup a) => Score a -> Score a
sine                  :: Floating a => Behavior a
splice                :: Behavior a -> Bound (Behavior a) -> Behavior a
splitAbs              :: (HasPosition a, Splittable a) => Time -> a -> (a, a)
splitReactive         :: Reactive a -> Either a ((a, Time), [Music.Time.Note.Note a], (Time, a))
startAt               :: (Transformable a, HasPosition a) => Time -> a -> a
startsBefore          :: Span -> Span -> Bool
startsLater           :: Span -> Span -> Bool
startsWhenStarts      :: Span -> Span -> Bool
startsWhenStops       :: Span -> Span -> Bool
stopAt                :: (Transformable a, HasPosition a) => Time -> a -> a
stopsAtTheSameTime    :: Span -> Span -> Bool
stopsBefore           :: Span -> Span -> Bool
stopsLater            :: Span -> Span -> Bool
stopsWhenStarts       :: Span -> Span -> Bool
stopsWhenStops        :: Span -> Span -> Bool
stretch               :: Transformable a => Duration -> a -> a
stretchComponent      :: Span -> Duration
stretchTo             :: (Transformable a, HasDuration a) => Duration -> a -> a
stretched             :: Iso (Duration, a) (Duration, b) (Stretched a) (Stretched b)
stretcheds            :: Lens (Voice a) (Voice b) [Stretched a] [Stretched b]
stretchee             :: (Transformable a, Transformable b) => Lens (Stretched a) (Stretched b) a b
stretching            :: Duration -> Span
strictlyAfterOffset   :: Time -> Span -> Bool
strictlyAfterOnset    :: Time -> Span -> Bool
strictlyBeforeOffset  :: Time -> Span -> Bool
strictlyBeforeOnset   :: Time -> Span -> Bool
sustain               :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
switch                :: Time -> Behavior a -> Behavior a -> Behavior a
switch'               :: Time -> Behavior a -> Behavior a -> Behavior a -> Behavior a
switchR               :: Time -> Reactive a -> Reactive a -> Reactive a
times                 :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => Int -> a -> a
toAbsoluteTime        :: [Duration] -> [Time]
toRelativeTime        :: [Time] -> [Duration]
toRelativeTimeN       :: [Time] -> [Duration]
toRelativeTimeN'      :: Time -> [Time] -> [Duration]
transformed           :: (Transformable a, Transformable b) => Span -> Iso a b a b
trim                  :: Monoid b => Bound (Behavior b) -> Behavior b
trimAfter             :: Monoid a => Time -> Behavior a -> Behavior a
trimBefore            :: Monoid a => Time -> Behavior a -> Behavior a
trimR                 :: Monoid a => Span -> Reactive a -> Reactive a
turnOff               :: Num a => Behavior a
turnOn                :: Num a => Behavior a
unchord               :: Lens (Chord a) (Chord b) [Delayed a] [Delayed b]
undelay               :: Transformable a => Duration -> a -> a
undelaying            :: Duration -> Span
unit                  :: Fractional a => Behavior a
unsafeChord           :: Iso (Chord a) (Chord b) [Delayed a] [Delayed b]
unsafeEvents          :: Iso (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
unsafeEventsV         :: Iso (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
unsafeNotes           :: Iso (Score a) (Score b) [Note a] [Note b]
unsafeStretcheds      :: Iso (Voice a) (Voice b) [Stretched a] [Stretched b]
unzipVoice            :: Voice (a, b) -> (Voice a, Voice b)
updates               :: Reactive a -> [(Time, a)]
valuesV               :: Functor f => ([a] -> f [b]) -> Voice a -> f (Voice b)
voice                 :: Getter [Stretched a] (Voice a)
voiceAsList           :: Iso (Voice a) (Voice b) [a] [b]
voiceLens             :: (s -> a) -> (b -> s -> t) -> Lens (Voice s) (Voice t) (Voice a) (Voice b)
withContext           :: Voice a -> Voice (Data.Functor.Context.Ctxt a)
withDurations         :: ([Duration] -> [Duration]) -> Voice a -> Voice a
withValues            :: ([a] -> [b]) -> Voice a -> Voice b
zipVoice              :: Voice a -> Voice b -> Voice (a, b)
zipVoice3             :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
zipVoice4             :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoiceNoScale       :: Voice a -> Voice b -> Voice (a, b)
zipVoiceNoScale3      :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
zipVoiceNoScale4      :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoiceWith          :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith'         :: (Duration -> Duration -> Duration) -> (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWithNoScale   :: (a -> b -> c) -> Voice a -> Voice b -> Voice c

