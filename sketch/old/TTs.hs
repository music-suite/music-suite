data Segment a
-- Monad
segment             :: (Duration -> a) <~> (Segment a)
focusing            :: (Behavior a) ~> (Segment a)
apSegments          :: Voice (Segment a) -> Stretched (Segment a)

data Behavior a
-- Monad
behavior            :: (Time -> a) <~> (Behavior a)
switch              :: Time -> Behavior a -> Behavior a -> Behavior a
switch'             :: Time -> Behavior a -> Behavior a -> Behavior a -> Behavior a
splice              :: Behavior a -> Bound (Behavior a) -> Behavior a
trim                :: Monoid b => Bound (Behavior b) -> Behavior b
trimBefore          :: Monoid a => Time -> Behavior a -> Behavior a
trimAfter           :: Monoid a => Time -> Behavior a -> Behavior a
concatB             :: Monoid a => Score (Behavior a) -> Behavior a
-- time      :: Fractional a => Behavior a
-- unit      :: Fractional a => Behavior a
-- impulse   :: Num a => Behavior a
-- turnOn    :: Num a => Behavior a
-- turnOff   :: Num a => Behavior a
-- sawtooth  :: RealFrac a => Behavior a
-- sine      :: Floating a => Behavior a
-- cosine    :: Floating a => Behavior a

data Reactive a
-- Applicative
initial       :: Reactive a -> a
final         :: Reactive a -> a
intermediate  :: Transformable a => Reactive a -> [Note a]
swithR        :: Time -> Reactive a -> Reactive a -> Reactive a
discrete      :: Reactive a -> Behavior a
continous     :: Reactive (Segment a) -> Behavior a
continousWith :: Segment (a -> b) -> Reactive a -> Behavior b
sample        :: [Time] -> Behavior a -> Reactive a


data Bound a
-- Functor
bounds        :: Time -> Time -> a -> Bound a
bounding      :: Span -> a -> Bound a
bounded'      :: <~>' (Note (Segment a)) (Bound (Behavior a))
bounded       :: <~> (Note (Segment a)) (Note (Segment b)) (Bound (Behavior a)) (Bound (Behavior b))


















data Delayed a
-- Monad, Comonad
delayed               :: (Time, a) <~> (Delayed b)
getDelayed            :: Transformable a => (Delayed a) ~> a

data Stretched a
-- Monad, Comonad
stretched             :: (Duration, a) <~> (Stretched a)
getStretched          :: Transformable a => (Stretched b) ~> a

data Note a
-- Monad, Comonad
note                  :: Transformable a => (Span, a) <~> (Note a)
getNote               :: Transformable a => (Note a) ~> a

data Chord a
chord                 :: [(Position, a)] <~> (Chord a) -- Position = Duration (locally)

data Track a
-- MonadPlus
track                 :: [Delayed a] ~>> (Track a)
getTrack              :: (Track a) ~> [Delayed a]


data Voice a
-- MonadPlus
voice                 :: [Stretched a] ~>> (Voice a)
getVoice              :: (Voice a) ~> [Stretched a]
singleStretched       :: (Voice a) >~ (Stretched a)

zipVoice            :: Voice a -> Voice b -> Voice (a, b)
zipVoiceWith        :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
dzipVoiceWith       :: (Duration -> Duration -> a -> b -> (Duration, c)) -> Voice a -> Voice b -> Voice c
-- withPrev etc
mergeVoice          :: Eq a => Voice a -> Voice a


data Score a
-- MonadPlus
score               :: [Note a] ~>> (Score a)
notes               :: (Score a) ~> [Note a]
singleNote          :: (Score a) >~ (Note a)
singleVoice         :: (Score a) >~ (Voice a)

-- ?????
simultaneous        :: Semigroup a => Score a -> Score a
simultaneous'       :: Score a -> Score (NonEmpty a)
parts               :: HasPart a a => ~> (Score a) (Score b) [(Part a, Score a)] [(Part a, Score a)]
singlePart          :: HasPart a a => >~' (Score a) (Voice a)






-- Ignoring meta:
range ::                Span      <-> (Time, Time)
delta ::                Span      <-> (Time, Duration)
note  ::                (Span, a) <-> Note a
notes ::                Score a   <-> [Note a]
parts :: HasPart a' =>  Score a   <-> [(Part a, Score a)]

Score a ~> [[((Time, Time), a)]]
















-- IndexedTraversal stuff:
-- mapWithSpan         :: (Span -> a -> b) -> Score a -> Score b
-- filterWithSpan      :: (Span -> a -> Bool) -> Score a -> Score a
-- mapFilterWithSpan   :: (Span -> a -> Maybe b) -> Score a -> Score b
-- mapEvents           :: (Time -> Duration -> a -> b) -> Score a -> Score b
-- filterEvents        :: (Time -> Duration -> a -> Bool) -> Score a -> Score a
-- mapFilterEvents     :: (Time -> Duration -> a -> Maybe b) -> Score a -> Score b


