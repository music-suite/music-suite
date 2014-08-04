

sameDurations           :: Voice a -> Voice b -> Bool

mergeIfSameDuration     :: Voice a -> Voice b -> Maybe (Voice (a, b))
mergeIfSameDurationWith :: (a -> b -> c) -> Voice a -> Voice b -> Maybe (Voice c)

-- TODO relate to Splittable
splitAt :: 
splitAt :: [Duration] -> Voice a -> [Voice a]
-- This is a specification
-- splitTiesVoiceAt :: Tiable a => [Duration] -> Voice a -> [Voice a]

splitLatterToAssureSameDuration :: Voice b -> Voice b -> Voice b
splitLatterToAssureSameDurationWith :: (b -> (b, b)) -> Voice b -> Voice b -> Voice b

-- TODO more potential here!
-- Compare music21's Stream.chordify
polyToHomophonic      :: [Voice a] -> Maybe (Voice [a])
polyToHomophonicForce :: [Voice a] -> Voice [a]
homoToPolyphonic      :: Voice [a] -> [Voice a]
joinVoice             :: Voice (Voice a) -> a

changeCrossing   :: Ord a => Voice a -> Voice a -> (Voice a, Voice a)
changeCrossingBy :: Ord b => (a -> b) -> Voice a -> Voice a -> (Voice a, Voice a)
-- More generally
-- | If two voices have *exactly* overlapping notes, do something with them (i.e. swap them)
processExactOverlaps :: (a -> a -> (a, a)) -> Voice a -> Voice a -> (Voice a, Voice a)
processExactOverlaps' :: (a -> b -> Either (a,b) (b,a)) -> Voice a -> Voice b -> (Voice (Either b a), Voice (Either a b))

-- Position of each value
onsetsRelative    :: Time -> Voice a -> [Time]
offsetsRelative   :: Time -> Voice a -> [Time]
midpointsRelative :: Time -> Voice a -> [Time]
erasRelative      :: Time -> Voice a -> [Span]
onsetMap  :: Time -> Voice a -> Map Time a
offsetMap :: Time -> Voice a -> Map Time a
midpointMap :: Time -> Voice a -> Map Time a
eraMap :: Time -> Voice a -> Map Span a
-- TODO relates to Track (old "Score")
-- Note that eraMap for Track

durations :: Voice a -> [Duration]
values    :: Voice a -> [a] -- Same as Foldable.toList

isPossiblyInfinite :: Voice a -> Bool

hasMelodicDissonanceWith :: (a -> a -> Bool) -> Voice a -> Bool
hasIntervalWith :: AffineSpace a => (Diff a -> Bool) -> Voice a -> Bool
hasDurationWith :: (Duration -> Bool) -> Voice a -> Bool

reifyVoice :: Voice a -> Voice (Duration, a)
mapWithIndex :: (Int -> a -> b) -> Voice a -> Voice b
mapWithDuration :: (Duration -> a -> b) -> Voice a -> Voice b
mapWithIndexAndDuration :: (Int -> Duration -> a -> b) -> Voice a -> Voice b

-- Up to meta-data...
_ :: Iso (Voice ()) [Duration]
asingleton' :: Prism (Voice a) (Duration, a)
asingleton :: Prism (Voice a) a

-- More interesting for score...
separateVoicesWith :: (a -> k) -> Voice a -> Map k (Voice a)

-- List functions as voice functions for free
-- TODO this *only* works with fully polymorphic functions, so combinators where
-- user arguments can restrict the function (i.e. intersperse) does not work
freeVoiceR :: (forall a. -> [a] -> a)          -> Voice a -> (a, Duration)
-- head last null "length"
freeVoiceRNoDur :: ([a] -> a)          -> Voice a -> a
-- head last null "length"
-- sum product maximum minimum
-- TODO folds and scans?

freeVoice  :: (forall a. -> [a] -> [a])        -> Voice a -> Voice a
-- tail init reverse cycle take drop
freeVoice2 :: (forall a. -> [a] -> [a] -> [a]) -> Voice a -> Voice a -> Voice a

-- "special" lifted functions
empty :: Voice a
singleton :: a -> Voice a
cons :: a -> Voice a -> Voice a
snoc :: Voice a -> a -> Voice a
append :: Voice a -> Voice a -> Voice a
ap :: Voice (a -> b) -> Voice a -> Voice b
apDur :: Voice (Duration -> Duration -> a -> b) -> Voice a -> Voice b
-- TODO ap vs. zip
-- Which one is the instance?

intersperse :: Duration -> a -> Voice a -> Voice a
-- intercalate :: Voice a -> Voice (Voice a) -> Voice a
subsequences :: Voice a -> [Voice a]
permutations :: Voice a -> [Voice a]
iterate :: (a -> a) -> a -> Voice a
repeat :: a -> Voice a
replicate :: Int -> a -> Voice a
unfoldr :: (b -> Maybe (a, b)) -> b -> Voice a

-- Standard levels in a score
data ScoreLevel 
  = Score -- parallell composition of
  | Part  -- sequential composition of
  | Bar   -- sequential composition of
  | Chord -- parallel composition

-- Idea: API to "tag" a voice/chord with its level
-- Similar to music21 "groups"
    


{-

Differences between Voice and Chord (except the obviously different composition styles):
  - Voice is a Monoid, Chord just a Semigroup (??)
    - Rationale: We want to separate Note/Chord/Rest
  - Scores are always sorted (i.e. more like (multi)sets than lists) (??)
  - TODO is there Monad/Applicative for MultiSet/SortedList?

-}


{-
  - TODO represent spanners using (Voice a, Map (Int,Int) s)
  Arguably this should be part of Voice
-}

{-
  TODO the MVoice/TVoice stuff
-}
newtype MVoice = Voice (Maybe a)
newtype PVoice = [Either Duration (Voice a)]


{-
  TODO the context stuff

-}

{-
  TODO Zippers

-}



{-
  Inspired by music21 variants
-}
newtype Variant a = { _getVariant :: [a] }
instance Representable Variant where
  type Rep = Positive
  tabulate f = fmap f [1..]
  index v n = cycle v !! n' where n' = fromIntegral n
  -- OR
  -- index v n = v !! n `mod` (length n)

expandRepeats :: [Voice (Variant a)] -> Voice a


--------------

invert :: Transposable a => Chord a -> Chord a
inversions :: Transposable a => Chord a -> [Chord a]
chordToScore :: Chord a -> Score a
arpUp3 :: Chord a -> Score a
arpDown3 :: Chord a -> Score a
alberti3 :: Chord a -> Score a
majorTriad :: Transposable a => a -> Chord a
minorTriad :: Transposable a => a -> Chord a

-- What is a scale, chord, "sequence" etc?


----------------


data Division
divisions :: Int -> [Division]
getDivision :: Division -> (Int, Int)
data Subpart
data Instrument
= StdInstrument Int
| OtherInstrument String
data Solo
= Solo
| Tutti
data Part = Part Solo Instrument Subpart
divide :: Int -> Part -> [Part]
containsPart :: Part -> Part -> Bool
containsSubpart :: Subpart -> Subpart -> Bool
solo :: Instrument -> Part
tutti :: Instrument -> Part
piccoloFlute :: Instrument
flute :: Instrument
altoFlute :: Instrument
bassFlute :: Instrument
oboe :: Instrument
corAnglais :: Instrument
heckelphone :: Instrument
ebClarinet :: Instrument
clarinet :: Instrument
aClarinet :: Instrument
bassClarinet :: Instrument
sopranoSax :: Instrument
altoSax :: Instrument
tenorSax :: Instrument
baritoneSax :: Instrument
bassoon :: Instrument
contraBassoon :: Instrument
horn :: Instrument
piccoloTrumpet :: Instrument
trumpet :: Instrument
bassTrumpet :: Instrument
altoTrombone :: Instrument
tenorTrombone :: Instrument
trombone :: Instrument
bassTrombone :: Instrument
tuba :: Instrument
timpani :: Instrument
piano :: Instrument
celesta :: Instrument
glockenspiel :: Instrument
vibraphone :: Instrument
marimba :: Instrument
xylophone :: Instrument
xylorimba :: Instrument
tubularBells :: Instrument
dulcimer :: Instrument
accordion :: Instrument
harmonica :: Instrument
violin :: Instrument
viola :: Instrument
cello :: Instrument
doubleBass :: Instrument
piccoloFlutes :: Part
flutes :: Part
oboes :: Part
clarinets :: Part
bassoons :: Part
flutes1 :: Part
flutes2 :: Part
oboes1 :: Part
oboes2 :: Part
clarinets1 :: Part
clarinets2 :: Part
horns :: Part
highHorns :: [Part]
lowHorns :: [Part]
trumpets :: Part
trombones :: Part
trumpets1 :: Part
trumpets2 :: Part
trombones1 :: Part
trombones2 :: Part
tubas :: Part
violins :: Part
violins1 :: Part
violins2 :: Part
violas :: Part
cellos :: Part
doubleBasses :: Part
harp :: Part
defaultClef :: Part -> Int
defaultMidiProgram :: Part -> Int
defaultMidiChannel :: Part -> Int
defaultMidiNote :: Part -> Int
data BasicPart


                                                            

