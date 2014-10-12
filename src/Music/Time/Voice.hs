
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Time.Voice (

        -- * Voice type
        Voice,

        -- * Construction
        voice,

        -- ** Extracting values
        stretcheds,
        eventsV,
        singleStretched,

        -- * Fusion
        fuse,
        fuseBy,

        -- ** Fuse rests
        fuseRests,
        coverRests,

        -- * Traversal
        -- ** Separating rhythms and values
        valuesV,
        durationsV,
        withValues,
        withDurations,
        rotateValues,
        rotateDurations,
        reverseValues,
        reverseDurations,

        -- ** Zips
        unzipVoice,
        zipVoice,
        zipVoice3,
        zipVoice4,
        zipVoiceNoScale,
        -- FIXME compose with (lens assoc unassoc) for the 3 and 4 versions
        zipVoiceNoScale3,
        zipVoiceNoScale4,
        zipVoiceWith,
        zipVoiceWith',
        zipVoiceWithNoScale,

        -- * Context
        withContext,
        voiceLens,
        -- voiceL,
        voiceAsList,
        listAsVoice,

        -- * Unsafe versions
        unsafeStretcheds,
        unsafeEventsV,

  ) where

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Functor.Adjunction  (unzipR)
import           Data.Functor.Context
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Ratio
import           Data.Semigroup
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq
import           Data.String
import           Data.VectorSpace

import           Music.Time.Reverse
import           Music.Time.Split
import           Music.Time.Stretched

import           Control.Applicative
import           Control.Lens             hiding (Indexable, Level, above,
                                           below, index, inside, parts,
                                           reversed, transform, (<|), (|>))
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.Foldable            (Foldable)
import qualified Data.Foldable            as Foldable
import qualified Data.List
import           Data.List.NonEmpty       (NonEmpty)
import           Data.Traversable         (Traversable)
import qualified Data.Traversable         as T
import           Data.Typeable
import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Internal.Util

-- |
-- A 'Voice' is a sequential composition of values. Events may not overlap.
--
newtype Voice a = Voice { getVoice :: VoiceList (VoiceEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Eq)

instance (Show a, Transformable a) => Show (Voice a) where
  show x = show (x^.stretcheds) ++ "^.voice"

--
-- $semantics Voice
-- type Voice a = [Stretched a]
--

-- A voice is a list of events with explicit duration. Events can not overlap.
--
-- Voice is a 'Monoid' under sequential composition. 'mempty' is the empty part and 'mappend'
-- appends parts.
--
-- Voice is a 'Monad'. 'return' creates a part containing a single value of duration
-- one, and '>>=' transforms the values of a part, allowing the addition and
-- removal of values under relative duration. Perhaps more intuitively, 'join' scales
-- each inner part to the duration of the outer part, then removes the
-- intermediate structure.

-- Can use [] or Seq here
type VoiceList = []

-- Can use any type as long as voiceEv provides an Iso
type VoiceEv a = Stretched a
-- type VoiceEv a = ((),Stretched a)

voiceEv :: Iso (Stretched a) (Stretched b) (VoiceEv a) (VoiceEv b)
voiceEv = id
-- voiceEv = iso add remove
--   where
--     add x = ((),x)
--     remove ((),x) = x

instance Applicative Voice where
  pure  = return
  (<*>) = ap

instance Alternative Voice where
  (<|>) = (<>)
  empty = mempty

instance Monad Voice where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

instance MonadPlus Voice where
  mzero = mempty
  mplus = mappend
  
instance Wrapped (Voice a) where
  type Unwrapped (Voice a) = (VoiceList (VoiceEv a))
  _Wrapped' = iso getVoice Voice

instance Rewrapped (Voice a) (Voice b)

instance Cons (Voice a) (Voice b) (Stretched a) (Stretched b) where
  _Cons = prism (\(s,v) -> (view voice.return $ s) <> v) $ \v -> case view stretcheds v of
    []      -> Left  mempty
    (x:xs)  -> Right (x, view voice xs)

instance Snoc (Voice a) (Voice b) (Stretched a) (Stretched b) where
  _Snoc = prism (\(v,s) -> v <> (view voice.return $ s)) $ \v -> case unsnoc (view stretcheds v) of
    Nothing      -> Left  mempty
    Just (xs, x) -> Right (view voice xs, x)

instance Transformable (Voice a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Voice a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

instance Splittable a => Splittable (Voice a) where
  split t x
    | t <= 0           = (mempty, x)
    | t >= _duration x = (x,      mempty)
    | otherwise        = let (a,b) = split' t {-split-} (x^._Wrapped) in (a^._Unwrapped, b^._Unwrapped)
    where
      split' = error "TODO"

instance Reversible a => Reversible (Voice a) where
  rev = over _Wrapped' (fmap rev) -- TODO OK?


-- Lifted instances

instance IsString a => IsString (Voice a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Voice a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Voice a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Voice a) where
  fromDynamics = pure . fromDynamics

-- Bogus instance, so we can use [c..g] expressions
instance Enum a => Enum (Voice a) where
  toEnum = return . toEnum
  fromEnum = list 0 (fromEnum . head) . Foldable.toList

-- Bogus instance, so we can use numeric literals
instance Num a => Num (Voice a) where
  fromInteger = return . fromInteger
  abs    = fmap abs
  signum = fmap signum
  (+)    = error "Not implemented"
  (-)    = error "Not implemented"
  (*)    = error "Not implemented"

-- Bogus instances, so we can use c^*2 etc.
instance AdditiveGroup (Voice a) where
  zeroV   = error "Not implemented"
  (^+^)   = error "Not implemented"
  negateV = error "Not implemented"

instance VectorSpace (Voice a) where
  type Scalar (Voice a) = Duration
  d *^ s = d `stretch` s


-- |
-- Create a 'Voice' from a list of 'Stretched' values.
--
-- This is a 'Getter' (rather than a function) for consistency:
--
-- @
-- [ (0 '<->' 1, 10)^.'stretched',
--   (1 '<->' 2, 20)^.'stretched',
--   (3 '<->' 4, 30)^.'stretched' ]^.'voice'
-- @
--
-- @
-- 'view' 'voice' $ 'map' ('view' 'stretched') [(0 '<->' 1, 1)]
-- @
--
-- Se also 'stretcheds'.
--
voice :: Getter [Stretched a] (Voice a)
voice = from unsafeStretcheds
-- voice = to $ flip (set stretcheds) empty
{-# INLINE voice #-}

-- |
-- View a 'Voice' as a list of 'Stretched' values.
--
-- @
-- 'view' 'stretcheds'                        :: 'Voice' a -> ['Stretched' a]
-- 'set'  'stretcheds'                        :: ['Stretched' a] -> 'Voice' a -> 'Voice' a
-- 'over' 'stretcheds'                        :: (['Stretched' a] -> ['Stretched' b]) -> 'Voice' a -> 'Voice' b
-- @
--
-- @
-- 'preview'  ('stretcheds' . 'each')           :: 'Voice' a -> 'Maybe' ('Stretched' a)
-- 'preview'  ('stretcheds' . 'element' 1)      :: 'Voice' a -> 'Maybe' ('Stretched' a)
-- 'preview'  ('stretcheds' . 'elements' odd)   :: 'Voice' a -> 'Maybe' ('Stretched' a)
-- @
--
-- @
-- 'set'      ('stretcheds' . 'each')           :: 'Stretched' a -> 'Voice' a -> 'Voice' a
-- 'set'      ('stretcheds' . 'element' 1)      :: 'Stretched' a -> 'Voice' a -> 'Voice' a
-- 'set'      ('stretcheds' . 'elements' odd)   :: 'Stretched' a -> 'Voice' a -> 'Voice' a
-- @
--
-- @
-- 'over'     ('stretcheds' . 'each')           :: ('Stretched' a -> 'Stretched' b) -> 'Voice' a -> 'Voice' b
-- 'over'     ('stretcheds' . 'element' 1)      :: ('Stretched' a -> 'Stretched' a) -> 'Voice' a -> 'Voice' a
-- 'over'     ('stretcheds' . 'elements' odd)   :: ('Stretched' a -> 'Stretched' a) -> 'Voice' a -> 'Voice' a
-- @
--
-- @
-- 'toListOf' ('stretcheds' . 'each')                :: 'Voice' a -> ['Stretched' a]
-- 'toListOf' ('stretcheds' . 'elements' odd)        :: 'Voice' a -> ['Stretched' a]
-- 'toListOf' ('stretcheds' . 'each' . 'filtered'
--              (\\x -> '_duration' x \< 2))  :: 'Voice' a -> ['Stretched' a]
-- @
--
-- This is not an 'Iso', as the note list representation does not contain meta-data.
-- To construct a score from a note list, use 'score' or @'flip' ('set' 'stretcheds') 'empty'@.
--
stretcheds :: Lens (Voice a) (Voice b) [Stretched a] [Stretched b]
stretcheds = unsafeStretcheds
{-# INLINE stretcheds #-}

eventsV :: Lens (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
eventsV = unsafeEventsV
{-# INLINE eventsV #-}

unsafeEventsV :: Iso (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
unsafeEventsV = iso (map (^.from stretched) . (^.stretcheds)) ((^.voice) . map (^.stretched))
{-# INLINE unsafeEventsV #-}

unsafeStretcheds :: Iso (Voice a) (Voice b) [Stretched a] [Stretched b]
unsafeStretcheds = _Wrapped
{-# INLINE unsafeStretcheds #-}

singleStretched :: Prism' (Voice a) (Stretched a)
singleStretched = unsafeStretcheds . single
{-# INLINE singleStretched #-}
{-# DEPRECATED singleStretched "Use 'unsafeStretcheds . single'" #-}


-- |
-- Unzip the given voice. This is specialization of 'unzipR'.
--
unzipVoice :: Voice (a, b) -> (Voice a, Voice b)
unzipVoice = unzipR

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice :: Voice a -> Voice b -> Voice (a, b)
zipVoice = zipVoiceWith (,)

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice3 :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
zipVoice3 a b c = zipVoice a (zipVoice b c)

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice4 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoice4 a b c d = zipVoice a (zipVoice b (zipVoice c d))

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice5 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice e -> Voice (a, (b, (c, (d, e))))
zipVoice5 a b c d e = zipVoice a (zipVoice b (zipVoice c (zipVoice d e)))

-- |
-- Join the given voices by pairing values and selecting the first duration.
--
zipVoiceNoScale :: Voice a -> Voice b -> Voice (a, b)
zipVoiceNoScale = zipVoiceWithNoScale (,)

-- |
-- Join the given voices by pairing values and selecting the first duration.
--
zipVoiceNoScale3 :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
zipVoiceNoScale3 a b c = zipVoiceNoScale a (zipVoiceNoScale b c)

-- |
-- Join the given voices by pairing values and selecting the first duration.
--
zipVoiceNoScale4 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoiceNoScale4 a b c d = zipVoiceNoScale a (zipVoiceNoScale b (zipVoiceNoScale c d))

-- |
-- Join the given voices by pairing values and selecting the first duration.
--
zipVoiceNoScale5 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice e -> Voice (a, (b, (c, (d, e))))
zipVoiceNoScale5 a b c d e = zipVoiceNoScale a (zipVoiceNoScale b (zipVoiceNoScale c (zipVoiceNoScale d e)))


-- |
-- Join the given voices by multiplying durations and combining values using the given function.
--
zipVoiceWith :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith = zipVoiceWith' (*)

-- |
-- Join the given voices without combining durations.
--
zipVoiceWithNoScale :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWithNoScale = zipVoiceWith' const

-- |
-- Join the given voices by combining durations and values using the given function.
--
zipVoiceWith' :: (Duration -> Duration -> Duration) -> (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith' f g
  ((unzip.view eventsV) -> (ad, as))
  ((unzip.view eventsV) -> (bd, bs))
  = let cd = zipWith f ad bd
        cs = zipWith g as bs
     in view (from unsafeEventsV) (zip cd cs)

-- |
-- Merge consecutive equal notes.
--
fuse :: Eq a => Voice a -> Voice a
fuse = fuseBy (==)

-- |
-- Merge consecutive equal notes using the given function.
--
fuseBy :: (a -> a -> Bool) -> Voice a -> Voice a
fuseBy p = fuseBy' p head

-- |
-- Merge consecutive equal notes using the given equality predicate and merge function.
--
fuseBy' :: (a -> a -> Bool) -> ([a] -> a) -> Voice a -> Voice a
fuseBy' p g = over unsafeEventsV $ fmap foldNotes . Data.List.groupBy (inspectingBy snd p)
  where
    -- Add up durations and use a custom function to combine notes
    --
    -- Typically, the combination function us just 'head', as we know that group returns
    -- non-empty lists of equal elements.
    foldNotes (unzip -> (ds, as)) = (sum ds, g as)

-- | 
-- Fuse all rests in the given voice. The resulting voice will have no consecutive rests.
--
fuseRests :: Voice (Maybe a) -> Voice (Maybe a)
fuseRests = fuseBy (\x y -> isNothing x && isNothing y)

-- | 
-- Remove all rests in the given voice by prolonging the previous note. Returns 'Nothing'
-- if and only if the given voice contains rests only.
--
coverRests :: Voice (Maybe a) -> Maybe (Voice a)
coverRests x = if hasOnlyRests then Nothing else Just (fmap fromJust $ fuseBy merge x)
  where
    norm = fuseRests x
    merge Nothing  Nothing  = error "Voice normalized, so consecutive rests are impossible"
    merge (Just x) Nothing  = True
    merge Nothing  (Just x) = True
    merge (Just x) (Just y) = False
    hasOnlyRests = all isNothing $ toListOf traverse x -- norm


withContext :: Voice a -> Voice (Ctxt a)
withContext = over valuesV (fmap Ctxt . withPrevNext)

-- TODO expose?
voiceFromRhythm :: [Duration] -> Voice ()
voiceFromRhythm = mkVoice . fmap (, ())

mkVoice = view voice . fmap (view stretched)

--
-- TODO more elegant definition of durationsV and valuesV using indexed traversal or similar?
--

durationsV :: Lens' (Voice a) [Duration]
durationsV = lens getDurs (flip setDurs)
  where
    getDurs :: Voice a -> [Duration]
    getDurs = map fst . view eventsV

    setDurs :: [Duration] -> Voice a -> Voice a
    setDurs ds as = zipVoiceWith' (\a b -> a) (\a b -> b) (mconcat $ map durToVoice ds) as

    durToVoice d = stretch d $ pure ()

valuesV :: Lens (Voice a) (Voice b) [a] [b]
valuesV = lens getValues (flip setValues)
  where
    -- getValues :: Voice a -> [a]
    getValues = map snd . view eventsV

    -- setValues :: [a] -> Voice b -> Voice a
    setValues as bs = zipVoiceWith' (\a b -> b) (\a b -> a) (listToVoice as) bs

    listToVoice = mconcat . map pure

-- |
-- Transform the durations, leaving values intact.
withDurations :: ([Duration] -> [Duration]) -> Voice a -> Voice a
withDurations = over durationsV

-- |
-- Transform the values, leaving durations intact.
withValues :: ([a] -> [b]) -> Voice a -> Voice b
withValues = over valuesV

-- |
-- Rotate durations by the given number of steps, leaving values intact.
--
rotateDurations :: Int -> Voice a -> Voice a
rotateDurations n = over durationsV (rotate n)

-- |
-- Rotate values by the given number of steps, leaving durations intact.
--
rotateValues :: Int -> Voice a -> Voice a
rotateValues n = over valuesV (rotate n)

-- |
-- Reverse durations, leaving values intact.
--
reverseDurations :: Voice a -> Voice a
reverseDurations = over durationsV reverse

-- |
-- Reverse values, leaving durations intact.
--
reverseValues :: Voice a -> Voice a
reverseValues = over valuesV reverse

-- Lens "filtered" through a voice
voiceLens :: (s -> a) -> (b -> s -> t) -> Lens (Voice s) (Voice t) (Voice a) (Voice b)
voiceLens getter setter = lens (fmap getter) (flip $ zipVoiceWithNoScale setter)

-- TODO generalize to any zippable thing
-- voiceL :: ALens s t a b -> Lens (Voice s) (Voice t) (Voice a) (Voice b)
voiceL l = voiceLens (view $ cloneLens l) (set $ cloneLens l)


-- TODO not meta-safe
voiceAsList :: Iso (Voice a) (Voice b) [a] [b]
voiceAsList = iso voiceToList listToVoice
  where
    voiceToList = map snd . view eventsV
    listToVoice = mconcat . fmap pure

listAsVoice :: Iso [a] [b] (Voice a) (Voice b)
listAsVoice = from voiceAsList


--
-- TODO
-- Implement meta-data
--

-- List functions
headV, lastV :: Voice a -> Maybe (Stretched a)
headV = preview _head
lastV = preview _head

tailV, initV :: Voice a -> Maybe (Voice a)
tailV = preview _tail
initV = preview _init

consV :: Stretched a -> Voice a -> Voice a
unconsV :: Voice a -> Maybe (Stretched a, Voice a)
consV = cons
unconsV = uncons

snocV :: Voice a -> Stretched a -> Voice a
unsnocV :: Voice a -> Maybe (Voice a, Stretched a)
snocV = snoc
unsnocV = unsnoc

nullV :: Voice a -> Bool
nullV = nullOf eventsV

lengthV :: Voice a -> Int
lengthV = lengthOf eventsV

mapV :: (a -> b) -> Voice a -> Voice b
mapV = fmap


-- Voice-specific

sameDurations :: Voice a -> Voice b -> Bool
sameDurations a b = view durationsV a == view durationsV b

mergeIfSameDuration :: Voice a -> Voice b -> Maybe (Voice (a, b))
mergeIfSameDuration = mergeIfSameDurationWith (,)

mergeIfSameDurationWith :: (a -> b -> c) -> Voice a -> Voice b -> Maybe (Voice c)
mergeIfSameDurationWith f a b
  | sameDurations a b = Just $ zipVoiceWithNoScale f a b
  | otherwise         = Nothing

-- splitAt :: [Duration] -> Voice a -> [Voice a]
-- splitTiesAt :: Tiable a => [Duration] -> Voice a -> [Voice a]

-- |
-- Split all notes of the latter voice at the onset/offset of the former.
--
-- >>> ["a",(2,"b")^.stretched,"c"]^.voice
-- [(1,"a")^.stretched,(2,"b")^.stretched,(1,"c")^.stretched]^.voice
--
splitLatterToAssureSameDuration :: Voice b -> Voice b -> Voice b
splitLatterToAssureSameDuration = splitLatterToAssureSameDurationWith dup
  where
    dup x = (x,x)

splitLatterToAssureSameDurationWith :: (b -> (b, b)) -> Voice b -> Voice b -> Voice b
splitLatterToAssureSameDurationWith = undefined

polyToHomophonic      :: [Voice a] -> Maybe (Voice [a])
polyToHomophonic = undefined

polyToHomophonicForce :: [Voice a] -> Voice [a]
polyToHomophonicForce = undefined

homoToPolyphonic      :: Voice [a] -> [Voice a]
homoToPolyphonic = undefined

changeCrossing   :: Ord a => Voice a -> Voice a -> (Voice a, Voice a)
changeCrossing = undefined

changeCrossingBy :: Ord b => (a -> b) -> Voice a -> Voice a -> (Voice a, Voice a)
changeCrossingBy = undefined

processExactOverlaps :: (a -> a -> (a, a)) -> Voice a -> Voice a -> (Voice a, Voice a)
processExactOverlaps = undefined

processExactOverlaps' :: (a -> b -> Either (a,b) (b,a)) -> Voice a -> Voice b -> (Voice (Either b a), Voice (Either a b))
processExactOverlaps' = undefined

onsetsRelative    :: Time -> Voice a -> [Time]
onsetsRelative = undefined

offsetsRelative   :: Time -> Voice a -> [Time]
offsetsRelative = undefined

midpointsRelative :: Time -> Voice a -> [Time]
midpointsRelative = undefined

erasRelative      :: Time -> Voice a -> [Span]
erasRelative = undefined

onsetMap  :: Time -> Voice a -> Map Time a
onsetMap = undefined

offsetMap :: Time -> Voice a -> Map Time a
offsetMap = undefined

midpointMap :: Time -> Voice a -> Map Time a
midpointMap = undefined

eraMap :: Time -> Voice a -> Map Span a
eraMap = undefined

durations :: Voice a -> [Duration]
durations = undefined

-- values :: Voice a -> [a] -- Same as Foldable.toList
-- values = undefined



{-

sameDurations           :: Voice a -> Voice b -> Bool
mergeIfSameDuration     :: Voice a -> Voice b -> Maybe (Voice (a, b))
mergeIfSameDurationWith :: (a -> b -> c) -> Voice a -> Voice b -> Maybe (Voice c)
splitAt :: [Duration] -> Voice a -> [Voice a]
-- splitTiesAt :: Tiable a => [Duration] -> Voice a -> [Voice a]
splitLatterToAssureSameDuration :: Voice b -> Voice b -> Voice b
splitLatterToAssureSameDurationWith :: (b -> (b, b)) -> Voice b -> Voice b -> Voice b
polyToHomophonic      :: [Voice a] -> Maybe (Voice [a])
polyToHomophonicForce :: [Voice a] -> Voice [a]
homoToPolyphonic      :: Voice [a] -> [Voice a]
joinVoice             :: Voice (Voice a) -> a
changeCrossing   :: Ord a => Voice a -> Voice a -> (Voice a, Voice a)
changeCrossingBy :: Ord b => (a -> b) -> Voice a -> Voice a -> (Voice a, Voice a)
processExactOverlaps :: (a -> a -> (a, a)) -> Voice a -> Voice a -> (Voice a, Voice a)
processExactOverlaps' :: (a -> b -> Either (a,b) (b,a)) -> Voice a -> Voice b -> (Voice (Either b a), Voice (Either a b))
onsetsRelative    :: Time -> Voice a -> [Time]
offsetsRelative   :: Time -> Voice a -> [Time]
midpointsRelative :: Time -> Voice a -> [Time]
erasRelative      :: Time -> Voice a -> [Span]
onsetMap  :: Time -> Voice a -> Map Time a
offsetMap :: Time -> Voice a -> Map Time a
midpointMap :: Time -> Voice a -> Map Time a
eraMap :: Time -> Voice a -> Map Span a
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
_ :: Iso (Voice ()) [Duration]
asingleton' :: Prism (Voice a) (Duration, a)
asingleton :: Prism (Voice a) a
separateVoicesWith :: (a -> k) -> Voice a -> Map k (Voice a)
freeVoiceR :: (forall a. -> [a] -> a)          -> Voice a -> (a, Duration)
freeVoiceRNoDur :: ([a] -> a)          -> Voice a -> a
freeVoice  :: (forall a. -> [a] -> [a])        -> Voice a -> Voice a
freeVoice2 :: (forall a. -> [a] -> [a] -> [a]) -> Voice a -> Voice a -> Voice a
empty :: Voice a
singleton :: a -> Voice a
cons :: a -> Voice a -> Voice a
snoc :: Voice a -> a -> Voice a
append :: Voice a -> Voice a -> Voice a
ap :: Voice (a -> b) -> Voice a -> Voice b
apDur :: Voice (Duration -> Duration -> a -> b) -> Voice a -> Voice b
intersperse :: Duration -> a -> Voice a -> Voice a
-- intercalate :: Voice a -> Voice (Voice a) -> Voice a
subsequences :: Voice a -> [Voice a]
permutations :: Voice a -> [Voice a]
iterate :: (a -> a) -> a -> Voice a
repeat :: a -> Voice a
replicate :: Int -> a -> Voice a
unfoldr :: (b -> Maybe (a, b)) -> b -> Voice a
Differences between Voice and Chord (except the obviously different composition styles):
  - Voice is a Monoid, Chord just a Semigroup (??)
  - TODO represent spanners using (Voice a, Map (Int,Int) s)
  Arguably this should be part of Voice
  TODO the MVoice/TVoice stuff
newtype MVoice = Voice (Maybe a)
newtype PVoice = [Either Duration (Voice a)]
expandRepeats :: [Voice (Variant a)] -> Voice a

-}

