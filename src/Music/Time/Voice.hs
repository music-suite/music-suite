
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
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
        notes,
        pairs,
        durationsVoice,

        -- * Traversal
        -- ** Separating rhythms and values
        valuesV,
        durationsV,

        -- ** Zips
        unzipVoice,
        zipVoiceScale,
        zipVoiceScale3,
        zipVoiceScale4,
        zipVoiceNoScale,
        -- FIXME compose with (lens assoc unassoc) for the 3 and 4 versions
        zipVoiceNoScale3,
        zipVoiceNoScale4,
        zipVoiceScaleWith,
        zipVoiceScaleWith',
        zipVoiceWithNoScale,

        -- * Fusion
        fuse,
        fuseBy,

        -- ** Fuse rests
        fuseRests,
        coverRests,

        -- * Homophonic/Polyphonic texture
        sameDurations,
        mergeIfSameDuration,
        mergeIfSameDurationWith,
        homoToPolyphonic,

        -- * Points in a voice
        onsetsRelative,
        offsetsRelative,
        midpointsRelative,
        erasRelative,

        -- * Context
        -- TODO clean
        withContext,
        -- voiceLens,

        -- * Unsafe versions
        unsafeNotes,
        unsafePairs,

  ) where

import           Control.Applicative
import           Control.Lens             hiding (Indexable, Level, above,
                                           below, index, inside, parts,
                                           reversed, transform, (<|), (|>))
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Foldable            (Foldable)
import           Data.Functor.Adjunction  (unzipR)
import           Data.Functor.Context
import           Data.List.NonEmpty       (NonEmpty)
import           Data.Maybe
import           Data.Semigroup
import           Data.Sequence            (Seq)
import           Data.Set                 (Set)
import           Data.String
import           Data.Traversable         (Traversable)
import           Data.Typeable            (Typeable)
import           Data.VectorSpace
import           Data.Aeson                    (ToJSON (..))
import qualified Data.Aeson                    as JSON

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Internal.Util
import           Music.Time.Juxtapose
import           Music.Time.Note

import qualified Data.List
import qualified Data.Foldable
import qualified Data.Either

-- |
-- A 'Voice' is a sequential composition of non-overlapping note values.
--
-- Both 'Voice' and 'Note' have duration but no position. The difference
-- is that 'Note' sustains a single value throughout its duration, while
-- a voice may contain multiple values. It is called voice because it is
-- generalizes the notation of a voice in choral or multi-part instrumental music.
--
-- It may be useful to think about 'Voice' and 'Note' as vectors in time space
-- (i.e. 'Duration'), that also happens to carry around other values, such as pitches.
--
newtype Voice a = Voice { getVoice :: [Note a] }
  deriving (Eq, Ord, Typeable, Foldable, Traversable, Functor, Semigroup, Monoid)

instance (Show a, Transformable a) => Show (Voice a) where
  show x = show (x^.notes) ++ "^.voice"

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
  type Unwrapped (Voice a) = [Note a]
  _Wrapped' = iso getVoice Voice

instance Rewrapped (Voice a) (Voice b)

instance Cons (Voice a) (Voice b) (Note a) (Note b) where
  _Cons = prism (\(s,v) -> (view voice.return $ s) <> v) $ \v -> case view notes v of
    []      -> Left  mempty
    (x:xs)  -> Right (x, view voice xs)

instance Snoc (Voice a) (Voice b) (Note a) (Note b) where
  _Snoc = prism (\(v,s) -> v <> (view voice.return $ s)) $ \v -> case unsnoc (view notes v) of
    Nothing      -> Left  mempty
    Just (xs, x) -> Right (view voice xs, x)

instance ToJSON a => ToJSON (Voice a) where
  -- TODO meta
  toJSON x = JSON.object [ ("notes", toJSON ns) ]
    where
      ns = x^.notes
      
instance Transformable (Voice a) where
  transform s = over notes (transform s)

instance HasDuration (Voice a) where
  _duration = sumOf (notes . each . duration)

instance Reversible a => Reversible (Voice a) where
  rev = over notes reverse . fmap rev

instance (Transformable a, Splittable a) => Splittable (Voice a) where
  -- TODO meta
  split d v = case splitNotes d (v^.notes) of
    (as,Nothing,cs)     -> (as^.voice, cs^.voice)
    (as,Just(b1,b2),cs) -> (as^.voice `snoc` b1, b2 `cons` cs^.voice)

splitNotes :: (Transformable a, Splittable a) => Duration -> [a] -> ([a], Maybe (a, a), [a])
splitNotes d xs = case (durAndNumNotesToFirst, needSplit) of
  (Just (_,0),_)     -> ([],Nothing,xs)
  (Nothing   ,False) -> (xs,Nothing,[])
  (Just (_,n),False) -> (take n xs,Nothing,drop n xs)
  (Nothing   ,True)  -> (init xs,Just (splitEnd (sum (fmap (^.duration) xs) - d) (last xs)),[])
  (Just (d',n),True) -> (
    take (n-1) xs
   ,Just (splitEnd (d'-d) (xs!!pred n)) -- (d'-d) is how much we have to cut
    ,drop n xs)
  where
    needSplit = case durAndNumNotesToFirst of
        Nothing     -> d < sum (fmap (^.duration) xs)
        Just (d',_) -> d /= d'
    -- Given dur is >= requested dur
    -- Nothing means all goes to first
    durAndNumNotesToFirst = accumUntil (\(ds,ns) x -> if ds < d then Left(ds+x,ns+1) else Right (ds,ns))
      (0,0) (fmap (^.duration) xs)
    splitEnd d x = split ((x^.duration) - d) x

    -- >>> accumUntil (\s a -> if s < 345 then Left (s + a) else Right s) 0 [1..]
    -- Just 351
    accumUntil :: (s -> a -> Either s b) -> s -> [a] -> Maybe b
    accumUntil f z xs = Data.Maybe.listToMaybe $ fmap fromRight $ dropWhile Data.Either.isLeft $ scanl (f . fromLeft) (Left z) xs
        where
          fromRight (Right x) = x
          fromLeft (Left x) = x

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
  fromEnum = list 0 (fromEnum . head) . Data.Foldable.toList

instance Num a => Num (Voice a) where
  fromInteger = return . fromInteger
  abs    = fmap abs
  signum = fmap signum
  (+)    = liftA2 (+)
  (-)    = liftA2 (-)
  (*)    = liftA2 (*)

-- | Create a 'Voice' from a list of 'Note's.
voice :: Getter [Note a] (Voice a)
voice = from unsafeNotes
{-# INLINE voice #-}

-- | View a 'Voice' as a list of 'Note' values.
notes :: Lens (Voice a) (Voice b) [Note a] [Note b]
notes = unsafeNotes

--
-- @
-- 'view' 'notes'                        :: 'Voice' a -> ['Note' a]
-- 'set'  'notes'                        :: ['Note' a] -> 'Voice' a -> 'Voice' a
-- 'over' 'notes'                        :: (['Note' a] -> ['Note' b]) -> 'Voice' a -> 'Voice' b
-- @
--
-- @
-- 'preview'  ('notes' . 'each')           :: 'Voice' a -> 'Maybe' ('Note' a)
-- 'preview'  ('notes' . 'element' 1)      :: 'Voice' a -> 'Maybe' ('Note' a)
-- 'preview'  ('notes' . 'elements' odd)   :: 'Voice' a -> 'Maybe' ('Note' a)
-- @
--
-- @
-- 'set'      ('notes' . 'each')           :: 'Note' a -> 'Voice' a -> 'Voice' a
-- 'set'      ('notes' . 'element' 1)      :: 'Note' a -> 'Voice' a -> 'Voice' a
-- 'set'      ('notes' . 'elements' odd)   :: 'Note' a -> 'Voice' a -> 'Voice' a
-- @
--
-- @
-- 'over'     ('notes' . 'each')           :: ('Note' a -> 'Note' b) -> 'Voice' a -> 'Voice' b
-- 'over'     ('notes' . 'element' 1)      :: ('Note' a -> 'Note' a) -> 'Voice' a -> 'Voice' a
-- 'over'     ('notes' . 'elements' odd)   :: ('Note' a -> 'Note' a) -> 'Voice' a -> 'Voice' a
-- @
--
-- @
-- 'toListOf' ('notes' . 'each')                :: 'Voice' a -> ['Note' a]
-- 'toListOf' ('notes' . 'elements' odd)        :: 'Voice' a -> ['Note' a]
-- 'toListOf' ('notes' . 'each' . 'filtered'
--              (\\x -> x^.'duration' \< 2))  :: 'Voice' a -> ['Note' a]
-- @

-- | View a score as a list of duration-value pairs. Analogous to 'triples'.
pairs :: Lens (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
pairs = unsafePairs

-- | A voice is a list of notes up to meta-data. To preserve meta-data, use the more
-- restricted 'voice' and 'notes'.
unsafeNotes :: Iso (Voice a) (Voice b) [Note a] [Note b]
unsafeNotes = _Wrapped

-- | A score is a list of (duration-value pairs) up to meta-data.
-- To preserve meta-data, use the more restricted 'pairs'.
unsafePairs :: Iso (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
unsafePairs = iso (map (^.from note) . (^.notes)) ((^.voice) . map (^.note))

durationsVoice :: Iso' [Duration] (Voice ())
durationsVoice = iso (mconcat . fmap (\d -> stretch d $ pure ())) (^. durationsV)

-- |
-- Unzip the given voice.
--
unzipVoice :: Voice (a, b) -> (Voice a, Voice b)
unzipVoice = unzipR

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoiceScale :: Voice a -> Voice b -> Voice (a, b)
zipVoiceScale = zipVoiceScaleWith (,)

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoiceScale3 :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
zipVoiceScale3 a b c = zipVoiceScale a (zipVoiceScale b c)

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoiceScale4 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoiceScale4 a b c d = zipVoiceScale a (zipVoiceScale b (zipVoiceScale c d))

-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoiceScale5 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice e -> Voice (a, (b, (c, (d, e))))
zipVoiceScale5 a b c d e = zipVoiceScale a (zipVoiceScale b (zipVoiceScale c (zipVoiceScale d e)))

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
zipVoiceScaleWith :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceScaleWith = zipVoiceScaleWith' (*)

-- |
-- Join the given voices without combining durations.
--
zipVoiceWithNoScale :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWithNoScale = zipVoiceScaleWith' const

-- |
-- Join the given voices by combining durations and values using the given function.
--
zipVoiceScaleWith' :: (Duration -> Duration -> Duration) -> (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceScaleWith' f g
  ((unzip.view pairs) -> (ad, as))
  ((unzip.view pairs) -> (bd, bs))
  = let cd = zipWith f ad bd
        cs = zipWith g as bs
     in view (from unsafePairs) (zip cd cs)


-- TODO generalize these to use a Monoidal interface, rather than ([a] -> a)
-- The use of head (see below) if of course the First monoid

-- |
-- Merge consecutive equal notes.
--
fuse :: Eq a => Voice a -> Voice a
fuse = fuseBy (==)

-- |
-- Merge consecutive notes deemed equal by the given predicate.
--
fuseBy :: (a -> a -> Bool) -> Voice a -> Voice a
fuseBy p = fuseBy' p head

-- |
-- Merge consecutive equal notes using the given equality predicate and merge function.
--
fuseBy' :: (a -> a -> Bool) -> ([a] -> a) -> Voice a -> Voice a
fuseBy' p g = over unsafePairs $ fmap foldNotes . Data.List.groupBy (inspectingBy snd p)
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
coverRests x = if hasOnlyRests then Nothing else Just (fmap fromJust $ fuseBy merge x)
  where
    norm = fuseRests x
    merge Nothing  Nothing  = error "Voice normalized, so consecutive rests are impossible"
    merge (Just x) Nothing  = True
    merge Nothing  (Just x) = True
    merge (Just x) (Just y) = False
    hasOnlyRests = all isNothing $ toListOf traverse x -- norm

-- | Decorate all notes in a voice with their context, i.e. previous and following value
-- if present.
withContext :: Voice a -> Voice (Ctxt a)
withContext = over valuesV addCtxt

-- durationsV and valuesV are useful, but slightly odd
-- What happens if the user changes the length of the list?
-- Is there a more safe idiom that can be used instead?
-- TODO more elegant definition?

-- | A lens to the durations in a voice.
durationsV :: Lens' (Voice a) [Duration]
durationsV = lens getDurs (flip setDurs)
  where
    getDurs :: Voice a -> [Duration]
    getDurs = map fst . view pairs

    setDurs :: [Duration] -> Voice a -> Voice a
    setDurs ds as = zipVoiceScaleWith' (\a b -> a) (\a b -> b) (mconcat $ map durToVoice ds) as

    durToVoice d = stretch d $ pure ()

-- | A lens to the values in a voice.
valuesV :: Lens (Voice a) (Voice b) [a] [b]
valuesV = lens getValues (flip setValues)
  where
    -- getValues :: Voice a -> [a]
    getValues = map snd . view pairs

    -- setValues :: [a] -> Voice b -> Voice a
    setValues as bs = zipVoiceScaleWith' (\a b -> b) (\a b -> a) (listToVoice as) bs

    listToVoice = mconcat . map pure

-- Lens "filtered" through a voice
voiceLens :: (s -> a) -> (b -> s -> t) -> Lens (Voice s) (Voice t) (Voice a) (Voice b)
voiceLens getter setter = lens (fmap getter) (flip $ zipVoiceWithNoScale setter)
-- TODO could also use (zipVoiceScaleWith' max) or (zipVoiceScaleWith' min)

-- | Whether two notes have exactly the same duration pattern.
-- Two empty voices are considered to have the same duration pattern.
-- Voices with an non-equal number of notes differ by default.
sameDurations :: Voice a -> Voice b -> Bool
sameDurations a b = view durationsV a == view durationsV b

-- | Pair the values of two voices if and only if they have the same duration
-- pattern (as per 'sameDurations').
mergeIfSameDuration :: Voice a -> Voice b -> Maybe (Voice (a, b))
mergeIfSameDuration = mergeIfSameDurationWith (,)

-- | Combine the values of two voices using the given function if and only if they
-- have the same duration pattern (as per 'sameDurations').
mergeIfSameDurationWith :: (a -> b -> c) -> Voice a -> Voice b -> Maybe (Voice c)
mergeIfSameDurationWith f a b
  | sameDurations a b = Just $ zipVoiceWithNoScale f a b
  | otherwise         = Nothing
-- TODO could also use (zipVoiceScaleWith' max) or (zipVoiceScaleWith' min)

-- |
-- Split all notes of the latter voice at the onset/offset of the former.
--
-- >>> ["a",(2,"b")^.note,"c"]^.voice
-- [(1,"a")^.note,(2,"b")^.note,(1,"c")^.note]^.voice
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

-- | Split a homophonic texture into a polyphonic one. The returned voice list will not
-- have as many elements as the chord with the fewest number of notes.
homoToPolyphonic      :: Voice [a] -> [Voice a]
homoToPolyphonic xs = case nvoices xs of
  Nothing -> []
  Just n  -> fmap (\n -> fmap (!! n) xs) [0..n-1]
  where
    nvoices :: Voice [a] -> Maybe Int
    nvoices = maybeMinimum . fmap length . (^.valuesV)

    maybeMinimum :: Ord a => [a] -> Maybe a
    maybeMinimum xs = if null xs then Nothing else Just (minimum xs)

changeCrossing   :: Ord a => Voice a -> Voice a -> (Voice a, Voice a)
changeCrossing = undefined

changeCrossingBy :: Ord b => (a -> b) -> Voice a -> Voice a -> (Voice a, Voice a)
changeCrossingBy = undefined

processExactOverlaps :: (a -> a -> (a, a)) -> Voice a -> Voice a -> (Voice a, Voice a)
processExactOverlaps = undefined

processExactOverlaps' :: (a -> b -> Either (a,b) (b,a)) -> Voice a -> Voice b -> (Voice (Either b a), Voice (Either a b))
processExactOverlaps' = undefined

-- | Returns the onsets of all notes in a voice given the onset of the first note.
onsetsRelative    :: Time -> Voice a -> [Time]
onsetsRelative o v = case offsetsRelative o v of
  [] -> []
  xs -> o : init xs

-- | Returns the offsets of all notes in a voice given the onset of the first note.
offsetsRelative   :: Time -> Voice a -> [Time]
offsetsRelative o = fmap (\t -> o .+^ (t .-. 0)) . toAbsoluteTime . (^. durationsV)

-- | Returns the midpoints of all notes in a voice given the onset of the first note.
midpointsRelative :: Time -> Voice a -> [Time]
midpointsRelative o v = zipWith between (onsetsRelative o v) (offsetsRelative o v)
  where
    between p q = alerp p q 0.5

-- | Returns the eras of all notes in a voice given the onset of the first note.
erasRelative :: Time -> Voice a -> [Span]
erasRelative o v = zipWith (<->) (onsetsRelative o v) (offsetsRelative o v)

{-
onsetMap  :: Score a -> Map Time a
onsetMap = fmap (view onset) . eraMap

offsetMap :: Score a -> Map Time a
offsetMap = fmap (view offset) . eraMap

midpointMap :: Score a -> Map Time a
midpointMap = fmap (view midpoint) . eraMap

eraMap :: Score a -> Map Span a
eraMap = error "No eraMap"

durations :: Voice a -> [Duration]
durations = view durationsV
-}
