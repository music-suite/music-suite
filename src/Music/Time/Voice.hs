{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-deprecations
  -fno-warn-unused-matches #-}

module Music.Time.Voice
  ( -- * Voice type
    Voice,

    -- * Construction
    -- $smartConstructors
    voice,
    notes,
    pairs,
    durationsAsVoice,

    -- * Map
    mapWithOnsetRelative,
    mapWithOffsetRelative,
    mapWithSpanRelative,

    -- * Points in a voice
    onsetsRelative,
    offsetsRelative,
    midpointsRelative,
    erasRelative,

    -- * Transformations
    -- ** Rotation
    rotateDurations,
    rotateValues,

    -- ** Fusion
    fuse,
    fuseBy,

    -- *** Fuse rests
    fuseRests,
    coverRests,

    {- TODO: Move here from Music.Score.Pitch when we can refer to HasPitches from here
    -- * Combining voices
    stitch,
    stitchLast,
    stitchWith,
    -}

    -- ** Zips and unzip
    zipVoiceScale,
    zipVoiceNoScale,
    -- FIXME compose with (lens assoc unassoc) for the 3 and 4 versions
    zipVoiceScaleWith,
    zipVoiceWithNoScale,
    zipVoiceWith',
    unzipVoice,

    -- ** Merging
    sameDurations,
    mergeIfSameDuration,
    mergeIfSameDurationWith,
    homoToPolyphonic,

    -- * Context
    -- TODO clean
    withContext,

    -- * Conversion
    noteToVoice,
  )
where

import Control.Applicative
import Control.Lens hiding
  ( (<|),
    Indexable,
    Level,
    below,
    index,
    inside,
    parts,
    reversed,
    transform,
    (|>),
  )
import Control.Monad
import Control.Monad.Compose
import Control.Monad.Zip
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.AffineSpace
import Data.Coerce (coerce)
import qualified Data.Either
import qualified Data.Foldable
import Data.Functor.Context
import qualified Data.List
import Data.Maybe
import Data.String
import Data.Typeable (Typeable)
import GHC.Exts (IsList (..))
import Music.Dynamics.Literal
import Music.Pitch.Literal
import Music.Time.Internal.Util
import Music.Time.Juxtapose
import Music.Time.Note

-- Both 'Voice' and 'Note' have duration but no position. The difference
-- is that 'Note' sustains a single value throughout its duration, while
-- a voice may contain multiple values. It is called voice because it is
-- generalizes the notation of a voice in choral or multi-part instrumental music.
--
-- It may be useful to think about 'Voice' and 'Note' as vectors in time space
-- (i.e. 'Duration'), that also happens to carry around other values, such as pitches.

-- |
-- A sequential composition of values with associated durations.
newtype Voice a = Voice {getVoice :: [Note a]}
  deriving (Eq, Ord, Typeable, Foldable, Traversable, Functor, Semigroup, Monoid)

instance (Show a, Transformable a) => Show (Voice a) where
  show x = show (x ^. notes) ++ "^.voice"

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

  pure = return

  (<*>) = ap

instance Alternative Voice where

  (<|>) = (<>)

  empty = mempty

instance Monad Voice where

  return = Voice . return . return

  (>>=) :: forall a b. Voice a -> (a -> Voice b) -> Voice b
  xs >>= f = coerce @[Note b] @(Voice b) $ (coerce @(Voice b) @[Note b] . f) `mbind` coerce xs

instance MonadPlus Voice where

  mzero = mempty

  mplus = mappend

instance IsList (Voice a) where

  -- NOTE "ignoring meta" is a misnomer, see TODO.md
  type Item (Voice a) = Note a

  toList = view notesIgnoringMeta

  fromList = view (re notesIgnoringMeta)

instance Cons (Voice a) (Voice b) (Note a) (Note b) where
  _Cons = prism (\(s, v) -> (view voice . return $ s) <> v) $ \v -> case view notes v of
    [] -> Left mempty
    (x : xs) -> Right (x, view voice xs)

instance Snoc (Voice a) (Voice b) (Note a) (Note b) where
  _Snoc = prism (\(v, s) -> v <> (view voice . return $ s)) $ \v -> case unsnoc (view notes v) of
    Nothing -> Left mempty
    Just (xs, x) -> Right (view voice xs, x)

instance ToJSON a => ToJSON (Voice a) where
  -- TODO meta
  toJSON x = JSON.object [("notes", toJSON ns)]
    where
      ns = x ^. notes

instance FromJSON a => FromJSON (Voice a) where
  -- TODO change to include meta
  parseJSON (JSON.Object x) = parseNL =<< (x JSON..: "notes")
    where
      parseNL (JSON.Array xs) = fmap ((^. voice) . toList) $ traverse parseJSON xs
      parseNL _ = empty
      toList = toListOf traverse
  parseJSON _ = empty

instance Transformable (Voice a) where
  transform s = over notes (transform s)

instance HasDuration (Voice a) where
  _duration = sumOf (notes . each . duration)

instance (Transformable a, HasDuration a, Splittable a) => Splittable (Voice a) where
  -- TODO meta
  split d v = case splitNotes d (v ^. notes) of
    (as, Nothing, cs) -> (as ^. voice, cs ^. voice)
    (as, Just (b1, b2), cs) -> (as ^. voice `snoc` b1, b2 `cons` cs ^. voice)

splitNotes :: (Transformable a, HasDuration a, Splittable a) => Duration -> [a] -> ([a], Maybe (a, a), [a])
splitNotes d xs = case (durAndNumNotesToFirst, needSplit) of
  (Just (_, 0), _) -> ([], Nothing, xs)
  (Nothing, False) -> (xs, Nothing, [])
  (Just (_, n), False) -> (take n xs, Nothing, drop n xs)
  (Nothing, True) -> (init xs, Just (splitEnd (sum (fmap (^. duration) xs) - d) (last xs)), [])
  (Just (d', n), True) ->
    ( take (n -1) xs,
      Just (splitEnd (d' - d) (xs !! pred n)), -- (d'-d) is how much we have to cut
      drop n xs
    )
  where
    needSplit = case durAndNumNotesToFirst of
      Nothing -> d < sum (fmap (^. duration) xs)
      Just (d', _) -> d /= d'
    -- Given dur is >= requested dur
    -- Nothing means all goes to first
    durAndNumNotesToFirst =
      accumUntil
        (\(ds, ns) x -> if ds < d then Left (ds + x, ns + 1) else Right (ds, ns))
        (0, 0)
        (fmap (^. duration) xs)
    splitEnd d x = split ((x ^. duration) - d) x
    -- >>> accumUntil (\s a -> if s < 345 then Left (s + a) else Right s) 0 [1..]
    -- Just 351
    accumUntil :: (s -> a -> Either s b) -> s -> [a] -> Maybe b
    accumUntil f z xs = Data.Maybe.listToMaybe $ fmap fromRight $ dropWhile Data.Either.isLeft $ scanl (f . fromLeft) (Left z) xs
      where
        fromRight (Right x) = x
        fromRight _ = error "accumUntil"
        fromLeft (Left x) = x
        fromLeft _ = error "accumUntil"

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

  abs = fmap abs

  signum = fmap signum

  (+) = liftA2 (+)

  (-) = liftA2 (-)

  (*) = liftA2 (*)

-- | Create a 'Voice' from a list of 'Note's.
voice :: Iso' [Note a] (Voice a)
voice = coerced
{-# INLINE voice #-}

-- | Convert a note to a singleton voice.
noteToVoice :: Note a -> Voice a
noteToVoice = view voice . pure

-- | View a 'Voice' as a list of 'Note' values.
notes :: Iso (Voice a) (Voice b) [Note a] [Note b]
notes = coerced

-- $smartConstructors
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
pairs :: Iso (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
pairs = pairsIgnoringMeta

-- | A voice is a list of notes up to meta-data. To preserve meta-data, use the more
-- restricted 'voice' and 'notes'.
notesIgnoringMeta :: Iso (Voice a) (Voice b) [Note a] [Note b]
notesIgnoringMeta = iso getVoice Voice

-- | A score is a list of (duration-value pairs) up to meta-data.
-- To preserve meta-data, use the more restricted 'pairs'.
pairsIgnoringMeta :: Iso (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
pairsIgnoringMeta = iso (map (^. from note) . (^. notes)) ((^. voice) . map (^. note))

durationsAsVoice :: Iso' [Duration] (Voice ())
durationsAsVoice = iso (mconcat . fmap (\d -> stretch d $ pure ())) (^. durationsV)

mapWithOffsetRelative :: Time -> (Time -> a -> b) -> Voice a -> Voice b
mapWithOffsetRelative t f = mapWithSpanRelative t (\s x -> f (s ^. offset) x)

mapWithOnsetRelative :: Time -> (Time -> a -> b) -> Voice a -> Voice b
mapWithOnsetRelative t f = mapWithSpanRelative t (\s x -> f (s ^. onset) x)

-- >>> mapWithSpanRelative 0 (\s x -> s) $ asVoice $ mconcat [c,d^*2,e]
-- [(1,0 <-> 1)^.note,(2,1 <-> 3)^.note,(1,3 <-> 4)^.note]^.voice
mapWithSpanRelative :: Time -> (Span -> a -> b) -> Voice a -> Voice b
mapWithSpanRelative t f v = set valuesV newValues v
  where
    newValues = zipWith f (erasRelative t v) (v ^. valuesV)

-- |
-- Unzip the given voice.
unzipVoice :: Voice (a, b) -> (Voice a, Voice b)
unzipVoice = unzipR
  where
    unzipR :: Functor f => f (a, b) -> (f a, f b)
    unzipR x = (fmap fst x, fmap snd x)

-- |
-- Join the given voices by multiplying durations and pairing values.
zipVoiceScale :: Voice a -> Voice b -> Voice (a, b)
zipVoiceScale = zipVoiceScaleWith (,)

-- |
-- Join the given voices by pairing values and selecting the first duration.
zipVoiceNoScale :: Voice a -> Voice b -> Voice (a, b)
zipVoiceNoScale = zipVoiceWithNoScale (,)

{-
[NOTE this proof ignores meta-data and newtypes]

Naturality law:

  ∀ f g ma mb.
  fmap@Voice (f *** g) (mzip ma mb)
    = mzip (fmap@Voice f ma) (fmap@Voice g mb)

  ∀ f g ma mb.
  fmap@Voice (f *** g) (zipVoiceWith' (const) (,) ma mb)
    = zipVoiceWith' (const) (,) (fmap@Voice f ma) (fmap@Voice g mb)

  ∀ f g ma mb.
  fmap@Voice (f *** g) (zipVoiceWith' (const) (,) ma mb) = (
    \f' g' xs ys -> let
        (ad, as) = unzip xs
        (bd, bs) = unzip ys
        cd = zipWith f' ad bd
        cs = zipWith g' as bs
     in (zip cd cs)
    )
    (const) (,) (fmap@Voice f ma) (fmap@Voice g mb)

  ∀ f g ma mb.
  fmap@Voice (f *** g) (zipVoiceWith' (const) (,) ma mb) =
    let
        (ad, as) = unzip (fmap@Voice f ma)
        (bd, bs) = unzip (fmap@Voice g mb)
        cd = zipWith (const) ad bd
        cs = zipWith (,) as bs
     in (zip cd cs)

  ∀ f g ma mb.
  fmap@Voice (f *** g)
  (
    let
        (ad, as) = unzip ma
        (bd, bs) = unzip mb
        cd = zipWith (const) ad bd
        cs = zipWith (,) as bs
     in (zip cd cs)
  )
  =
    let
        (ad, as) = unzip (fmap@Voice f ma)
        (bd, bs) = unzip (fmap@Voice g mb)
        cd = zipWith (const) ad bd
        cs = zipWith (,) as bs
     in (zip cd cs)

  ∀ f g ma mb.
  fmap@Voice (f *** g)
  (
    let
        (ad, as) = unzip ma
        (bd, bs) = unzip mb
        cd = zipWith (const) ad bd
        cs = zipWith (,) as bs
     in (zip cd cs)
  )
  =
    let
        (ad, as) = unzip (fmap@Voice f ma)
        (bd, bs) = unzip (fmap@Voice g mb)
        cd = zipWith (const) ad bd
        cs = zipWith (,) as bs
     in (zip cd cs)

  ∀ f g ma mb.
  map (second (f *** g))
  (
    let
        (ad, as) = unzip ma
        (bd, bs) = unzip mb
        cd = zipWith (const) ad bd
        cs = zipWith (,) as bs
     in (zip cd cs)
  )
  =
    let
        (ad, as) = unzip (map (second f ma))
        (bd, bs) = unzip (map (second g mb))
        cd = zipWith (const) ad bd
        cs = zipWith (,) as bs
     in (zip cd cs)

  ∀ f g ma mb.
  let
      (ad, as) = unzip ma
      (bd, bs) = unzip mb
      cd = zipWith (const) ad bd
      cs = zipWith (,) as bs
   in map (second (f ***g)) (zip cd cs)
  =
  let
      (ad, as) = unzip (map (second f ma))
      (bd, bs) = unzip (map (second g mb))
      cd = zipWith (const) ad bd
      cs = zipWith (,) as bs
   in zip cd cs

  ∀ f g ma mb.
  let
      (ad, as) = unzip ma
      (bd, bs) = unzip mb
      cd = zipWith (const) ad bd
      cs = zipWith (,) as bs
   in zip cd (map (f *** g) cs)
  =
  let
      (ad, as) = unzip (map (second f ma))
      (bd, bs) = unzip (map (second g mb))
      cd = zipWith (const) ad bd
      cs = zipWith (,) as bs
   in zip cd cs


  ∀ f g ma mb.
  let
      (ad, as) = unzip ma
      (bd, bs) = unzip mb
      cd = zipWith (const) ad bd
      cs = map (f *** g) (zipWith (,) as bs)
   in zip cd cs
  =
  let
      (ad, as) = unzip (map (second f ma))
      (bd, bs) = unzip (map (second g mb))
      cd = zipWith (const) ad bd
      cs = zipWith (,) as bs
   in zip cd cs

  ∀ f g ma mb.
  let
      (ad, as) = unzip ma
      (bd, bs) = unzip mb
      cd = zipWith (const) ad bd
      cs = zipWith (,) (map f as) (map g bs)
   in zip cd cs
  =
  let
      (ad, as) = unzip (map (second f ma))
      (bd, bs) = unzip (map (second g mb))
      cd = zipWith (const) ad bd
      cs = zipWith (,) as bs
   in zip cd cs

  ∀ f g ma mb.
  let
      (ad, as) = unzip ma
      (bd, bs) = unzip mb
      cd = zipWith (const) ad bd
      cs = zipWith (,) (map f as) (map g bs)
   in zip cd cs
  =
  let
      (ad, as) = second (map f) (unzip ma)
      (bd, bs) = second (map g) (unzip mb)
      cd = zipWith (const) ad bd
      cs = zipWith (,) as bs
   in zip cd cs

  ∀ f g ma mb.
  let
      (ad, as) = unzip ma
      (bd, bs) = unzip mb
      cd = zipWith (const) ad bd
      cs = zipWith (,) (map f as) (map g bs)
   in zip cd cs
  =
  let
      (ad, as) = unzip ma
      (bd, bs) = unzip mb
      cd = zipWith (const) ad bd
      cs = zipWith (,) (map f as) (map g bs)
   in zip cd cs

   QED.

Information preservation:
  ∀ ma mb.
  fmap@Voice (const ()) ma = fmap@Voice (const ()) mb
  ->
  munzip@Voice (mzip@Voice ma mb) = (ma, mb)

  ∀ ma mb.
  durations ma = durations mb
  ->
  munzip@Voice (mzip@Voice ma mb) = (ma, mb)

  ∀ ma mb.
  durations ma = durations mb
  ->
  munzip@Voice (zipVoiceWith const (,) ma mb) = (ma, mb)

  ∀ ma mb.
  durations ma = durations mb
  ->
  fmap@Voice fst (zipVoiceWith const (,) ma mb) = ma
    /\
  fmap@Voice snd (zipVoiceWith const (,) ma mb) = mb

  ∀ ma mb.
  durations ma = durations mb
  ->
  fmap@Voice fst (
             let
                (ad, as) = unzip ma
                (bd, bs) = unzip mb
                cd = zipWith const ad bd
                cs = zipWith (,) as bs
             in zip cd cs
    ) = ma
    /\
  fmap@Voice snd (
             let
                (ad, as) = unzip ma
                (bd, bs) = unzip mb
                cd = zipWith const ad bd
                cs = zipWith (,) as bs
             in zip cd cs
    ) = mb

  ∀ ma mb.
  durations ma = durations mb
  ->
  map (second fst) (
             let
                (ad, as) = unzip ma
                (bd, bs) = unzip mb
                cd = zipWith const ad bd
                cs = zipWith (,) as bs
             in zip cd cs
    ) = ma
    /\
  map (second snd) (
             let
                (ad, as) = unzip ma
                (bd, bs) = unzip mb
                cd = zipWith const ad bd
                cs = zipWith (,) as bs
             in zip cd cs
    ) = mb

  ∀ ad as bd bs.
  ad = bd
  ->
  map (second fst) (
             let
                cd = zipWith const ad bd
                cs = zipWith (,) as bs
             in zip cd cs
    ) = zip ad as
    /\
  map (second snd) (
             let
                cd = zipWith const ad bd
                cs = zipWith (,) as bs
             in zip cd cs
    ) = zip bd bs

  ∀ ad as bd bs.
  ad = bd
  ->
  map (second fst) (
             let
                cs = zipWith (,) as bs
             in zip ad cs
    ) = zip ad as
    /\
  map (second snd) (
             let
                cs = zipWith (,) as bs
             in zip ad cs
    ) = zip bd bs

  ∀ ad as bd bs.
  ad = bd
  ->
  map (second fst) (zip ad (zip as bs)) = zip ad as
    /\
  map (second snd) (zip ad (zip as bs)) = zip bd bs

  ∀ ad as bd bs.
  ad = bd
  ->
  zip ad as = zip ad as
    /\
  zip ad bs = zip bd bs

  QED.

Note that MonadZip can not use zipVoiceScale: that would break Information preservation.

 -}
instance MonadZip Voice where
  mzip = zipVoiceNoScale

-- |
-- Join the given voices by multiplying durations and combining values using the given function.
zipVoiceScaleWith :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceScaleWith = zipVoiceWith' (*)

-- |
-- Join the given voices without combining durations. The durations are taken from the first
-- voice.
zipVoiceWithNoScale :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWithNoScale = zipVoiceWith' const

-- |
-- Join the given voices by combining durations and values using the given function.
zipVoiceWith' :: (Duration -> Duration -> Duration) -> (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith'
  f
  g
  ((unzip . view pairs) -> (ad, as))
  ((unzip . view pairs) -> (bd, bs)) =
    let cd = zipWith f ad bd
        cs = zipWith g as bs
     in view (from pairsIgnoringMeta) (zip cd cs)

-- TODO generalize these to use a Monoidal interface, rather than ([a] -> a)
-- The use of head (see below) if of course the First monoid

-- |
-- Merge consecutive equal notes.
fuse :: Eq a => Voice a -> Voice a
fuse = fuseBy (==)

-- |
-- Merge consecutive notes deemed equal by the given predicate.
fuseBy :: (a -> a -> Bool) -> Voice a -> Voice a
fuseBy p = fuseBy' p head

-- |
-- Merge consecutive equal notes using the given equality predicate and merge function.
fuseBy' :: (a -> a -> Bool) -> ([a] -> a) -> Voice a -> Voice a
fuseBy' p g = over pairsIgnoringMeta $ fmap foldNotes . Data.List.groupBy (inspectingBy snd p)
  where
    -- Add up durations and use a custom function to combine notes
    --
    -- Typically, the combination function us just 'head', as we know that group returns
    -- non-empty lists of equal elements.
    foldNotes (unzip -> (ds, as)) = (sum ds, g as)

-- |
-- Fuse all rests in the given voice. The resulting voice will have no consecutive rests.
fuseRests :: Voice (Maybe a) -> Voice (Maybe a)
fuseRests = fuseBy (\x y -> isNothing x && isNothing y)

-- |
-- Remove all rests in the given voice by prolonging the previous note. Returns 'Nothing'
-- if and only if the given voice contains rests only.
coverRests :: Voice (Maybe a) -> Maybe (Voice a)
coverRests x = if hasOnlyRests then Nothing else Just (fmap fromJust $ fuseBy merge x)
  where
    -- norm = fuseRests x
    merge Nothing Nothing = error "Voice normalized, so consecutive rests are impossible"
    merge (Just x) Nothing = True
    merge Nothing (Just x) = True
    merge (Just x) (Just y) = False
    hasOnlyRests = all isNothing $ toListOf traverse x -- norm


-- | Decorate all notes in a voice with their context, i.e. previous and following value
-- if present.
withContext :: Voice a -> Voice (Ctxt a)
withContext = over valuesV addCtxt

-- Warning: Breaks the lens laws, unless the length of the list is unmodified.
durationsV :: Lens' (Voice a) [Duration]
durationsV = lens getDurs (flip setDurs)
  where
    getDurs :: Voice a -> [Duration]
    getDurs = map fst . view pairs
    setDurs :: [Duration] -> Voice a -> Voice a
    setDurs ds as = zipVoiceWith' (\a b_ -> a) (\a_ b -> b) (mconcat $ map durToVoice ds) as
    durToVoice d = stretch d $ pure ()

-- Warning: Breaks the lens laws, unless the length of the list is unmodified.
valuesV :: Lens (Voice a) (Voice b) [a] [b]
valuesV = lens getValues (flip setValues)
  where
    -- getValues :: Voice a -> [a]
    getValues = map snd . view pairs
    -- setValues :: [a] -> Voice b -> Voice a
    setValues as bs = zipVoiceWith' (\_a b -> b) (\a _b -> a) (listToVoice as) bs
    listToVoice = mconcat . map pure

{-
-- | A lens filtered through a voice.
--
-- /Warning/ This is only a lens if the length of the voice is not altered.
voiceLens :: Lens s t a b -> Lens (Voice s) (Voice t) (Voice a) (Voice b)
voiceLens getter setter = lens (fmap getter) (flip $ zipVoiceWithNoScale setter)
-}

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
  | otherwise = Nothing

-- TODO could also use (zipVoiceWith' max) or (zipVoiceWith' min)

-- -- |
-- -- Split all notes of the latter voice at the onset/offset of the former.
-- --
-- -- >>> ["a",(2,"b")^.note,"c"]^.voice
-- -- [(1,"a")^.note,(2,"b")^.note,(1,"c")^.note]^.voice
-- --
-- splitLatterToAssureSameDuration :: Voice b -> Voice b -> Voice b
-- splitLatterToAssureSameDuration = splitLatterToAssureSameDurationWith dup
--   where
--     dup x = (x,x)
--
-- splitLatterToAssureSameDurationWith :: (b -> (b, b)) -> Voice b -> Voice b -> Voice b

-- polyToHomophonic      :: [Voice a] -> Maybe (Voice [a])
--
-- polyToHomophonicForce :: [Voice a] -> Voice [a]

-- | Split a homophonic texture into a polyphonic one. The returned voice list will
-- have as many elements as the chord with the fewest number of notes.
homoToPolyphonic :: Voice [a] -> [Voice a]
homoToPolyphonic xs = case nvoices xs of
  Nothing -> []
  Just n -> fmap (\n -> fmap (!! n) xs) [0 .. n -1]
  where
    nvoices :: Voice [a] -> Maybe Int
    nvoices = maybeMinimum . fmap length . (^. valuesV)
    maybeMinimum :: Ord a => [a] -> Maybe a
    maybeMinimum xs = if null xs then Nothing else Just (minimum xs)

-- changeCrossing   :: Ord a => Voice a -> Voice a -> (Voice a, Voice a)
--
-- changeCrossingBy :: Ord b => (a -> b) -> Voice a -> Voice a -> (Voice a, Voice a)
--
-- processExactOverlaps :: (a -> a -> (a, a)) -> Voice a -> Voice a -> (Voice a, Voice a)
--
-- processExactOverlaps' :: (a -> b -> Either (a,b) (b,a)) -> Voice a -> Voice b -> (Voice (Either b a), Voice (Either a b))

-- | Returns the onsets of all notes in a voice given the onset of the first note.
onsetsRelative :: Time -> Voice a -> [Time]
onsetsRelative o v = case offsetsRelative o v of
  [] -> []
  xs -> o : init xs

-- | Returns the offsets of all notes in a voice given the onset of the first note.
offsetsRelative :: Time -> Voice a -> [Time]
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

-- | Rotate the durations of a voice.
--
-- >>> rotateDurations 1 [(1,'c'), (2, 'd'), (1, 'e')]
-- [(2,'c'), (1, 'd'), (1, 'e')]
rotateDurations :: Int -> Voice a -> Voice a
rotateDurations n x = view voice $ fmap (view note) $ zip (rotate n ds) vs
  where
    (ds, vs) = unzip $ fmap (view $ from note) $ view notes x

-- | Rotate the values of a voice.
--
-- >>> rotateValues 1 [(1,'c'), (2, 'd'), (1, 'e')]
-- [(1,'d'), (2, 'e'), (1, 'c')]
rotateValues :: Int -> Voice a -> Voice a
rotateValues n x = view voice $ fmap (view note) $ zip ds (rotate n vs)
  where
    (ds, vs) = unzip $ fmap (view $ from note) $ view notes x
