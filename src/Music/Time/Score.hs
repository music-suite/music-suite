{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

module Music.Time.Score
  ( -- * Score type
    Score,

    -- * Construction
    -- $traversals
    score,
    events,
    eras,
    triples,

    -- * Traversal
    mapWithSpan,
    filterWithSpan,
    mapFilterWithSpan,
    mapWithTime,
    filterWithTime,
    mapFilterWithTime,

    -- * Simultaneous
    -- TODO check for overlapping values etc
    -- simult,
    hasOverlappingEvents,
    simultaneous,

    -- * Normalize
    normalizeScore,
    removeRests,

    -- * Unsafe versions
    eventsIgnoringMeta,
    triplesIgnoringMeta,

    -- * Conversion
    eventToScore,
  )
where

import Control.Applicative
import Control.Comonad
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
import Control.Monad.Plus
import Control.Monad.Writer
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import Data.Ratio
import Data.Semigroup
import Data.Semigroup hiding ()
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Typeable
import Data.VectorSpace
import Data.VectorSpace hiding (Sum (..))
import Iso.Deriving

import Music.Dynamics.Literal
import Music.Pitch.Literal
import Music.Time.Event
import Music.Time.Internal.Util
import Music.Time.Juxtapose
import Music.Time.Meta
import Music.Time.Note
import Music.Time.Voice

-- $traversals
-- @
-- events . each . from event . swapped . mapping onsetAndDuration . swapped
--    :: Traversal (Score a) (Score b) ((Time, Duration), a) ((Time, Duration), b)
-- @
--
-- @
-- events . each . from event . swapped . mapping onsetAndOffset . swapped
--    :: Traversal (Score a) (Score b) ((Time, Time), a) ((Time, Time), b)
-- @

--   * 'empty' creates an empty score
--
--   * 'pure' creates a score containing a single note in the span @0 '<->' 1@
--
--   * '<|>' composes scores in parallel
--
--   * '|>' composes scores as a forward sequence
--
--   * '<|' composes scores as a backward sequence
--
-- You can also use '<>' and 'mempty' of course.
--

-- | A set of events of type @a@, with an associated time spans.
newtype Score a = Score {getScore :: (Meta, Score' a)}
  deriving (Functor, Semigroup, Monoid, Foldable, Traversable, Typeable {-, Show, Eq, Ord-})

--
-- You typically create a 'Score' using 'score', 'events', 'voices', and 'phrases', or the 'Alternative' interface.
--
-- Score is an instance of 'Transformable', so you can use 'delay' and 'stretch'.
--
-- Score is an instance of 'HasPosition', so you can use 'duration', 'onset', 'offset', 'era'.
--
-- To inspect or deconstruct a score, see 'events', 'voices', and 'phrases', as
-- well as 'singleNote', 'singleVoice', and 'singlePhrase'
--

-- | Up to meta-data.
instance Applicative Score where

  pure = Score . pure . pure

  (<*>) = ap

-- | Up to meta-data.
instance Monad Score where
  Score (meta, xs) >>= f = Score (meta, xs >>= snd . getScore . f)

instance Alternative Score where

  empty = mempty

  (<|>) = mappend

-- | Up to meta-data.
instance MonadPlus Score where

  mzero = mempty

  mplus = mappend

{-
instance FunctorWithIndex Span Score where
  imap f = over (_Wrapped._2) $ imap f

instance FoldableWithIndex Span Score where
  ifoldMap f (Score (m,x)) = ifoldMap f x

instance TraversableWithIndex Span Score where
  itraverse f (Score (m,x)) = fmap (\x -> Score (m,x)) $ itraverse f x
-}

instance ToJSON a => ToJSON (Score a) where
  -- TODO meta
  toJSON x = JSON.object [("events", toJSON es)]
    where
      es = x ^. events

instance FromJSON a => FromJSON (Score a) where
  -- TODO change to include meta
  parseJSON (JSON.Object x) = parseEL =<< (x JSON..: "events")
    where
      parseEL (JSON.Array xs) = fmap ((^. score) . toList) $ traverse parseJSON xs
      parseEL _ = empty
      toList = toListOf traverse
  parseJSON _ = empty

instance Transformable (Score a) where
  transform t (Score (m, x)) = Score (transform t m, transform t x)

-- instance Splittable a => Splittable (Score a) where
-- split t (Score (m,x)) = (Score (m1,x1), Score (m2,x2))
-- where
-- (m1, m2) = split t m
-- (x1, x2) = split t x

instance HasPosition (Score a) where
  _era = _era . snd . getScore

instance IsString a => IsString (Score a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Score a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Score a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Score a) where
  fromDynamics = pure . fromDynamics

instance Num a => Num (Score a) where

  fromInteger = return . fromInteger

  abs = fmap abs

  signum = fmap signum

  (+) = liftA2 (+)

  (-) = liftA2 (-)

  (*) = liftA2 (*)

instance HasMeta (Score a) where
  meta = iso getScore Score . _1

-- | This instance exists only for the @enumFrom...@ methods.
instance Enum a => Enum (Score a) where

  toEnum = return . toEnum

  fromEnum = list 0 (fromEnum . head) . Foldable.toList

newtype Score' a = Score' {getScore' :: [Event a]}
  deriving ({-Eq, -} {-Ord, -} {-Show, -} Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

instance (Show a, Transformable a) => Show (Score a) where
  show x = show (x ^. events) ++ "^.score"

deriving via
  (WriterT Span [] `As1` Score')
  instance
    Applicative Score'

deriving via
  (WriterT Span [] `As1` Score')
  instance
    Monad Score'

instance Inject (WriterT Span [] x) (Score' x) where

  -- TODO zero-cost version
  inj (WriterT xs) = Score' (fmap (view event . swap) xs)

instance Project (WriterT Span [] x) (Score' x) where
  prj (Score' xs) = WriterT (fmap (swap . view (from event)) xs)

instance Isomorphic (WriterT Span [] x) (Score' x) where

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

instance Alternative Score' where

  empty = mempty

  (<|>) = mappend

instance MonadPlus Score' where

  mzero = mempty

  mplus = mappend

instance Transformable (Score' a) where
  transform t = Score' . transform t . getScore'

instance HasPosition (Score' a) where
  _era x = case foldMap (NonEmptyInterval . _era1) $ getScore' x of
    EmptyInterval -> Nothing
    NonEmptyInterval x -> Just x

{-
  (f x, g x) ^. from onsetAndOffset
    where
      f, g :: Score' a -> Time
      f = safeMinimum . fmap ((^. onset) . normalizeSpan) . toListOf (each . era) . getScore'
      g = safeMaximum . fmap ((^. offset) . normalizeSpan) . toListOf (each . era) . getScore'
      safeMinimum xs = if null xs then 0 else minimum xs
      safeMaximum xs = if null xs then 0 else maximum xs
-}

-- | Create a score from a list of events.
score :: Getter [Event a] (Score a)
score = from eventsIgnoringMeta
{-# INLINE score #-}

-- | View a 'Score' as a list of 'Event' values.
events :: Lens (Score a) (Score b) [Event a] [Event b]
events = iso getScore Score . _2 . iso getScore' Score' . sorted
  where
    -- TODO should not have to sort...
    sorted = iso (List.sortBy (Ord.comparing (^. onset))) (List.sortBy (Ord.comparing (^. onset)))
{-# INLINE events #-}

-- | Convert an event to a singleton score.
eventToScore :: Event a -> Score a
eventToScore = view score . pure

--
-- @
-- 'view' 'events'                        :: 'Score' a -> ['Event' a]
-- 'set'  'events'                        :: ['Event' a] -> 'Score' a -> 'Score' a
-- 'over' 'events'                        :: (['Event' a] -> ['Event' b]) -> 'Score' a -> 'Score' b
-- @
--
-- @
-- 'preview'  ('events' . 'each')           :: 'Score' a -> 'Maybe' ('Event' a)
-- 'preview'  ('events' . 'element' 1)      :: 'Score' a -> 'Maybe' ('Event' a)
-- 'preview'  ('events' . 'elements' odd)   :: 'Score' a -> 'Maybe' ('Event' a)
-- @
--
-- @
-- 'set'      ('events' . 'each')           :: 'Event' a -> 'Score' a -> 'Score' a
-- 'set'      ('events' . 'element' 1)      :: 'Event' a -> 'Score' a -> 'Score' a
-- 'set'      ('events' . 'elements' odd)   :: 'Event' a -> 'Score' a -> 'Score' a
-- @
--
-- @
-- 'over'     ('events' . 'each')           :: ('Event' a -> 'Event' b) -> 'Score' a -> 'Score' b
-- 'over'     ('events' . 'element' 1)      :: ('Event' a -> 'Event' a) -> 'Score' a -> 'Score' a
-- 'over'     ('events' . 'elements' odd)   :: ('Event' a -> 'Event' a) -> 'Score' a -> 'Score' a
-- @
--
-- @
-- 'toListOf' ('events' . 'each')                :: 'Score' a -> ['Event' a]
-- 'toListOf' ('events' . 'elements' odd)        :: 'Score' a -> ['Event' a]
-- 'toListOf' ('events' . 'each' . 'filtered'
--              (\\x -> x^.'duration' \< 2))  :: 'Score' a -> ['Event' a]
-- @

-- | A score is a list of events up to meta-data. To preserve meta-data, use the more
-- restricted 'score' and 'events'.
eventsIgnoringMeta :: Iso (Score a) (Score b) [Event a] [Event b]
eventsIgnoringMeta = iso getScore Score . noMeta . iso getScore' Score' . sorted
  where
    sorted = iso (List.sortBy (Ord.comparing (^. onset))) (List.sortBy (Ord.comparing (^. onset)))
    noMeta = iso extract return

-- | A score is a list of (time-duration-value triples) up to meta-data.
-- To preserve meta-data, use the more restricted 'triples'.
triplesIgnoringMeta :: Iso (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
triplesIgnoringMeta = iso _getScore _score
  where
    _score :: [(Time, Duration, a)] -> Score a
    _score = mconcat . fmap (uncurry3 event)
      where
        event t d x = (delay (t .-. 0) . stretch d) (return x)
    _getScore {-Transformable a => -} :: Score a -> [(Time, Duration, a)]
    _getScore =
      fmap (\(view onsetAndDuration -> (t, d), x) -> (t, d, x))
        . List.sortBy (Ord.comparing fst)
        . Foldable.toList
        . fmap (view $ from event)
        . reifyScore

-- | Map with the associated time span.
mapScore :: (Event a -> b) -> Score a -> Score b
mapScore f = over (iso getScore Score . _2) (mapScore' f)
  where
    mapScore' f = over (iso getScore' Score' . traverse) (extend f)

reifyScore :: Score a -> Score (Event a)
reifyScore = over (iso getScore Score . _2 . iso getScore' Score') $ fmap duplicate

-- | View a score as a list of time-duration-value triplets.
--
-- TODO replace this with traversals
triples {-Transformable a => -} :: Lens (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
triples = triplesIgnoringMeta

-- | Map over the values in a score.
mapWithSpan :: (Span -> a -> b) -> Score a -> Score b
mapWithSpan f = mapScore (uncurry f . view (from event))

-- | Filter the values in a score.
filterWithSpan :: (Span -> a -> Bool) -> Score a -> Score a
filterWithSpan f = mapFilterWithSpan (partial2 f)

-- | Efficient combination of 'mapWithSpan' and 'filterWithTime'.
mapFilterWithSpan :: (Span -> a -> Maybe b) -> Score a -> Score b
mapFilterWithSpan f = mcatMaybes . mapWithSpan f

-- | Map over the values in a score.
mapWithTime :: (Time -> Duration -> a -> b) -> Score a -> Score b
mapWithTime f = mapWithSpan (uncurry f . view onsetAndDuration)

-- | Filter the values in a score.
filterWithTime :: (Time -> Duration -> a -> Bool) -> Score a -> Score a
filterWithTime f = mapFilterWithTime (partial3 f)

-- | Efficient combination of 'mapWithTime' and 'filterWithTime'.
mapFilterWithTime :: (Time -> Duration -> a -> Maybe b) -> Score a -> Score b
mapFilterWithTime f = mcatMaybes . mapWithTime f

-- | Normalize a score, assuring its events spans are all forward (as by 'isForwardSpan'),
-- and that its onset is at least zero. Consequently, the onset and offset of each event
-- in the score is at least zero.
normalizeScore :: Score a -> Score a
normalizeScore = reset . normalizeScoreDurations
  where
    normalizeScoreDurations = over (events . each . era) normalizeSpan

-- | Delay a score so that it starts no later than time @0@.
reset :: Score a -> Score a
reset x = case _era x of
  Nothing -> x
  Just e ->
    let o = view onset e
     in if o < 0 then delay (0 .-. o) x else x

-- | Remove all 'Nothing' values in the score.
removeRests :: Score (Maybe a) -> Score a
removeRests = mcatMaybes

-- TODO version that reverses the values where appropriate
-- Use over (events . each) normalizeEvent or similar

-- |
-- Print all eras of the given score.
--
-- >>> toListOf eras $ pseq [c,d,e :: Score Integer]
-- [0 <-> 1,1 <-> 2,2 <-> 3]
eras :: Traversal' (Score a) Span
eras = events . each . era

-- TODO rename and expose this
-- We have an (Iso (Score a) (TMap Span [a])), with [] as default value
chordEvents :: Transformable a => Span -> Score a -> [a]
chordEvents s = fmap extract . filter ((== s) . view era) . view events

simultaneous' :: Transformable a => Score a -> Score [a]
simultaneous' sc = (^. from triplesIgnoringMeta) vs
  where
    -- es :: [Era]
    -- evs :: [[a]]
    -- vs :: [(Time, Duration, [a])]
    es = List.nub $ toListOf eras sc
    evs = fmap (`chordEvents` sc) es
    vs = zipWith (\(view onsetAndDuration -> (t, d)) a -> (t, d, a)) es evs

-- overSimult :: Transformable a => (Score [a] -> Score [b]) -> Score a -> Score b
-- overSimult f = mpseqter . f . simultaneous'

-- | Merge all simultaneous events using their 'Semigroup' instance.
simultaneous :: (Transformable a, Semigroup a) => Score a -> Score a
simultaneous = fmap (sconcat . NonEmpty.fromList) . simultaneous'

-- | True if the score has more than one overlapping event.
--
-- @
-- hasOverlappingEvents (simultaneous x) = False
-- @
hasOverlappingEvents :: Score a -> Bool
hasOverlappingEvents = anyDistinctOverlaps . toListOf (events . each . era)

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = List.nub xs /= xs

anyDistinctOverlaps :: [Span] -> Bool
anyDistinctOverlaps xs = hasDuplicates xs || anyOverlaps xs
  where
    anyOverlaps = foldr (||) False . combined overlaps

-- If the span list has duplicates, we have overlaps.

combined :: Eq a => (a -> a -> b) -> [a] -> [b]
combined f as = mcatMaybes [if x == y then Nothing else Just (x `f` y) | x <- as, y <- as]

