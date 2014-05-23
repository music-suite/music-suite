
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
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

module Music.Time.Score (
      -- * Music.Time.Score
      Score,

      -- ** Substructure
      score,
      notes,
      events,
      -- voices,
      singleNote,
      -- singleVoice,

      -- *** Unsafe operations
      unsafeNotes,
      unsafeEvents,
      -- unsafeVoices,

      -- ** Special traversals
      mapWithSpan,
      filterWithSpan,
      mapFilterWithSpan,
      mapEvents,
      filterEvents,
      mapFilterEvents,

      -- ** Simultaneous notes
      simult,
      simultaneous,
      -- mapSimultaneous,
      -- simultaneous',
  ) where

import           Data.AffineSpace
import           Data.AffineSpace.Point
import qualified Data.List.NonEmpty     as NonEmpty
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Ratio
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace

import           Music.Time.Juxtapose   (scat)
import           Music.Time.Meta
import           Music.Time.Note
import           Music.Time.Reverse
import           Music.Time.Split
import           Music.Time.Stretched
import           Music.Time.Voice

import           Control.Applicative
import           Control.Arrow          (first, second, (&&&), (***))
import           Control.Comonad
import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as Foldable
import qualified Data.List              as List
import qualified Data.Ord               as Ord
import           Data.Semigroup         hiding ()
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as T
import           Data.Typeable
import           Data.VectorSpace       hiding (Sum (..))
import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Util



type ScoreNote a = Note a

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

-- |
-- A 'Score' is a sequential or parallel composition of values, and allows overlapping events
--
-- You typically create a 'Score' using 'score', 'notes', 'voices', and 'phrases', or the 'Alternative' interface.
--
-- Score is an instance of 'Transformable', so you can use 'delay' and 'stretch'.
--
-- Score is an instance of 'HasPosition', so you can use 'duration', 'onset', 'offset', 'era'.
--
-- To inspect or deconstruct a score, see 'notes', 'voices', and 'phrases', as
-- well as 'singleNote', 'singleVoice', and 'singlePhrase'
--
-- The semantics are given by
--
-- @
-- type Score a = [Note a]
-- @
--

newtype Score a = Score { getScore' :: (Meta, NScore a) }
    deriving (Functor, Semigroup, Monoid, Foldable, Traversable, Typeable{-, Show, Eq, Ord-})

instance Wrapped (Score a) where
  type Unwrapped (Score a) = (Meta, NScore a)
  _Wrapped' = iso getScore' Score

instance Rewrapped (Score a) (Score b) where

instance Applicative Score where
  pure = return
  (<*>) = ap

instance Monad Score where
  return = (^. _Unwrapped') . return . return
  xs >>= f = (^. _Unwrapped') $ mbind ((^. _Wrapped') . f) ((^. _Wrapped') xs)

instance Alternative Score where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Score where
  mzero = mempty
  mplus = mappend

instance FunctorWithIndex Span Score where
  imap f = over (_Wrapped._2) $ imap f

instance FoldableWithIndex Span Score where
  ifoldMap f (Score (m,x)) = ifoldMap f x

instance TraversableWithIndex Span Score where
  itraverse f (Score (m,x)) = fmap (\x -> Score (m,x)) $ itraverse f x

instance Transformable (Score a) where
  transform t (Score (m,x)) = Score (transform t m, transform t x)

instance Reversible a => Reversible (Score a) where
  rev (Score (m,x)) = Score (rev m, rev x)

instance Splittable a => Splittable (Score a) where
  -- split (Score (m,x)) = unzipR $ Score (split m, split x)
  split = error "No Score.split"

instance HasPosition (Score a) where
  _onset (Score (_,x))    = _onset x
  _offset (Score (_,x))   = _offset x
  _position (Score (_,x)) = _position x

instance HasDuration (Score a) where
  _duration (Score (_,x)) = _duration x



-- Lifted instances

instance IsPitch a => IsPitch (Score a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Score a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Score a) where
  fromDynamics = pure . fromDynamics

-- Bogus instance, so we can use [c..g] expressions
instance Enum a => Enum (Score a) where
  toEnum = return . toEnum
  fromEnum = list 0 (fromEnum . head) . Foldable.toList

-- Bogus instance, so we can use numeric literals
instance Num a => Num (Score a) where
  fromInteger = return . fromInteger
  abs    = fmap abs
  signum = fmap signum
  (+)    = error "Not implemented"
  (-)    = error "Not implemented"
  (*)    = error "Not implemented"

-- Bogus instances, so we can use c^*2 etc.
instance AdditiveGroup (Score a) where
  zeroV   = error "Not implemented"
  (^+^)   = error "Not implemented"
  negateV = error "Not implemented"

instance VectorSpace (Score a) where
  type Scalar (Score a) = Duration
  d *^ s = d `stretch` s

instance HasMeta (Score a) where
  meta = _Wrapped . _1







newtype NScore a = NScore { getNScore :: [ScoreNote a] }
  deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (NScore a) where
  type Unwrapped (NScore a) = [ScoreNote a]
  _Wrapped' = iso getNScore NScore

instance Rewrapped (NScore a) (NScore b)

instance Applicative NScore where
  pure  = return
  (<*>) = ap

instance Monad NScore where
  return = (^. _Unwrapped) . pure . pure
  xs >>= f = (^. _Unwrapped) $ mbind ((^. _Wrapped') . f) ((^. _Wrapped') xs)

instance Alternative NScore where
  empty = mempty
  (<|>) = mappend

instance MonadPlus NScore where
  mzero = mempty
  mplus = mappend

instance FunctorWithIndex Span NScore where
  imap = undefined
  -- TODO

instance FoldableWithIndex Span NScore where
  ifoldMap = undefined
  -- TODO

instance TraversableWithIndex Span NScore where
  itraverse = undefined
  -- TODO

instance Transformable (NScore a) where
  transform t (NScore xs) = NScore (fmap (transform t) xs)

instance Reversible a => Reversible (NScore a) where
  rev (NScore xs) = NScore (fmap rev xs)

instance HasPosition (NScore a) where
  _onset  = safeMinimum . fmap _onset . view _Wrapped'
  _offset = safeMaximum . fmap _offset . view _Wrapped'

-- TODO move
safeMinimum xs = if null xs then 0 else minimum xs
safeMaximum xs = if null xs then 0 else maximum xs

instance HasDuration (NScore a) where
  _duration x = _offset x .-. _onset x

instance Splittable a => Splittable (NScore a) where
  split = undefined
  -- TODO


-- |
-- Create a score from a list of notes.
--
-- This is a getter (rather than a function) for consistency:
--
-- @
-- [ (0 '<->' 1, 10)^.'note',
--   (1 '<->' 2, 20)^.'note',
--   (3 '<->' 4, 30)^.'note' ]^.'score'
-- @
--
-- @
-- 'view' 'score' $ 'map' ('view' 'note') [(0 '<->' 1, 1)]
-- @
--
-- Se also 'notes'.
--
score :: Getter [Note a] (Score a)
score = from unsafeNotes
{-# INLINE score #-}

-- |
-- View a 'Score' as a list of 'Note' values.
--
-- @
-- 'view' 'notes'                        :: 'Score' a -> ['Note' a]
-- 'set'  'notes'                        :: ['Note' a] -> 'Score' a -> 'Score' a
-- 'over' 'notes'                        :: (['Note' a] -> ['Note' b]) -> 'Score' a -> 'Score' b
-- @
--
-- @
-- 'preview'  ('notes' . 'each')           :: 'Score' a -> 'Maybe' ('Note' a)
-- 'preview'  ('notes' . 'element' 1)      :: 'Score' a -> 'Maybe' ('Note' a)
-- 'preview'  ('notes' . 'elements' odd)   :: 'Score' a -> 'Maybe' ('Note' a)
-- @
--
-- @
-- 'set'      ('notes' . 'each')           :: 'Note' a -> 'Score' a -> 'Score' a
-- 'set'      ('notes' . 'element' 1)      :: 'Note' a -> 'Score' a -> 'Score' a
-- 'set'      ('notes' . 'elements' odd)   :: 'Note' a -> 'Score' a -> 'Score' a
-- @
--
-- @
-- 'over'     ('notes' . 'each')           :: ('Note' a -> 'Note' b) -> 'Score' a -> 'Score' b
-- 'over'     ('notes' . 'element' 1)      :: ('Note' a -> 'Note' a) -> 'Score' a -> 'Score' a
-- 'over'     ('notes' . 'elements' odd)   :: ('Note' a -> 'Note' a) -> 'Score' a -> 'Score' a
-- @
--
-- @
-- 'toListOf' ('notes' . 'each')                :: 'Score' a -> ['Note' a]
-- 'toListOf' ('notes' . 'elements' odd)        :: 'Score' a -> ['Note' a]
-- 'toListOf' ('notes' . 'each' . 'filtered'
--              (\\x -> '_duration' x \< 2))  :: 'Score' a -> ['Note' a]
-- @
--
-- This is not an 'Iso', as the note list representation does not contain meta-data.
-- To construct a score from a note list, use 'score' or @'flip' ('set' 'notes') 'empty'@.
--
notes :: Lens (Score a) (Score b) [Note a] [Note b]
notes = _Wrapped . _2 . _Wrapped . sorted
  where
    sorted = iso (List.sortBy (Ord.comparing _onset)) (List.sortBy (Ord.comparing _onset))
-- notes = unsafeNotes
{-# INLINE notes #-}

-- -- |
-- -- View a score as a list of voices.
-- --
-- -- @
-- -- 'view' 'voices'                        :: 'Score' a -> ['Voice' a]
-- -- 'set'  'voices'                        :: ['Voice' a] -> 'Score' a -> 'Score' a
-- -- 'over' 'voices'                        :: (['Voice' a] -> ['Voice' b]) -> 'Score' a -> 'Score' b
-- -- @
-- --
-- -- @
-- -- 'preview'  ('voices' . 'each')           :: 'Score' a -> 'Maybe' ('Voice' a)
-- -- 'preview'  ('voices' . 'element' 1)      :: 'Score' a -> 'Maybe' ('Voice' a)
-- -- 'preview'  ('voices' . 'elements' odd)   :: 'Score' a -> 'Maybe' ('Voice' a)
-- -- @
-- --
-- -- @
-- -- 'set'      ('voices' . 'each')           :: 'Voice' a -> 'Score' a -> 'Score' a
-- -- 'set'      ('voices' . 'element' 1)      :: 'Voice' a -> 'Score' a -> 'Score' a
-- -- 'set'      ('voices' . 'elements' odd)   :: 'Voice' a -> 'Score' a -> 'Score' a
-- -- @
-- --
-- -- @
-- -- 'over'     ('voices' . 'each')           :: ('Voice' a -> 'Voice' b) -> 'Score' a -> 'Score' b
-- -- 'over'     ('voices' . 'element' 1)      :: ('Voice' a -> 'Voice' a) -> 'Score' a -> 'Score' a
-- -- 'over'     ('voices' . 'elements' odd)   :: ('Voice' a -> 'Voice' a) -> 'Score' a -> 'Score' a
-- -- @
-- --
-- -- @
-- -- 'toListOf' ('voices' . 'each')           :: 'Score' a -> ['Voice' a]
-- -- 'toListOf' ('voices' . 'elements' odd)   :: 'Score' a -> ['Voice' a]
-- -- 'toListOf' ('voices' . 'each' . 'filtered' (\\x -> '_duration' x \< 2)) :: 'Score' a -> ['Voice' a]
-- -- @
-- --
-- -- This is not an 'Iso', as the voice list representation does not contain meta-data.
-- -- To construct a score from a voice list, use 'score' or @'flip' ('set' 'voices') 'empty'@.
-- --
-- voices :: Lens (Score a) (Score b) [Voice a] [Voice b]
-- voices = unsafeVoices
-- {-# INLINE voices #-}

-- |
-- View a score as a list of notes.
--
-- This operation is /unsafe/ as it is only isomorphic up to meta-data equivalence,
-- i.e. it only works for values @x@ such that @'view' 'meta' x == 'mempty'@.
--
-- See also the safe (but more restricted) 'notes' and 'score'.
--
unsafeNotes :: Iso (Score a) (Score b) [Note a] [Note b]
unsafeNotes = _Wrapped . noMeta . _Wrapped . sorted
  where
    noMeta = iso extract return
    -- noMeta = iso (\(_,x) -> x) (\x -> (mempty,x))

    sorted = iso (List.sortBy (Ord.comparing _onset)) (List.sortBy (Ord.comparing _onset))

{-# INLINE unsafeNotes #-}

-- |
-- View a score as a single note.
--
singleNote :: Prism' (Score a) (Note a)
singleNote = unsafeNotes . single
{-# INLINE singleNote #-}
-- TODO make prism fail if score contains meta-data
-- (or else second prism law is not satisfied)


-- | Map with the associated time span.
mapScore :: (Note a -> b) -> Score a -> Score b
mapScore f = over (_Wrapped._2) (mapNScore f)
  where
    mapNScore f = over (_Wrapped.traverse) (extend f)

reifyScore :: Score a -> Score (Note a)
reifyScore = over (_Wrapped . _2 . _Wrapped) $ fmap duplicate

-- |
-- View a score as a list of events, i.e. time-duration-value triplets.
--
-- This is a convenient combination of 'notes' and 'event'.
--
-- @
-- 'events' = 'notes' . 'through' 'event' 'event'
-- @
--
events :: {-Transformable a => -}Lens (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
events = notes . through event event

unsafeEvents :: {-Transformable a => -}Iso (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
unsafeEvents = iso _getScore _score

_score :: [(Time, Duration, a)] -> Score a
_score = mconcat . fmap (uncurry3 event)
  where
    event t d x   = (delay (t .-. 0) . stretch d) (return x)

_getScore :: {-Transformable a => -}Score a -> [(Time, Duration, a)]
_getScore =
  fmap (\(view delta -> (t,d),x) -> (t,d,x)) .
  List.sortBy (Ord.comparing fst) .
  Foldable.toList .
  fmap (view $ from note) .
  reifyScore


-- | Map over the values in a score.
mapWithSpan :: (Span -> a -> b) -> Score a -> Score b
mapWithSpan f = mapScore (uncurry f . view (from note))

-- | Filter the values in a score.
filterWithSpan :: (Span -> a -> Bool) -> Score a -> Score a
filterWithSpan f = mapFilterWithSpan (partial2 f)

-- | Combination of 'mapEvents' and 'filterEvents'.
mapFilterWithSpan :: (Span -> a -> Maybe b) -> Score a -> Score b
mapFilterWithSpan f = mcatMaybes . mapWithSpan f

-- | Map over the values in a score.
mapEvents :: (Time -> Duration -> a -> b) -> Score a -> Score b
mapEvents f = mapWithSpan (uncurry f . view delta)

-- | Filter the values in a score.
filterEvents   :: (Time -> Duration -> a -> Bool) -> Score a -> Score a
filterEvents f = mapFilterEvents (partial3 f)

-- | Efficient combination of 'mapEvents' and 'filterEvents'.
mapFilterEvents :: (Time -> Duration -> a -> Maybe b) -> Score a -> Score b
mapFilterEvents f = mcatMaybes . mapEvents f




-- |
-- Process all simultaneous events.
--
-- Two events /a/ and /b/ are considered simultaneous if and only if they have the same
-- era, that is if @`era` a == `era` b@
--
mapSimultaneous :: Transformable a => (Score [a] -> Score [b]) -> Score a -> Score b
mapSimultaneous f = mscatter . f . simultaneous'

-- |
-- Merge all simultaneous events using their 'Semigroup' instance.
--
-- Two events /a/ and /b/ are considered simultaneous if and only if they have the same
-- era, that is if @`era` a == `era` b@
--
simultaneous :: (Transformable a, Semigroup a) => Score a -> Score a
simultaneous = fmap (sconcat . NonEmpty.fromList) . simultaneous'

-- |
-- Group simultaneous events as lists.
--
-- Two events /a/ and /b/ are considered simultaneous if and only if they have the same
-- era, that is if @`era` a == `era` b@
--
-- Note that 'simultaneous' is identical to 'simultaneous' @.@ 'fmap' 'return'
--
simultaneous' :: Transformable a => Score a -> Score [a]
simultaneous' sc = (^. from unsafeEvents) vs
    where
        -- es :: [Era]
        -- evs :: [[a]]
        -- vs :: [(Time, Duration, [a])]
        es  = List.nub $ eras sc
        evs = fmap (`chordEvents` sc) es
        vs  = zipWith (\(view delta -> (t,d)) a -> (t,d,a)) es evs
-- TODO handle meta


-- TODO (re)move these

eras :: Transformable a => Score a -> [Span]
eras sc = fmap getSpan . (^. events) $ sc

chordEvents :: Transformable a => Span -> Score a -> [a]
chordEvents era sc = fmap getValue . filter (\ev -> getSpan ev == era) . (^. events) $ sc

getValue :: (Time, Duration, a) -> a
getValue (t,d,a) = a

getSpan :: (Time, Duration, a) -> Span
getSpan (t,d,a) = t >-> d

-- TODO identical to: lens simultaneous' (flip $ mapSimultaneous . const)
-- wrap in something to preserve meta
simult :: Transformable a => Lens (Score a) (Score b) (Score [a]) (Score [b])
simult = iso simultaneous' mscatter

