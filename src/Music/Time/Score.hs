
{-# LANGUAGE CPP                        #-}
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

module Music.Time.Score (
  ) where

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Ratio
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace

import           Music.Time.Types
import           Music.Time.Transform
import           Music.Time.Position
import           Music.Time.Duration
import           Music.Time.Split
import           Music.Time.Reverse
import           Music.Time.Note
import           Music.Time.Stretched
import           Music.Time.Voice

-----
import Control.Monad.Compose
import Music.Score.Util
import Data.Fixed
import           Data.Default
import           Data.Ratio

import           Control.Applicative
import           Control.Arrow                (first, second, (***), (&&&))
import qualified Control.Category
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Lens                 hiding (Indexable, Level, above,
                                               below, index, inside, parts,
                                               reversed, transform, (|>), (<|))
import           Control.Monad
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Distributive
import           Data.Foldable                (Foldable)
import qualified Data.Foldable                as Foldable
import           Data.Functor.Rep
import qualified Data.List
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Maybe
import           Data.NumInstances
import           Data.Semigroup               hiding ()
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Traversable             (Traversable)
import qualified Data.Traversable             as T
import           Data.Typeable
import           Data.VectorSpace hiding (Sum(..))
import           Music.Dynamics.Literal
import           Music.Pitch.Literal

import qualified Data.Ratio                   as Util_Ratio
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Ord as Ord
-----



type ScoreNote a = Note a

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
newtype Score a = Score { getScore :: [ScoreNote a] }
  deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

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

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (Score a) where
  type Unwrapped (Score a) = [ScoreNote a]
  _Wrapped' = iso getScore Score

instance Rewrapped (Score a) (Score b)

instance Applicative Score where
  pure  = return
  (<*>) = ap

instance Monad Score where
  return = (^. _Unwrapped) . pure . pure
  xs >>= f = (^. _Unwrapped) $ mbind ((^. _Wrapped') . f) ((^. _Wrapped') xs)

instance Alternative Score where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Score where
  mzero = mempty
  mplus = mappend

instance FunctorWithIndex Span Score where
  imap = mapWithSpan

instance FoldableWithIndex Span Score where
  -- TODO

instance TraversableWithIndex Span Score where
  itraverse = undefined
  -- TODO

instance IsPitch a => IsPitch (Score a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Score a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Score a) where
  fromDynamics = pure . fromDynamics

instance Transformable (Score a) where
  transform t (Score xs) = Score (fmap (transform t) xs)

instance Reversible a => Reversible (Score a) where
  rev (Score xs) = Score (fmap rev xs)

instance HasPosition (Score a) where
  _onset = Foldable.minimum . fmap _onset . view _Wrapped'
  _offset = Foldable.maximum . fmap _offset . view _Wrapped'

instance HasDuration (Score a) where
  _duration x = _offset x .-. _onset x

instance Splittable a => Splittable (Score a) where
  -- TODO

-- TODO
-- instance HasMeta (Score a) where
  -- meta = error "Not implemented: meta"

-- TODO
-- type instance Pitch (Score a) = Pitch a
-- type instance SetPitch g (Score a) = Score (SetPitch g a)
-- 
-- instance (HasPitches a b) => HasPitches (Score a) (Score b) where
--   pitches = _Wrapped . traverse . _Wrapped . whilstL pitches
-- 
-- type instance Part (Score a) = Part a
-- type instance SetPart g (Score a) = Score (SetPart g a)
-- 
-- instance (HasParts a b) => HasParts (Score a) (Score b) where
--   parts = _Wrapped . traverse . _Wrapped . whilstL parts
-- 
-- type instance Dynamic (Score a) = Dynamic a
-- type instance SetDynamic g (Score a) = Score (SetDynamic g a)
-- 
-- instance HasDynamics a b => HasDynamics (Score a) (Score b) where
--   dynamics = _Wrapped . traverse . _Wrapped . whilstL dynamics
-- 
-- type instance Articulation (Score a) = Articulation a
-- type instance SetArticulation g (Score a) = Score (SetArticulation g a)
-- 
-- instance (HasArticulations a b) => HasArticulations (Score a) (Score b) where
--   articulations = _Wrapped . traverse . _Wrapped . whilstL articulations


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
score = to $ flip (set notes) empty
{-# INLINE score #-}

-- |
-- View a score as a list of notes.
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
notes = unsafeNotes
{-# INLINE notes #-}

-- |
-- View a score as a list of voices.
--
-- @
-- 'view' 'voices'                        :: 'Score' a -> ['Voice' a]
-- 'set'  'voices'                        :: ['Voice' a] -> 'Score' a -> 'Score' a
-- 'over' 'voices'                        :: (['Voice' a] -> ['Voice' b]) -> 'Score' a -> 'Score' b
-- @
--
-- @
-- 'preview'  ('voices' . 'each')           :: 'Score' a -> 'Maybe' ('Voice' a)
-- 'preview'  ('voices' . 'element' 1)      :: 'Score' a -> 'Maybe' ('Voice' a)
-- 'preview'  ('voices' . 'elements' odd)   :: 'Score' a -> 'Maybe' ('Voice' a)
-- @
--
-- @
-- 'set'      ('voices' . 'each')           :: 'Voice' a -> 'Score' a -> 'Score' a
-- 'set'      ('voices' . 'element' 1)      :: 'Voice' a -> 'Score' a -> 'Score' a
-- 'set'      ('voices' . 'elements' odd)   :: 'Voice' a -> 'Score' a -> 'Score' a
-- @
--
-- @
-- 'over'     ('voices' . 'each')           :: ('Voice' a -> 'Voice' b) -> 'Score' a -> 'Score' b
-- 'over'     ('voices' . 'element' 1)      :: ('Voice' a -> 'Voice' a) -> 'Score' a -> 'Score' a
-- 'over'     ('voices' . 'elements' odd)   :: ('Voice' a -> 'Voice' a) -> 'Score' a -> 'Score' a
-- @
--
-- @
-- 'toListOf' ('voices' . 'each')           :: 'Score' a -> ['Voice' a]
-- 'toListOf' ('voices' . 'elements' odd)   :: 'Score' a -> ['Voice' a]
-- 'toListOf' ('voices' . 'each' . 'filtered' (\\x -> '_duration' x \< 2)) :: 'Score' a -> ['Voice' a]
-- @
--
-- This is not an 'Iso', as the voice list representation does not contain meta-data.
-- To construct a score from a voice list, use 'score' or @'flip' ('set' 'voices') 'empty'@.
--
voices :: Lens (Score a) (Score b) [Voice a] [Voice b]
voices = unsafeVoices
{-# INLINE voices #-}

-- |
-- View a score as a list of phrases.
-- 
phrases :: Lens (Score a) (Score b) [[Voice a]] [[Voice b]]
phrases = error "Not implemented: phrases"
{-# INLINE phrases #-}

unsafeNotes :: Iso (Score a) (Score b) [Note a] [Note b]
unsafeNotes = _Wrapped
{-# INLINE unsafeNotes #-}

unsafeVoices :: Iso (Score a) (Score b) [Voice a] [Voice b]
unsafeVoices = error "Not impl"
{-# INLINE unsafeVoices #-}

-- |
-- View a score as a single note.
-- 
singleNote :: Prism' (Score a) (Note a)
singleNote = unsafeNotes . single
{-# INLINE singleNote #-}
-- TODO make prism fail if score contains meta-data
-- (or else second prism law is not satisfied)

-- |
-- View a score as a single voice.
-- 
singleVoice :: Prism' (Score a) (Voice a)
singleVoice = unsafeVoices . single
{-# INLINE singleVoice #-}
-- TODO make prism fail if score contains meta-data
-- (or else second prism law is not satisfied)

{-
-- |
-- View a score as a single phrase.
-- 
singlePhrase :: Prism' (Score a) (Phrase () a)
singlePhrase = error "Not implemented: singlePhrase"
-}

-- | Map with the associated time span.
mapScore :: (Note a -> b) -> Score a -> Score b
mapScore f = error "Not implemented: singleNote"


reifyScore :: Score a -> Score (Note a)
reifyScore = over _Wrapped $ fmap duplicate

events :: Transformable a => Iso (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
events = iso _getScore _score

_score :: [(Time, Duration, a)] -> Score a
_score = mconcat . fmap (uncurry3 event)
  where
    event t d x   = (delay (t .-. 0) . stretch d) (return x)

_getScore :: Transformable a => Score a -> [(Time, Duration, a)]
_getScore =
  fmap (\(view delta -> (t,d),x) -> (t,d,x)) .
  List.sortBy (Ord.comparing fst) .
  Foldable.toList .
  fmap (view $ from note) .
  reifyScore

scoreToVoice :: Transformable a => Score a -> Voice (Maybe a)
scoreToVoice = view voice . fmap (view stretched) . fmap throwTime . addRests . (^. events)
  where
     throwTime (t,d,x) = (d,x)
     addRests = concat . snd . List.mapAccumL g 0
       where
         g u (t, d, x)
           | u == t  = (t .+^ d, [(t, d, Just x)])
           | u <  t  = (t .+^ d, [(u, t .-. u, Nothing), (t, d, Just x)])
           | otherwise = error "addRests: Strange prevTime"

















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

