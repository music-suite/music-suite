{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-matches #-}
module Music.Time.Pattern
  ( Pattern,
    newPattern,
    rhythmPattern,
    renderPattern,
    renderPatternsRel,
    renderPatternsAbs,

  -- TODO hide?
    renderLunga,
    getCycles,
    splitAt,
  )
where

import Prelude hiding (splitAt)
import Control.Lens ((^.), from, view)
import Control.Monad.Abort
import Data.AffineSpace
import Data.VectorSpace
import Data.Traversable (for)
import qualified Data.List
import Music.Pitch (IsPitch (..))
import Music.Pitch.Literal (c)
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Dynamics
import Music.Score.Articulation
import Music.Time.Aligned
import Music.Time.Juxtapose
import Music.Time.Note
import Music.Time.Score
import Music.Time.Voice
import Control.Monad.State.Strict


-- TODO move divModDur
-- TODO generalize to any vector
-- > v*^n ^+^ r = x where (n,r) = divModDur x v
divModDur :: Duration -> Duration -> (Integer, Duration)
divModDur x v = (n, r)
  where
    n = floor (x / v)
    -- r follows from (v*^n ^+^ r = x)
    r = x ^-^ (v *^ fromIntegral n)
{-# INLINE divModDur #-}




takeM, dropM :: Duration -> Voice a -> Voice a
takeM d = fst . splitAt d
dropM d = snd . splitAt d

data SplitAtState = SplitAtState { remainingDur :: Duration, notesEaten :: Integer }
  deriving (Show)

fromVoice :: Voice a -> [Note a]
toVoice :: [Note a] -> Voice a
(fromVoice, toVoice) = (view notes, view voice)

fromNote :: Note a -> (Duration, a)
toNote :: (Duration, a) -> Note a
(fromNote, toNote) = (view $ from note, view note)

-- | Duration must be >0
splitNote :: Duration -> Note a -> (Note a, Maybe (Note a))
splitNote d' (fromNote -> (d, x)) = if d' < d
  then (toNote (d', x), Just $ toNote (d - d', x))
  else (toNote (d, x), Nothing)

-- |
-- Take the given number of notes and up to d of the next note.
splitNotesAt :: Integer -> Duration -> Voice a -> (Voice a, Voice a)
splitNotesAt n d xs
  | d <= 0 =
    case Data.List.splitAt (fromIntegral n) (fromVoice xs) of
      (ys,zs) ->
        (toVoice ys, toVoice zs)
  | otherwise =
    case fmap Data.List.uncons $ Data.List.splitAt (fromIntegral n) (fromVoice xs) of
      (ys, Nothing) -> (toVoice ys, mempty)
      (ys, Just (z, zs)) -> case splitNote d z of
        (m, Nothing) -> (toVoice (ys++[m]), toVoice zs)
        (m, Just o)  -> (toVoice (ys++[m]), toVoice (o : zs))

-- TODO move splitAt to Time.Voice

getSplitPoint :: Duration -> Voice a -> SplitAtState
getSplitPoint d xs =
  snd $ flip runAbort (SplitAtState { remainingDur = d, notesEaten = 0 }) $
    for (view notes xs) $ \note -> do
      SplitAtState { remainingDur, notesEaten } <- get
      if _duration note > remainingDur
        then
          abort
        else
          put $ SplitAtState
            { remainingDur = remainingDur - _duration note,
            notesEaten = notesEaten + 1 }

-- TODO property tests:
--
--    (\x y -> _duration + _duration y) (splitAt d xs) = _duration xs
--
--    _duration (takeM d xs) = min d (_duration xs)
splitAt :: Duration -> Voice a -> (Voice a, Voice a)
splitAt d xs = split $ getSplitPoint d xs
  where
    split SplitAtState{remainingDur, notesEaten} = splitNotesAt notesEaten remainingDur xs

--
modDur :: Duration -> Duration -> Duration
modDur a b = snd $ a `divModDur` b

getCycles :: Span -> Span -> Either Duration (Duration, Integer, Duration)
getCycles ts s
  | _duration ts <= 0 = error "getCycles: Duration of repeated span must be >0"
  --  | _duration s < _duration ts = (0, 0, _duration s)
  | otherwise = if cycles >= 0
      then Right (rem, cycles, phase)
      else Left (_duration ts - rem)
  where
    phase = (view offset s .-. view offset ts) `modDur` _duration ts
    (cycles, rem) = (_duration s - phase) `divModDur` _duration ts


newtype Lunga a = Lunga { getLunga :: [Aligned a] }
  deriving (Functor, Foldable, Traversable, Transformable, Semigroup, Monoid)

instance (HasDuration a, Transformable a) => HasPosition (Lunga a) where
  _era (Lunga xs) = case foldMap (NonEmptyInterval . _era1) xs of
    EmptyInterval -> Nothing
    NonEmptyInterval x -> Just x

renderLunga :: Span -> Aligned (Voice a) -> Score a
renderLunga s l = case getCycles (_era1 l) s of
  Left phase ->
    renderAlignedVoice $ alignOnsetTo (view onset s)
      (takeM (_duration s) $ dropM phase $ unAlign l)
  Right (introDur, numCycles, outroDur) -> let
      intro  = alignOnsetTo (view onset s) (dropM (_duration l - introDur) $ unAlign l)
      outro  = alignOffsetTo (view offset s) (takeM outroDur $ unAlign l)
      cycles = getLunga $ times (fromIntegral numCycles) $
              Lunga $ pure $ alignOnsetTo (view onset s .+^ introDur) (unAlign l)
    in mconcat $ fmap renderAlignedVoice $ [intro] ++ cycles ++ [outro]
  where
    alignOnsetTo t = aligned t 0
    alignOffsetTo t = aligned t 1

-- List of repeated voices
-- TODO is this isomorphic to Tidal's pattern (i.e. Span -> Score a)
newtype Pattern a
  = Pattern {_getPattern :: [Aligned (Voice a)]}
  deriving (Semigroup, Monoid, Transformable, Functor, Foldable, Traversable)

instance (IsPitch a) => IsPitch (Pattern a) where
  fromPitch = pureP . fromPitch

type instance GetArticulation (Pattern a) = GetArticulation a

type instance SetArticulation b (Pattern a) = Pattern (SetArticulation b a)

instance HasArticulations a b => HasArticulations (Pattern a) (Pattern b) where
  articulations = traverse . articulations

type instance Pitch (Pattern a) = Pitch a

type instance SetPitch b (Pattern a) = Pattern (SetPitch b a)

instance HasPitches a b => HasPitches (Pattern a) (Pattern b) where
  pitches = traverse . pitches

type instance Part (Pattern a) = Part a

type instance SetPart b (Pattern a) = Pattern (SetPart b a)

instance HasParts a b => HasParts (Pattern a) (Pattern b) where
  parts = traverse . parts

type instance GetDynamic (Pattern a) = GetDynamic a

type instance SetDynamic b (Pattern a) = Pattern (SetDynamic b a)

instance HasDynamics a b => HasDynamics (Pattern a) (Pattern b) where
  dynamics = traverse . dynamics

-- What sort of Applicative is Pattern?
-- instance Applicative Pattern where
--   pure x = newPattern' (pure x) (pure x)
--   Pattern fs <*> Pattern xs = Pattern (fs <*> xs)

pureP :: a -> Pattern a
pureP = newPattern . pure

newPattern :: Voice a -> Pattern a
newPattern v = Pattern [aligned 0 0 v]

rhythmPattern :: IsPitch a => [Duration] -> Pattern a
rhythmPattern a = newPattern $ fmap (const c) $ a ^. durationsAsVoice

-- TODO variant that returns [Aligned (Voice a)]
renderPattern :: Pattern a -> Span -> Score a
renderPattern (Pattern xs) s = mconcat $ fmap (renderLunga s) xs


-- |
-- Renders each pattern over one cycle (0<->1) and stretches to the duration of the surrounding note.
--
-- Each note triggers exactly /one/ cycle of the pattern (frequency = 1), starting at the beginning of the pattern (phase =
-- 0).
renderPatternsRel :: Score (Pattern a) -> Score a
renderPatternsRel = join . fmap (flip renderPattern zeroV)

-- | Renders each pattern in the span of its note.
--
-- This means that notes of different onset and duration may trigger a different number of cycles (frequency), with
-- different starting point in the pattern (phase).
renderPatternsAbs :: Score (Pattern a) -> Score a
renderPatternsAbs = join . mapWithSpan (\s -> transform (negateV s) . flip renderPattern s)
-- Note: We can not change the span of a note using mapWithSpan, so we transform the result to position (0<->1)
-- and trust join to put it back in the same position it was rendered.
