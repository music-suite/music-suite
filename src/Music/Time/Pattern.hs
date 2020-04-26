{-# LANGUAGE ConstraintKinds,
                        DeriveDataTypeable,
                        DeriveFoldable,
                        DeriveFunctor,
                        DeriveTraversable,
                        GeneralizedNewtypeDeriving,
                        MultiParamTypeClasses,
                        NoMonomorphismRestriction,
                        RankNTypes,
                        StandaloneDeriving,
                        TupleSections,
                        TypeFamilies,
                        TypeOperators,
                        TypeApplications,
                        ViewPatterns,
                        OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-deprecations
  -fno-warn-unused-imports
  -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO FIXME
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- TODO FIXME
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-} -- TODO FIXME

module Music.Time.Pattern
  ( Pattern,
    newPattern,
    rhythmPattern,
    renderPattern,
    renderPatternsRel,
    renderPatternsAbs,
  )
where

import Prelude hiding (splitAt)
import Control.Lens (Rewrapped, Wrapped (..), (^.), _Wrapped, from, iso, over, view)
import Control.Monad (join)
import Data.AffineSpace
import Data.VectorSpace
import Data.Traversable (for)
import qualified Data.List
import Data.Bifunctor (first)
import Music.Pitch (IsPitch (..))
import Music.Pitch.Literal (c)
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Dynamics
import Music.Score.Articulation
import Music.Time.Aligned
import Music.Time.Event
import Music.Time.Juxtapose
import Music.Time.Note
import Music.Time.Placed
import Music.Time.Score
import Music.Time.Transform
import Music.Time.Voice
import Control.Monad.State.Strict
import Control.Monad.Except
import Iso.Deriving


-- sppar = pseq . fmap ppar

-- ppseq = ppar . fmap pseq

-- TODO move
-- TODO use proper math terminology here!

-- | Given a duration, return the cycle number and phase of a given time point relative origo.
--
-- >>> (2::Time) `cycleAndPhase` 3
-- (0,2)                              -- The point in time 2 happens in the 0-th 3-cycle at phase 2
-- >>> (-2::Time) `cycleAndPhase` 3
-- (-1,1)                             -- The point in time -1 happens in the (-1)-th 3-cycle at phase 1
cycleAndPhase :: Time -> Duration -> (Integer, Duration)
cycleAndPhase t d = (t .-. 0) `divModDur` d
{-# INLINE cycleAndPhase #-}

-- TODO move
-- TODO generalize to any vector
-- > v*^n ^+^ r = x where (n,r) = divModDur x v
divModDur :: Duration -> Duration -> (Integer, Duration)
divModDur x v = (n, r)
  where
    n = floor (x / v)
    -- r follows from (v*^n ^+^ r = x)
    r = x ^-^ (v *^ fromIntegral n)
{-# INLINE divModDur #-}


-- TODO move Abort elsewhere



-- |
-- Abort is like 'State' but allow short-circuiting the computation.
data Abort s a = Abort { runAbort :: s -> (Maybe a, s) }
  deriving (Functor)
  deriving (Applicative, Monad, MonadState s) via
    (ExceptT () (State s) `As1` Abort s)

-- | Abort the computation. The current state will be retained, but no
-- result will be returned.
abort :: Abort s a
abort = Abort $ \s -> (Nothing, s)

quit :: a -> Abort s a
quit x = Abort $ \s -> (Just x, s)

instance Inject (ExceptT () (State s) a) (Abort s a) where
  inj (ExceptT f) = Abort $ \s -> first eitherToMaybe $ runState f s
instance Project (ExceptT () (State s) a) (Abort s a) where
  prj (Abort f) = ExceptT $ StateT $ fmap (pure . first maybeToEither) f
instance Isomorphic (ExceptT () (State s) a) (Abort s a)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing = Left ()
maybeToEither (Just x) = Right x

-- TODO move this instance to iso-deriving
-- TODO undecidable
instance forall f g s . (forall x . Isomorphic (f x) (g x), MonadState s f) =>
  MonadState s (As1 f g) where
    state :: forall a . (s -> (a, s)) -> As1 f g a
    state k = As1 $ inj @(f a) @(g a) (state @s @f k)

t :: Abort Int ()
t = do
  !x <- get
  when (x > 10) abort
  put $ x + 1
  t

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
    split (SplitAtState { remainingDur, notesEaten })
      = ( splitNotesAt notesEaten remainingDur xs
        )

-- TODO proper test
testSplitAt :: Bool
testSplitAt = all id
  [ splitAt (-1) ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    == ([]^.voice, [(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)

  , splitAt 0 ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    == ([]^.voice, [(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)

  , splitAt 0.5 ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    == ([((0.5),'a')^.note]^.voice, [((0.5),'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)

  , splitAt 2.5 ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    == ([(1,'a')^.note,((1.5),'b')^.note]^.voice, [((0.5),'b')^.note,(1,'c')^.note]^.voice)

  , splitAt 5 ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    == ([(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice,[]^.voice)
  ]

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

-- TODO proper test
testGetCycles :: Bool
testGetCycles = all id
  -- At least one cycle
  [ getCycles (0 <-> 2) (0 <-> 4)         == Right (0, 2, 0)
  , getCycles (0 <-> 2) (1 <-> 4)         == Right (1, 1, 0)
  , getCycles (0 <-> 2) ((-1) <-> 5)      == Right (1, 2, 1)
  , getCycles (0 <-> 2) ((-9) <-> (-2.5)) == Right (1, 2, 1.5)
  , getCycles (0 <-> 2) (2 <-> 4)         == Right (0, 1, 0)

  -- No full cycles, s properly enclosed by a cycle
  , getCycles (0 <-> 2) (0.5 <-> 1.5)     == Left  (0.5)
  , getCycles (0 <-> 2) (0.6 <-> 1.6)     == Left  (0.6)
  , getCycles (0 <-> 2) ((-0.2) <-> (-0.1)) == Left (1.8)

  -- No full cycles, s NOT properly enclosed by a cycle
  , getCycles (1 <-> 3) (1 <-> 2)         == Right (0, 0, 1)
  , getCycles (1 <-> 3) (2 <-> 3)         == Right (1, 0, 0)
  , getCycles (1 <-> 3) (0 <-> 1)         == Right (1, 0, 0)
  , getCycles (1 <-> 3) ((-1) <-> 0)      == Right (0, 0, 1)
  ]

-- TODO get dir of (newtype wrapper, or inline):
instance (HasDuration a, Transformable a) => HasPosition [Aligned a] where
  _era xs = case foldMap (NonEmptyInterval . _era1) $ xs of
    EmptyInterval -> Nothing
    NonEmptyInterval x -> Just x

testC :: Span -> Aligned (Voice a) -> Either Duration (Duration, Integer, Duration)
testC s l = getCycles (_era1 l) s

testRenderLungaNEW :: Bool
testRenderLungaNEW = all id
  [ renderLungaNEW (0 <-> 2) aba
  == [(0 <-> 1,'a')^.event,(1 <-> 2,'b')^.event]^.score

  , renderLungaNEW (0 <-> 4) aba
  == [(0 <-> 1,'a')^.event,(1 <-> 3,'b')^.event,(3 <-> 4,'c')^.event]^.score

  , renderLungaNEW (0 <-> 1) aba
  == [(0 <-> 1,'a')^.event]^.score

  , renderLungaNEW ((-1)<->1) aba
  == [(-1 <-> 0,'c')^.event,(0 <-> 1,'a')^.event]^.score

  , renderLungaNEW ((0)<->1) aba1
  == [(0 <-> 1,'c')^.event]^.score

  , renderLungaNEW ((0.5)<->1.5) (aligned 0 0 $ stretch 2 $ pure 'c')
  == [(0.5 <-> 1.5,'c')^.event]^.score

  , renderLungaNEW (3<->8) aba
  == [(3 <-> 4,'c')^.event,(4 <-> 5,'a')^.event,(5 <-> 7,'b')^.event,(7 <-> 8,'c')^.event]^.score

  , renderLungaNEW (0<->2) (aligned 0 0 c :: Aligned (Voice Int))
  == [(0 <-> 1,0)^.event,(1 <-> 2,0)^.event]^.score

  , renderLungaNEW (0<->2) (stretch 2 $ aligned 0 0 c :: Aligned (Voice Int))
  == [(0 <-> 2,0)^.event]^.score

  , renderLungaNEW (0<->2) (delay 0.3 $ stretch 0.9 $ aligned 0 0 c :: Aligned (Voice Int))
  == [(0 <-> (3/10),0)^.event,((3/10) <-> (6/5),0)^.event,((6/5) <-> 2,0)^.event]^.score

  , renderLungaNEW ((-4) <-> 0) aba
  == [((-4) <-> (-3),'a')^.event,((-3) <-> (-1),'b')^.event,((-1) <-> 0,'c')^.event]^.score
  ]
  where
    aba =
      (aligned 0 0 $ [(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)
    aba1 =
      (aligned 1 0 $ [(1,'a')^.note,(2,'b')^.note,(1,'c')^.note]^.voice)

-- TODO replace the old ones, change type of pattern to [Aligned (Voice a)]
renderLungaNEW :: Span -> Aligned (Voice a) -> Score a
renderLungaNEW s l = case getCycles (_era1 l) s of
  Left phase ->
    renderAlignedVoice $ alignOnsetTo (view onset s)
      (takeM (_duration s) $ dropM phase $ unAlign l)
  Right (introDur, numCycles, outroDur) -> let
      intro  = alignOnsetTo (view onset s) (dropM (_duration l - introDur) $ unAlign l)
      outro  = alignOffsetTo (view offset s) (takeM outroDur $ unAlign l)
      cycles = times (fromIntegral numCycles) $
              pure $ alignOnsetTo (view onset s .+^ introDur) (unAlign l)
    in mconcat $ fmap renderAlignedVoice $ [intro] ++ cycles ++ [outro]
  where
    alignOnsetTo t = aligned t 0
    alignOffsetTo t = aligned t 1

-- List of repeated voices
-- TODO is this isomorphic to Tidal's pattern (i.e. Span -> Score a)
newtype Pattern a
  = Pattern {getPattern :: [Aligned (Voice a)]}
  deriving (Semigroup, Monoid, Transformable, Functor, Foldable, Traversable)

instance (IsPitch a) => IsPitch (Pattern a) where
  fromPitch = pureP . fromPitch

type instance Articulation (Pattern a) = Articulation a

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
renderPattern (Pattern xs) s = mconcat $ fmap (renderLungaNEW s) xs


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
