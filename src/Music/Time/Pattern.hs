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

instance (IsPitch a) => IsPitch (Pattern a) where
  fromPitch = pureP . fromPitch

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

-- |
-- An infinitely repeating monophonic pattern.
newtype Lunga a = Lunga (Duration, Voice a, Voice a)
  deriving (Functor, Foldable, Traversable)

instance Applicative Lunga where

  pure x = Lunga (mempty, pure x, pure x)

  Lunga (fd, fa, fb) <*> Lunga (xd, xa, xb) = Lunga (fd <> xd, fa <*> xa, fb <*> xb)

instance Transformable (Lunga a) where
  transform t (Lunga (d, a, b)) = Lunga (transform t d, transform t a, transform t b)

type instance Pitch (Lunga a) = Pitch a

type instance SetPitch b (Lunga a) = Lunga (SetPitch b a)

type instance Part (Lunga a) = Part a

type instance SetPart b (Lunga a) = Lunga (SetPart b a)

type instance Articulation (Lunga a) = Articulation a

type instance SetArticulation b (Lunga a) = Lunga (SetArticulation b a)

instance HasArticulations a b => HasArticulations (Lunga a) (Lunga b) where
  articulations = traverse . articulations

instance HasPitches a b => HasPitches (Lunga a) (Lunga b) where
  {-
    pitches visit (Lunga (dur, back, front)) = do
      back' <- pitches visit $ back
      front' <- pitches visit $ front
      pure $ Lunga (dur, front', back')
  -}
  pitches = traverse . pitches

instance HasParts a b => HasParts (Lunga a) (Lunga b) where
  parts = traverse . parts

-- nominalDuration :: Lunga a -> Duration
-- nominalDuration (Lunga (d, _, _)) = d

-- Transform a voice (assumed to be finite) into a lunga.
newLunga :: Voice a -> Lunga a
newLunga v = Lunga (v ^. duration, cycleV $ over notes reverse v, cycleV v)
  where
    cycleV = over notes cycle

{-
TODO write a version of renderLungaSpan'' that can handle *any* span
  0 < on < off
  on < 0 < off
  on < off < 0

We know that the origo of the lunga is 0. We also know the *nominal duration* d.
Hence we know that the repeating pattern starts and ends at [-nd..-1d,0,1d,2d..nd].
When requested to render a span s, we want to use *as short voices as possible*.
  If s starts at nd+0 where n is integral, simply take (s^.duration) from the front voice.
  If s starts at nd+m where n is integral and 0 < m < d,
    EITHER
      take d-m from the back voice, put together with the result of taking ((s^.duration) + m - d) from the front voice.
    OR
      drop m from the front voice, then take (d-m), put together...

>>> (1::Time) `cycleAndPhase` 3
(0,1)
>>> (7::Time) `cycleAndPhase` 3
(2,1)

Render the cycles 0<->3, 3<->6, an extra cycle of (takeM 1), finally drop 1

>>>
>>> (1::Time) `cycleAndPhase` 3
(0,1)
>>> (2::Time) `cycleAndPhase` 3
(0,2)
Render the cycles -, an extra cycle of (takeM 2), finally drop 1

-}




renderLunga :: Span -> Lunga a -> Aligned (Voice a)
renderLunga s (Lunga (d, _, b))
  | d < 0 = error "renderLunga: Negative (nominal) duration"
  | otherwise =
    aligned (s ^. position 0) 0 $ voca
  where
    (n1, r1) = (s ^. onset) `cycleAndPhase` d
    (n2, r2) = (s ^. offset) `cycleAndPhase` d
    fullCycles = fromIntegral $ n2 - n1 -- 0 or greater
    voca = dropM r1 $ takeM (d ^* fullCycles) b <> takeM r2 b


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

getCycles :: Span -> Span -> (Duration, Integer, Duration)
getCycles ts s
  | _duration ts <= 0 = error "getCycles: Duration of repeated span must be >0"
  --  | _duration s < _duration ts = (0, 0, _duration s)
  | otherwise = (rem, cycles, phase)
  where
    phase = (view offset s .-. view offset ts) `modDur` _duration ts
    (cycles, rem) = (_duration s - phase) `divModDur` _duration ts

-- TODO proper test
testGetCycles :: Bool
testGetCycles = all id
  [ getCycles (0 <-> 2) (0 <-> 4)         == (0, 2, 0)
  , getCycles (0 <-> 2) (1 <-> 4)         == (1, 1, 0)
  , getCycles (0 <-> 2) ((-1) <-> 5)      == (1, 2, 1)
  , getCycles (0 <-> 2) ((-9) <-> (-2.5)) == (1, 2, 1.5)
  , getCycles (0 <-> 2) (2 <-> 4)         == (0, 1, 0)
  , getCycles (1 <-> 3) (1 <-> 2)         == (0, 0, 1)
  , getCycles (1 <-> 3) (2 <-> 3)         == (1, 0, 0)
  , getCycles (1 <-> 3) (0 <-> 1)         == (1, 0, 0)
  , getCycles (1 <-> 3) ((-1) <-> 0)      == (0, 0, 1)
  ]

-- TODO get dir of (newtype wrapper, or inline):
instance (HasDuration a, Transformable a) => HasPosition [Aligned a] where
  _era xs = case foldMap (NonEmptyInterval . _era1) $ xs of
    EmptyInterval -> Nothing
    NonEmptyInterval x -> Just x

testC :: Span -> Aligned (Voice a) -> (Duration, Integer, Duration)
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
renderLungaNEW s l =
  -- FIXME if too short, must take phase into account!
  mconcat $ fmap renderAlignedVoice $ [intro] ++ cycles ++ [outro]
  where
    intro  = alignOnsetTo (view onset s) (dropM (_duration l - introDur) $ unAlign l)
    outro  = alignOffsetTo (view offset s) (takeM outroDur $ unAlign l)
    cycles = times (fromIntegral numCycles) $
      pure $ alignOnsetTo (view onset s .+^ introDur) (unAlign l)
    (introDur, numCycles, outroDur) = getCycles (_era1 l) s

    alignOnsetTo t = aligned t 0
    alignOffsetTo t = aligned t 1

-- List of repeated voices
-- TODO is this isomorphic to Tidal's pattern (i.e. Span -> Score a)
newtype Pattern a
  = Pattern {getPattern :: [Placed (Lunga a)]} -- origo, pattern
  deriving (Semigroup, Monoid, Transformable, Functor, Foldable, Traversable)


type instance Articulation (Pattern a) = Articulation a

type instance SetArticulation b (Pattern a) = Pattern (SetArticulation b a)

instance HasArticulations a b => HasArticulations (Pattern a) (Pattern b) where
  articulations = iso getPattern Pattern . articulations

type instance Pitch (Pattern a) = Pitch a

type instance SetPitch b (Pattern a) = Pattern (SetPitch b a)

instance HasPitches a b => HasPitches (Pattern a) (Pattern b) where
  pitches = iso getPattern Pattern . pitches

type instance Part (Pattern a) = Part a

type instance SetPart b (Pattern a) = Pattern (SetPart b a)

instance HasParts a b => HasParts (Pattern a) (Pattern b) where
  parts = traverse . parts

-- What sort of Applicative is Pattern?
-- instance Applicative Pattern where
--   pure x = newPattern' (pure x) (pure x)
--   Pattern fs <*> Pattern xs = Pattern (fs <*> xs)

pureP :: a -> Pattern a
pureP = newPattern . pure

newPattern :: Voice a -> Pattern a
newPattern v = Pattern [pure $ newLunga v]

rhythmPattern :: IsPitch a => [Duration] -> Pattern a
rhythmPattern a = newPattern $ fmap (const c) $ a ^. durationsAsVoice

-- TODO variant that returns [Aligned (Voice a)]
renderPattern :: Pattern a -> Span -> Score a
renderPattern (Pattern ((unzip . fmap (^. from placed)) -> (origos, lungas))) s =
  mconcat $
    zipWith (renderLunga' s) origos lungas

renderLunga' :: Span -> Time -> Lunga a -> Score a
renderLunga' s t = renderAlignedVoice . delay' t . renderLunga (delay' t s)
  where
    -- TODO terminology here is not super-nice
    -- Work out whether this or renderLunga is the semantic function of lunga
    delay' t = delay (0 .-. t)

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
