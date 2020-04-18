{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Provides phrase-wise traversal.
module Music.Score.Phrases
  ( -- * HasPhrases class
    HasPhrases (..),
    HasPhrases',
    phrases,
    phrases',

    -- * Phrase types etc
    Phrase,
    MVoice,
    PVoice,

    -- ** Utility
    mVoicePVoice,
    singleMVoice,
    -- oldSingleMVoice,
    mapPhrasesWithPrevAndCurrentOnset,
  )
where

import BasePrelude hiding ((<>), Dynamic, first, second)
import Control.Lens hiding ((&), rewrite)
import Control.Monad.Plus
import Data.AffineSpace
import Data.AffineSpace.Point.Offsets (offsetPoints)
import Data.Bifunctor
import Data.Colour.Names as Color
import Data.Functor.Context
import Data.Functor.Couple
import qualified Data.List as List
import Data.Semigroup
import Data.VectorSpace hiding (Sum (..))
import Music.Score.Part
import Music.Time.Aligned
import Music.Time.Event
import Music.Time.Internal.Convert ()
import Music.Time.Internal.Util
import Music.Time.Note
import Music.Time.Placed
import Music.Time.Duration
import Music.Time.Score
import Music.Time.Track
import Music.Time.Voice

-- |
-- For a phrase, we simply use a voice without rests.
--
-- To represent a sequence of phrases we provide two equivalent representations:
--
-- * 'MVoice' is a sequence of notes/chords or rests. All consecutive non-rests consitute a phrase.
--
-- * 'PVoice' is a sequence of phrases or durations.
type Phrase a = Voice a

-- |
-- A sequence of phrases or rests, represented as notes or rests.
--
-- Each consecutive sequence of non-rest elements is considered to be a phrase.
-- For a more explicit representation of the phrase structure, see 'PVoice'.
type MVoice a = Voice (Maybe a)

-- |
-- A sequence of phrases or rests, represented with explicit phrase structure.
type PVoice a = [Either Duration (Phrase a)]

-- |
-- A sequence of phrases or rests, represented as phrases with an explicit onset.
--
-- This is only isomorphic to 'MVoice' (and 'PVoice') up to onset equivalence.
type TVoice a = Track (Phrase a)

-- |
-- Class of structures that can be traversed "by phrase".
--
-- A /phrase/ in this context is a sequence of consecutive notes played by the
-- same performer, with no rests in between. Phrase traverals are used for
-- attaching articulation marks, slurs and so on, but can also be used separately.
class HasPhrases s t a b | s -> a, t -> b, s b -> t, t a -> s where
  mvoices :: Traversal s t (MVoice a) (MVoice b)

-- | Traverses all phrases in a voice.
instance HasPhrases (MVoice a) (MVoice b) a b where
  mvoices = id

instance HasPhrases (PVoice a) (PVoice b) a b where
  -- Note: This is actually OK in 'phr', as that just becomes (id . each . _Right)
  mvoices = from mVoicePVoiceIgnoringMeta

-- | Traverses all phrases in each voice, using 'extracted'.
-- TODO get rid of UndecidableInstances
instance (HasParts' a, Ord (Part a), a ~ b) => HasPhrases (Score a) (Score b) a b where
  mvoices = extracted . each . singleMVoice

{-
instance HasPhrases (Note a) (Note b) a b where
  mvoices f (Note (d, x)) = f (Voice [
-}

{-
TODO get rid of the (a ~ b) restriction

To understand why it is there in the first place:

- Consider a score where some part has overlapping events

- The overlap means that singleMVoice will fail to match that particular part
  (this is why singleMVoice is a Prism and not an Iso/Lens).

- Consequently, the phrase traversal may not be able to modify certain parts, and must
  therefore retain them in their original form.

To get around this, we would need a version of singleMVoice that included voice separation.
That way, we would have the option to break up arbitrary scores into parts for them
purpose of the phrase traversal.
-}

type HasPhrases' s a = HasPhrases s s a a

-- |
-- A simple generic phrase-traversal.
phrases' :: HasPhrases' s a => Traversal' s (Phrase a)
phrases' = phrases

-- |
-- A generic phrase-traversal.
phrases :: HasPhrases s t a b => Traversal s t (Phrase a) (Phrase b)
phrases = mvoices . mVoicePVoice . each . _Right

-- |
-- View an 'MVoice' as a 'PVoice'.
mVoicePVoice :: Lens (MVoice a) (MVoice b) (PVoice a) (PVoice b)
mVoicePVoice = mVoicePVoiceIgnoringMeta

-- TODO meta

-- |
-- View an 'MVoice' as a 'PVoice' and vice versa.
--
-- This a valid 'Iso' up to meta-data equivalence.
mVoicePVoiceIgnoringMeta :: Iso (MVoice a) (MVoice b) (PVoice a) (PVoice b)
mVoicePVoiceIgnoringMeta = iso mvoiceToPVoice pVoiceToMVoice
  where
    mvoiceToPVoice :: MVoice a -> PVoice a
    mvoiceToPVoice =
      map
        ( bimap voiceToRest voiceToPhrase
            . bimap (^. from pairs) (^. from pairs)
        )
        . groupDiff (isJust . snd)
        . view pairs
    voiceToRest :: MVoice a -> Duration
    voiceToRest = sumOf (pairs . each . _1) . fmap (\x -> assert (isNothing x) x)
    -- TODO just _duration

    voiceToPhrase :: MVoice a -> Phrase a
    voiceToPhrase = fmap fromJust
    pVoiceToMVoice :: (PVoice a) -> MVoice a
    pVoiceToMVoice = mconcat . fmap (either restToVoice phraseToVoice)
    restToVoice :: Duration -> MVoice a
    restToVoice d = stretch d $ pure Nothing
    phraseToVoice :: Phrase a -> MVoice a
    phraseToVoice = fmap Just

singleMVoice :: (a ~ b) => Prism (Score a) (Score b) (MVoice a) (MVoice b)
singleMVoice = prism' voiceToScore' scoreToVoice
  where
    voiceToScore :: Voice a -> Score a
    voiceToScore = renderAlignedVoice . aligned 0 0
    voiceToScore' :: MVoice b -> Score b
    voiceToScore' = mcatMaybes . voiceToScore
    scoreToVoice :: Score a -> Maybe (MVoice a)
    scoreToVoice sc
      | hasOverlappingEvents sc = Nothing
      | otherwise =
        Just . (^. voice) . fmap (^. note) . fmap throwTime . addRests
          .
          -- TODO
          List.sortBy (comparing (^. _1))
          -- end TODO
          . (^. triples)
          $ sc
      where
        throwTime (t, d, x) = (d, x)
        addRests = concat . snd . List.mapAccumL g 0
          where
            g u (t, d, x)
              | u == t = (t .+^ d, [(t, d, Just x)])
              | u < t = (t .+^ d, [(u, t .-. u, Nothing), (t, d, Just x)])
              | otherwise = error "scoreToVoice: Impossible!" -- Because of overlapping events guard

mapPhrasesWithPrevAndCurrentOnset :: HasPhrases s t a b => (Maybe (Time, Phrase a) -> Time -> Phrase a -> Phrase b) -> s -> t
mapPhrasesWithPrevAndCurrentOnset f = over (mvoices . mVoiceTVoice) (withPrevAndCurrentOnset f)

withPrevAndCurrentOnset :: (Maybe (Time, a) -> Time -> a -> b) -> Track a -> Track b
withPrevAndCurrentOnset f = over placeds (fmap (\(x, y, z) -> fmap (f (fmap placedOnset x) (fst $ placedOnset y)) y) . withPrevNext)
  where
    placedOnset :: Placed a -> (Time, a)
    placedOnset = view (from placed)

mVoiceTVoice :: Lens (MVoice a) (MVoice b) (TVoice a) (TVoice b)
mVoiceTVoice = mVoicePVoice . pVoiceTVoice

pVoiceTVoice :: Lens (PVoice a) (PVoice b) (TVoice a) (TVoice b)
pVoiceTVoice = lens pVoiceToTVoice (flip tVoiceToPVoice)
  where
    pVoiceToTVoice :: PVoice a -> TVoice a
    pVoiceToTVoice x = mkTrack $ rights $ map (sequenceA) $ firsts (offsetPoints (0 :: Time)) (withDurationR x)
    -- TODO assert no overlapping
    tVoiceToPVoice :: TVoice a -> PVoice b -> PVoice a
    tVoiceToPVoice tv pv = set _rights newPhrases pv
      where
        newPhrases = toListOf traverse tv

_rights :: Lens [Either a b] [Either a c] [b] [c]
_rights = lens _rightsGet (flip _rightsSet)
  where
    _rightsGet :: [Either a b] -> [b]
    _rightsGet = rights
    _rightsSet :: [c] -> [Either a b] -> [Either a c]
    _rightsSet cs = sndMapAccumL f cs
      where
        f cs (Left a) = (cs, Left a)
        f (c : cs) (Right b) = (cs, Right c)
        f [] (Right _) = error "No more cs"
    sndMapAccumL f z = snd . List.mapAccumL f z

firsts :: ([a] -> [b]) -> [(a, c)] -> [(b, c)]
firsts f = uncurry zip . first f . unzipR

mkTrack :: [(Time, a)] -> Track a
mkTrack = view track . map (view placed)

withDurationR :: (Functor f, HasDuration a) => f a -> f (Duration, a)
withDurationR = fmap $ \x -> (_duration x, x)

-- |
-- Group contigous sequences matching/not-matching the predicate.
--
-- >>> groupDiff (== 0) [0,1,2,3,5,0,0,6,7]
-- [Right [0], Left [1,2,3,5], Right [0,0], Left [6,7]]
groupDiff :: (a -> Bool) -> [a] -> [Either [a] [a]]
groupDiff p [] = []
groupDiff p (x : xs)
  | not (p x) = Left (x : List.takeWhile (not . p) xs) : groupDiff p (List.dropWhile (not . p) xs)
  | p x = Right (x : List.takeWhile p xs) : groupDiff p (List.dropWhile p xs)

unzipR :: Functor f => f (a, b) -> (f a, f b)
unzipR x = (fmap fst x, fmap snd x)
