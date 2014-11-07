
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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
-- This module provides phrase-wise traversal.
--
-------------------------------------------------------------------------------------

module Music.Score.Phrases (
    -- * HasPhrases class
    HasPhrases(..),
    HasPhrases',
    phrases,
    phrases',

    -- * Phrase types etc
    Phrase,
    MVoice,
    PVoice,
    TVoice,

    -- ** Utility
    mVoicePVoice,
    mVoiceTVoice,
    pVoiceTVoice,
    unsafeMVoicePVoice,
    singleMVoice,
    mapPhrasesWithPrevAndCurrentOnset,
  ) where

import           Control.Applicative
import           Control.Applicative
import           Control.Comonad            (Comonad (..), extract)
import           Control.Exception          (assert)
import           Control.Lens
import           Control.Lens               hiding (rewrite)
import           Control.Monad
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace
import           Data.Bifunctor
import           Data.Colour.Names          as Color
import           Data.Default
import           Data.Either
import           Data.Either
import           Data.Foldable              (Foldable)
import           Data.Functor.Adjunction    (unzipR)
import           Data.Functor.Context
import           Data.Functor.Contravariant
import           Data.Functor.Couple
import qualified Data.List                  as List
import qualified Data.List
import           Data.Maybe
import           Data.Maybe
import           Data.Ord
import           Data.Ratio
import           Data.Semigroup
import           Data.Semigroup
import           Data.Traversable
import           Data.Traversable           (Traversable, sequenceA)
import           Data.VectorSpace           hiding (Sum (..))
import           System.Process

import           Music.Score.Part
import           Music.Time
import           Music.Time.Internal.Convert
import           Music.Time.Internal.Util


-- |
-- For a phrase, we simply use a voice without rests.
--
-- To represent a sequence of phrases we provide two equivalent representations:
--
-- * 'MVoice' is a sequence of notes/chords or rests. All consecutive non-rests consitute a phrase.
--
-- * 'PVoice' is a sequence of phrases or durations.
--
type Phrase a = Voice a

-- |
-- A sequence of phrases or rests, represented as notes or rests.
--
-- Each consecutive sequence of non-rest elements is considered to be a phrase.
-- For a more explicit representation of the phrase structure, see 'PVoice'.
--
type MVoice a = Voice (Maybe a)

-- |
-- A sequence of phrases or rests, represented with explicit phrase structure.
--
type PVoice a = [Either Duration (Phrase a)]

-- |
-- A sequence of phrases or rests, represented as phrases with an explicit onset.
--
-- This is only isomorphic to 'MVoice' (and 'PVoice') up to onset equivalence.
--
type TVoice a = Track (Phrase a)


-- |
-- Classes that provide a phrase traversal.
--
class HasPhrases s t a b | s -> a, t -> b, s b -> t, t a -> s where
  mvoices :: Traversal s t (MVoice a) (MVoice b)

-- | Traverses all phrases in a voice.
instance HasPhrases (MVoice a) (MVoice b) a b where
  mvoices = id

  -- | Traverses all phrases in a voice.
instance HasPhrases (PVoice a) (PVoice b) a b where
  -- Note: This is actually OK in 'phr', as that just becomes (id . each . _Right)
  mvoices = from unsafeMVoicePVoice

-- | Traverses all phrases in each voice, using 'extracted'.
instance (HasPart' a, {-HasPart a b, -}{-Transformable a,-} Ord (Part a)) =>
  HasPhrases (Score a) (Score b) a b where
  mvoices = extracted . each . singleMVoice

type HasPhrases' s a = HasPhrases s s a a

{-
Phrase traversal for score:

phrasesS :: (Ord (Part a), HasPart' a, Transformable a) => Traversal' (Score a) (Phrase a)
phrasesS = extracted . each . singleMVoice . mVoicePVoice . each . _Right

More generally:
-}

-- |
-- A simple generic phrase-traversal.
--
phrases' :: HasPhrases' s a => Traversal' s (Phrase a)
phrases' = phrases

-- |
-- A generic phrase-traversal.
--
phrases :: HasPhrases s t a b => Traversal s t (Phrase a) (Phrase b)
phrases = mvoices . mVoicePVoice . each . _Right

-- |
-- View an 'MVoice' as a 'PVoice'.
--
mVoicePVoice :: Lens (MVoice a) (MVoice b) (PVoice a) (PVoice b)
mVoicePVoice = unsafeMVoicePVoice
-- TODO meta

-- |
-- View an 'MVoice' as a 'PVoice' and vice versa.
--
-- This a valid 'Iso' up to meta-data equivalence.
--
unsafeMVoicePVoice :: Iso (MVoice a) (MVoice b) (PVoice a) (PVoice b)
unsafeMVoicePVoice = iso mvoiceToPVoice pVoiceToMVoice
  where
    mvoiceToPVoice :: MVoice a -> PVoice a
    mvoiceToPVoice =
      map ( bimap voiceToRest voiceToPhrase
          . bimap (^.from unsafePairs) (^.from unsafePairs) )
       . groupDiff' (isJust . snd)
       . view pairs

    voiceToRest :: MVoice a -> Duration
    voiceToRest = sumOf (pairs.each._1) . fmap (\x -> assert (isNothing x) x)
    -- TODO just _duration

    voiceToPhrase :: MVoice a -> Phrase a
    voiceToPhrase = fmap fromJust

    pVoiceToMVoice :: (PVoice a) -> MVoice a
    pVoiceToMVoice = mconcat . fmap (either restToVoice phraseToVoice)

    restToVoice :: Duration -> MVoice a
    restToVoice d = stretch d $ pure Nothing

    phraseToVoice :: Phrase a -> MVoice a
    phraseToVoice = fmap Just


-- TODO failure
singleMVoice :: Prism (Score a) (Score b) (MVoice a) (MVoice b)
singleMVoice = iso scoreToVoice voiceToScore'
  where
    scoreToVoice :: {-Transformable a =>-} Score a -> MVoice a
    scoreToVoice = (^. voice) . fmap (^. note) . fmap throwTime . addRests .
      -- TODO
      List.sortBy (comparing (^._1))
      -- end TODO
      . (^. triples)
      where
        throwTime (t,d,x) = (d,x)
        addRests = concat . snd . List.mapAccumL g 0
          where
            g u (t, d, x)
              | u == t    = (t .+^ d, [(t, d, Just x)])
              | u <  t    = (t .+^ d, [(u, t .-. u, Nothing), (t, d, Just x)])
              | otherwise = error "singleMVoice: Strange prevTime"

    voiceToScore :: Voice a -> Score a
    voiceToScore = scat . fmap g . (^. notes) where g = (^. notee) . fmap return

    voiceToScore' :: MVoice a -> Score a
    voiceToScore' = mcatMaybes . voiceToScore


-- foo :: HasPhrases' s a => s -> [TVoice a]
mapPhrasesWithPrevAndCurrentOnset :: HasPhrases s t a b => (Maybe Time -> Time -> Phrase a -> Phrase b) -> s -> t
mapPhrasesWithPrevAndCurrentOnset f = over (mvoices . mVoiceTVoice) (withPrevAndCurrentOnset f)

withPrevAndCurrentOnset :: (Maybe Time -> Time -> a -> b) -> Track a -> Track b
withPrevAndCurrentOnset f = over placeds (fmap (\(x,y,z) -> fmap (f (fmap placedOnset x) (placedOnset y)) y) . withPrevNext)
  where
    placedOnset :: Placed a -> Time
    placedOnset = view (from placed . _1)

mVoiceTVoice :: Lens (MVoice a) (MVoice b) (TVoice a) (TVoice b)
mVoiceTVoice = mVoicePVoice . pVoiceTVoice

pVoiceTVoice :: Lens (PVoice a) (PVoice b) (TVoice a) (TVoice b)
pVoiceTVoice = lens pVoiceToTVoice (flip tVoiceToPVoice)
  where
    pVoiceToTVoice :: PVoice a -> TVoice a
    pVoiceToTVoice x = mkTrack $ rights $ map (sequenceA) $ firsts (offsetPoints (0::Time)) (withDurationR x)

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
        f cs     (Left a)  = (cs, Left a)
        f (c:cs) (Right b) = (cs, Right c)
        f []     (Right _) = error "No more cs"

    sndMapAccumL f z = snd . List.mapAccumL f z

firsts :: ([a] -> [b]) -> [(a,c)] -> [(b,c)]
firsts f = uncurry zip . first f . unzipR

mkTrack :: [(Time, a)] -> Track a
mkTrack = view track . map (view placed)

withDurationR :: (Functor f, HasDuration a) => f a -> f (Duration, a)
withDurationR = fmap $ \x -> (_duration x, x)

-- TODO generalize and move
mapWithDuration :: HasDuration a => (Duration -> a -> b) -> a -> b
mapWithDuration = over dual withDurationL . uncurry
  where
    withDurationL :: (Contravariant f, HasDuration a) => f (Duration, a) -> f a
    withDurationL = contramap $ \x -> (_duration x, x)

    dual :: Iso (a -> b) (c -> d) (Op b a) (Op d c)
    dual = iso Op getOp

dursToVoice :: [Duration] -> Voice ()
dursToVoice = mconcat . map (\d -> stretch d $ return ())

{-
>>> print $ view (mVoiceTVoice) $ (fmap Just (dursToVoice [1,2,1]) <> return Nothing <> return (Just ()))


-}

-- |
-- Group contigous sequences matching/not-matching the predicate.
--
-- >>> groupDiff (== 0) [0,1,2,3,5,0,0,6,7]
-- [[0],[1,2,3,5],[0,0],[6,7]]
--
groupDiff :: (a -> Bool) -> [a] -> [[a]]
groupDiff p []     = []
groupDiff p (x:xs)
  | p x       = (x : List.takeWhile p         xs) : groupDiff p (List.dropWhile p         xs)
  | not (p x) = (x : List.takeWhile (not . p) xs) : groupDiff p (List.dropWhile (not . p) xs)

groupDiff' :: (a -> Bool) -> [a] -> [Either [a] [a]]
groupDiff' p []     = []
groupDiff' p (x:xs)
  | not (p x) = Left  (x : List.takeWhile (not . p) xs) : groupDiff' p (List.dropWhile (not . p) xs)
  | p x       = Right (x : List.takeWhile p         xs) : groupDiff' p (List.dropWhile p         xs)


-- JUNK
