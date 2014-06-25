
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
-- /Warning/ This module is experimental.
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

    -- ** Utility
    mvoicePVoice,
    unsafeMvoicePVoice,
    singleMVoice,
  ) where

import           Control.Applicative
import           Control.Exception   (assert)
import           Control.Lens
import           Control.Monad.Plus
import           Data.AffineSpace
import qualified Data.List           as List
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup

import           Music.Score.Part
import           Music.Score.Convert
import           Music.Time


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
-- For an explicit representation of the phrase structure, see 'mvoicePVoice'.
--
type MVoice a = Voice (Maybe a)

-- |
-- A sequence of phrases or rests, represented with explicit phrase structure.
--
type PVoice a = [Either Duration (Phrase a)]


{-
class HasPhrases a b | a -> b where
  mvoices :: Traversal' a (MVoice b)

instance HasPhrases (MVoice a) a where
  mvoices = id

instance HasPhrases (PVoice a) a where
  -- Note: This is actually OK in 'phrases', as that just becomes (id . each . _Right)
  mvoices = from unsafeMvoicePVoice

instance (HasPart' a, Transformable a, Ord (Part a)) => HasPhrases (Score a) a where
  mvoices = extracted . each . singleMVoice
-}

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
  mvoices = from unsafeMvoicePVoice

-- | Traverses all phrases in each voice, using 'extracted'.
instance (HasPart' a, {-HasPart a b, -}{-Transformable a,-} Ord (Part a)) =>
  HasPhrases (Score a) (Score b) a b where
  mvoices = extracted . each . singleMVoice

type HasPhrases' s a = HasPhrases s s a a

{-
Phrase traversal for score:

phrasesS :: (Ord (Part a), HasPart' a, Transformable a) => Traversal' (Score a) (Phrase a)
phrasesS = extracted . each . singleMVoice . mvoicePVoice . each . _Right

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
phrases = mvoices . mvoicePVoice . each . _Right

-- |
-- View an 'MVoice' as a 'PVoice'.
--
mvoicePVoice :: Lens (MVoice a) (MVoice b) (PVoice a) (PVoice b)
mvoicePVoice = unsafeMvoicePVoice
-- TODO meta

-- |
-- View an 'MVoice' as a 'PVoice' and vice versa.
--
-- This a valid 'Iso' up to meta-data equivalence.
--
unsafeMvoicePVoice :: Iso (MVoice a) (MVoice b) (PVoice a) (PVoice b)
unsafeMvoicePVoice = iso mvoiceToPVoice pVoiceToMVoice
  where
    mvoiceToPVoice :: MVoice a -> PVoice a
    mvoiceToPVoice =
      map ( bimap voiceToRest voiceToPhrase
          . bimap (^.from unsafeEventsV) (^.from unsafeEventsV) )
       . groupDiff' (isJust . snd)
       . view eventsV

    voiceToRest :: MVoice a -> Duration
    voiceToRest = sumOf (eventsV.each._1) . fmap (\x -> assert (isNothing x) x)
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
-- TODO why Transformable?
singleMVoice :: {-Transformable a =>-} Prism (Score a) (Score b) (MVoice a) (MVoice b)
singleMVoice = iso scoreToVoice voiceToScore'
  where
    scoreToVoice :: {-Transformable a =>-} Score a -> MVoice a
    scoreToVoice = (^. voice) . fmap (^. stretched) . fmap throwTime . addRests .
      -- TODO
      List.sortBy (comparing (^._1))
      -- end TODO
      . (^. events)
      where
        throwTime (t,d,x) = (d,x)
        addRests = concat . snd . List.mapAccumL g 0
          where
            g u (t, d, x)
              | u == t    = (t .+^ d, [(t, d, Just x)])
              | u <  t    = (t .+^ d, [(u, t .-. u, Nothing), (t, d, Just x)])
              | otherwise = error "singleMVoice: Strange prevTime"

    voiceToScore :: Voice a -> Score a
    voiceToScore = scat . fmap g . (^. stretcheds) where g = (^. stretchedValue) . fmap return

    voiceToScore' :: MVoice a -> Score a
    voiceToScore' = mcatMaybes . voiceToScore


instance (Transformable a, Transformable b) => Cons (Phrase a) (Phrase b) a b where
  _Cons = undefined
-- instance (Transformable a, Transformable b) => Snoc (Phrase a) (Phrase b) a b where
  -- _Snoc = prism' pure (preview lastV)


-- TODO move

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
