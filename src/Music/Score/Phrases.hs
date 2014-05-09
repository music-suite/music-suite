
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-------------------------------------------------------------------------------------

module Music.Score.Phrases (
    Phrase,

    MVoice,
    PVoice,
    mvoicePVoice,
    unsafeMvoicePVoice,
    singleMVoice,

    HasPhrases(..),
    HasPhrases',
    phrases,
    phrases',

    -- * Utility (TODO move)
    headV,
    middleV,
    lastV,
    eventsV,
    unsafeEventsV,
  ) where

import Data.Maybe
import qualified Data.List as List
import Control.Lens
import Control.Applicative
import Data.AffineSpace
import Data.Semigroup
import Control.Monad.Plus
import Control.Exception (assert)

import Music.Score.Part
-- import Music.Score.Util (tripl, untripl, through)
import Music.Score.Convert
import Music.Time


type Phrase a = Voice a
type MVoice a = Voice (Maybe a)
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

class HasPhrases s t a b | s -> a, t -> b, s b -> t, t a -> s where
  mvoices :: Traversal s t (MVoice a) (MVoice b)

instance HasPhrases (MVoice a) (MVoice b) a b where
  mvoices = id

instance HasPhrases (PVoice a) (PVoice b) a b where
  -- Note: This is actually OK in 'phr', as that just becomes (id . each . _Right)
  mvoices = from unsafeMvoicePVoice

instance (HasPart' a, {-HasPart a b, -}Transformable a, Ord (Part a)) => 
  HasPhrases (Score a) (Score b) a b where
  mvoices = extracted . each . singleMVoice

type HasPhrases' s a = HasPhrases s s a a

{-
Phrase traversal for score:

phrasesS :: (Ord (Part a), HasPart' a, Transformable a) => Traversal' (Score a) (Phrase a)
phrasesS = extracted . each . singleMVoice . mvoicePVoice . each . _Right

More generally:
-}
phrases' :: HasPhrases' s a => Traversal' s (Phrase a)
phrases' = phrases

phrases :: HasPhrases s t a b => Traversal s t (Phrase a) (Phrase b)
phrases = mvoices . mvoicePVoice . each . _Right


mvoicePVoice :: Lens (MVoice a) (MVoice b) (PVoice a) (PVoice b)
mvoicePVoice = unsafeMvoicePVoice

-- TODO only up to meta-data (as it uses unsafeEventsV)...
-- This is not a problem if we restrict it to a traversal
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
singleMVoice :: Transformable a => Prism (Score a) (Score b) (MVoice a) (MVoice b)
singleMVoice = iso scoreToVoice voiceToScore'
  where
    scoreToVoice :: Transformable a => Score a -> MVoice a
    scoreToVoice = (^. voice) . fmap (^. stretched) . fmap throwTime . addRests . (^. events)
      where
        throwTime (t,d,x) = (d,x)
        addRests = concat . snd . List.mapAccumL g 0
          where
            g u (t, d, x)
              | u == t    = (t .+^ d, [(t, d, Just x)])
              | u <  t    = (t .+^ d, [(u, t .-. u, Nothing), (t, d, Just x)])
              | otherwise = error "singleMVoice: Strange prevTime"

    voiceToScore :: Voice a -> Score a
    voiceToScore = scat . fmap g . (^. stretcheds) where g = (^. getStretched) . fmap return

    voiceToScore' :: MVoice a -> Score a
    voiceToScore' = mcatMaybes . voiceToScore


instance (Transformable a, Transformable b) => Cons (Phrase a) (Phrase b) a b where
  _Cons = undefined
-- instance (Transformable a, Transformable b) => Snoc (Phrase a) (Phrase b) a b where
  -- _Snoc = prism' pure (preview lastV)

-- TODO make Voice/Phrase an instance of Cons/Snoc and remove these
headV :: Transformable a => Traversal' (Phrase a) a
headV = (eventsV._head._2)

middleV :: Transformable a => Traversal' (Phrase a) a
middleV = (eventsV._middle.traverse._2)

lastV :: Transformable a => Traversal' (Phrase a) a
lastV = (eventsV._last._2)

_middle :: (Snoc s s a a, Cons s s b b) => Traversal' s s
-- Traverse writing to all elements *except* first and last
_middle = _tail._init





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







