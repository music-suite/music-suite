{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -Wwarn #-}
{-# OPTIONS_HADDOCK hide #-}

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
module Music.Score.Internal.Export
  ( extractTimeSignatures,
  )
where

-- voiceToBars',
-- -- separateBars,
-- spellPitch,
-- MVoice,
-- -- toMVoice,
-- unvoice,
-- openCommand

import qualified Codec.Midi as Midi
import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad hiding (mapM)
import Control.Monad.Plus
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Basis
import Data.Bifunctor (second)
import Data.Either
import Data.Foldable
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord (comparing)
import Data.Ratio
import Data.Semigroup
import Data.String
import Data.Traversable
import Data.Typeable
import Data.VectorSpace
import Music.Dynamics.Literal
import Music.Time.Meta (Meta, HasMeta(..))
import Music.Score.Meta (fromMetaReactive)
import Music.Pitch.Literal
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Internal.Quantize
import Music.Score.Internal.Util
import Music.Score.Meta.Time
import Music.Score.Meta.Key
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Ties
import Music.Time
import Music.Time.Internal.Convert
import System.IO.Unsafe
import qualified System.Info as Info
import System.Process
import qualified Text.Pretty as Pretty
import Prelude hiding
  ( concat,
    concatMap,
    foldl,
    foldr,
    mapM,
    maximum,
    minimum,
    sum,
  )

-- TODO also extract Barline, Key, RehearsalMark, Tempo here
extractBars :: (HasMeta a, HasPosition a, Transformable a) => a ->
  [(Duration, Maybe TimeSignature, KeySignature {-, Maybe Barline, Key, Maybe RehearsalMark, Maybe Tempo-})]
extractBars x = zip3 ds ts ks
  where
    (ds, ts) = unzip $ extractTimeSignatures x
    ks       = cycle [mempty] -- TODO

getKeySignatures :: HasMeta a => a -> Reactive KeySignature
getKeySignatures = fromMetaReactive . view meta

-- TODO generalize the below. We can get a Reactive for each meta property
-- (time sig, key sig etc).
--
-- Use Applicative instance to merge reactives pointwise then reactiveToVoice'
-- for the era of the score.

-- | Extract bar-related information from score meta-data.
--
-- Returns a list of bars with durations and possibly time signature, key signature
-- etc. For no change, Nothing or mempty is returned.
extractTimeSignatures :: (HasMeta a, HasPosition a, Transformable a) => a
  -> [(Duration, Maybe TimeSignature)]
extractTimeSignatures score = zip barDurations (retainUpdates barTimeSignatures)
  where
    defaultTimeSignature = time 4 4
    -- The duration of each time signature and how long it lasts
    --
    timeSignatures :: [(TimeSignature, Duration)]
    timeSignatures =
      fmap swap
        $ view pairs . fuse . reactiveToVoice' (0 <-> (score ^. offset))
        $ getTimeSignatures defaultTimeSignature score
    -- The time signature of each bar.
    --
    -- TODO alternative combinator that returns barTimeSignatures as this has all
    -- the information
    barTimeSignatures =
      prolongLastBarIfDifferent $
        getBarTimeSignatures timeSignatures
    barDurations = fmap realToFrac barTimeSignatures
    -- Allow the last bar to be shorter than the indicated time signature
    -- To prevent e.g. this from having an unexpected time signature change
    --
    -- @
    -- timeSignatures (3/4) c
    -- @
    prolongLastBarIfDifferent :: [TimeSignature] -> [TimeSignature]
    prolongLastBarIfDifferent = reverse . go . reverse
      where
        go [] = []
        go [x] = [x]
        go (last : penult : xs)
          | last < penult = penult : penult : xs
          | otherwise = last : penult : xs

getTimeSignatures :: HasMeta a => TimeSignature -> a -> Reactive TimeSignature
getTimeSignatures def = fmap (fromMaybe def . fmap getLast . getOption) . fromMetaReactive . (view meta)


-- | Given a list of time signatures and the duration between them (TODO use voice), return a list of appropriate
--   time signatures for each bar.
getBarTimeSignatures :: [(TimeSignature, Duration)] -> [TimeSignature]
getBarTimeSignatures = concatMap $ uncurry getBarTimeSignatures1

getBarTimeSignatures1 :: TimeSignature -> Duration -> [TimeSignature]
getBarTimeSignatures1 ts d =
  let (n, r) = numWholeBars ts d
   in -- Repeat the chosen time signature as long as possible
      -- If there is a rest duration, add a bar of that duration choosing an appropriate time signature
      replic n ts ++ if r > 0 then [standardTimeSignature r] else []

-- | Return the number of whole bars needed to notate the given duration, as well as the remainder duration.
numWholeBars :: TimeSignature -> Duration -> (Integer, Duration)
numWholeBars ts dur = second (* barDur) $ properFraction (dur / barDur) where barDur = realToFrac ts

