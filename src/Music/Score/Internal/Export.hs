{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}
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
--
-- TODO generalize the below. We can get a Reactive for each meta property
-- (time sig, key sig etc).
--
-- Use Applicative instance to merge reactives pointwise then reactiveToVoice'
-- for the era of the score.



-- | Extract bar-related information from score meta-data.
--
-- Returns a list of bars with durations and possibly time signature, key signature
-- etc. For no change, Nothing is returned.
extractBars :: (HasMeta a, HasPosition a, Transformable a) => a ->
  [(Duration, Maybe TimeSignature, Maybe KeySignature {-, Maybe Barline, Key, Maybe RehearsalMark, Maybe Tempo-})]
extractBars x = zip3 ds ts ks
  where
    (ds, ts) = unzip $ extractTimeSignatures x
    ks       = cycle [mempty] -- TODO



-- | Extract bar-related information from score meta-data.
--
-- Returns a list of bars with durations and possibly time signature, key signature
-- etc. For no change, Nothing is returned.
extractTimeSignatures :: (HasMeta a, HasPosition a, Transformable a) => a
  -> [(Duration, Maybe TimeSignature)]
extractTimeSignatures score = zip (fmap realToFrac barTimeSignatures) (retainUpdates barTimeSignatures)
  where
    -- From position 0, the duration of each time signature and how long it lasts
    timeSignatures :: [(TimeSignature, Duration)]
    timeSignatures =
      fmap swap
        $ view pairs . {-fuse . -} reactiveToVoice' (0 <-> (score ^. offset))
        $ getTimeSignatures defaultTimeSignature score
    -- The time signature of each bar.
    barTimeSignatures =
      prolongLastBarIfDifferent $
        fmap fst $ tsPerBar $ fmap (, ()) timeSignatures

-- | Extract the time signature meta-track, using the given default.
getTimeSignatures :: HasMeta a => TimeSignature -> a -> Reactive TimeSignature
getTimeSignatures def = fmap (fromMaybe def . fmap getLast . getOption) . fromMetaReactive . (view meta)

-- | Extract the key signature meta-track
getKeySignatures :: HasMeta a => a -> Reactive KeySignature
getKeySignatures = fromMetaReactive . view meta

defaultTimeSignature :: TimeSignature
defaultTimeSignature = time 4 4

-- |
-- If the last value is shorter than the preceding value, change it to
-- the preceding value.
--
-- Useful to scores this from having an unexpected time signature change
-- in the last bar.
prolongLastBarIfDifferent :: [TimeSignature] -> [TimeSignature]
prolongLastBarIfDifferent = reverse . go . reverse
  where
    go [] = []
    go [x] = [x]
    go (last : penult : xs)
      | last < penult = penult : penult : xs
      | otherwise = last : penult : xs



-- | Given a list of time signatures and the duration between them return a
-- list of appropriate time signatures for each bar.
tsPerBar :: [((TimeSignature, Duration), a)] -> [(TimeSignature, a)]
tsPerBar = concatMap $ uncurry $ uncurry go
-- (TODO use voice instead of list?),
  where
    go :: TimeSignature -> Duration -> a -> [(TimeSignature, a)]
    go ts d a  =
      let (n, r) = numWholeBars ts d
       in -- Repeat the chosen time signature as long as possible
          -- If there is a rest duration, add a bar of that duration choosing an appropriate time signature
          replic n (ts, a) ++ if r > 0 then
            case standardTimeSignature r of
              Just x -> [(x, a)]
              -- Remainder duration is not a power of two
              --
              -- TODO this should fail iff this is not the last "note" in the
              -- voice given to tsPerBar. For the last note it's OK to chose
              -- an arbitrary TS (FIXME as long as it's a power of 2 larger than r!)
              Nothing -> [(time 4 4, a)]
            else []

-- | Return the number of whole bars needed to notate the given duration, as well as the remainder duration.
numWholeBars :: TimeSignature -> Duration -> (Integer, Duration)
numWholeBars ts dur = second (* barDur) $ properFraction (dur / barDur) where barDur = realToFrac ts

