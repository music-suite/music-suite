{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wwarn #-}

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
import Music.Pitch.Literal
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Internal.Quantize
import Music.Score.Internal.Util
import Music.Score.Meta.Time
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

extractTimeSignatures :: Score a -> ([Maybe TimeSignature], [Duration])
extractTimeSignatures score = (retainUpdates barTimeSignatures, barDurations)
  where
    defaultTimeSignature = time 4 4

    -- | The time signature changes with their actual durations (which may
    -- span many bars).
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
      prolongLastBarIfDifferent
      $ getBarTimeSignatures timeSignatures

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
          | otherwise     = last   : penult : xs


--
-- -- | Convert a voice to a list of bars using the given bar durations.
-- voiceToBars' :: Tiable a => [Duration] -> Voice (Maybe a) -> [[(Duration, Maybe a)]]
-- voiceToBars' barDurs = fmap (map (^. from note) . (^. notes)) . splitTiesAt barDurs
-- -- TODO remove prime from name
--
-- -- | Basic spelling for integral types.
-- spellPitch :: Integral a => a -> (a, a, a)
-- spellPitch p = (
--     pitchClass,
--     alteration,
--     octave
--     )
--     where
--         octave     = (p `div` 12) - 1
--         semitone   = p `mod` 12
--         pitchClass = fromStep major semitone
--         alteration = semitone - step major pitchClass
--
--         step xs p = xs !! (fromIntegral p `mod` length xs)
--         fromStep xs p = fromIntegral $ fromMaybe (length xs - 1) $ List.findIndex (>= p) xs
--         scaleFromSteps = snd . List.mapAccumL add 0
--             where
--                 add a x = (a + x, a + x)
--         major = scaleFromSteps [0,2,2,1,2,2,2,1]
--
--
-- type MVoice a = Voice (Maybe a)
--
-- -- toMVoice :: (Semigroup a, Transformable a) => Score a -> MVoice a
-- -- toMVoice = scoreToVoice . simultaneous
--
-- unvoice :: Voice b -> [(Duration, b)]
-- unvoice = toListOf (notes . traverse . from note)
-- -- unvoice = fmap (^. from note) . (^. notes)
-- {-# DEPRECATED unvoice "Use 'unsafeEventsV'" #-}
--
--
-- openCommand :: String
-- openCommand = case Info.os of
--   "darwin" -> "open"
--   "linux"  -> "xdg-open"
--
-- {-
-- -- TODO any version and/or OS
-- hasMuseScore = do
--   result <- try (readProcess "ls" ["/Applications/MuseScore.app"] "")
--   return $ case result of
--     Left e   -> (e::SomeException) `assumed` False
--     Right _ ->  True
--
-- hasSibelius = do
--   result <- try (readProcess "ls" ["/Applications/Sibelius 7.app"] "")
--   return $ case result of
--     Left e   -> (e::SomeException) `assumed` False
--     Right _ ->  True
--
--
-- assumed = flip const
-- -}
--
-- -- JUNK
