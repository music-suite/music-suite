{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides miscellaneous instances.
module Music.Prelude.Inspectable
  ( Inspectable (..),
  )
where

import Music.Prelude.Standard hiding (open, play)
import Music.Score ()
import qualified Music.Score
import qualified System.Info
import qualified System.Process
import qualified System.Process


-- Not perfect but works for many cases
--
-- >>> ucat [[violins],[violas]]
-- [Violin,Viola]
-- >>> ucat [[violins],[violins]]
-- [Violin I,Violin II]
-- >>> ucat [[violins1],[violins2]]
-- [Violin I,Violin II]
-- >>> ucat [[violins],[violins2]]
-- [Violin I,Violin II]
--
ucat :: (Monoid a, Semigroup a, HasParts a a, Music.Score.Part a ~ Part) => [a] -> a
ucat xs =
  if allDistinct ps
    then pcat xs
    else pcat $ zipWith (parts' .~) (divide (length xs) p) xs
  where
    ps = concatMap (toListOf parts') xs
    p = foldr1 largestPart ps

class Inspectable a where
  inspectableToMusic :: a -> Music

instance Inspectable (Score StandardNote) where
  inspectableToMusic = id

instance Inspectable a => Inspectable (Maybe a) where
  inspectableToMusic = maybe mempty id . fmap inspectableToMusic

instance Inspectable (Score Pitch) where
  inspectableToMusic = inspectableToMusic . asScore . fmap fromPitch

instance Inspectable (Voice StandardNote) where
  inspectableToMusic = inspectableToMusic . renderAlignedVoice . aligned 0 0

instance Inspectable (Voice Pitch) where
  inspectableToMusic = inspectableToMusic . asScore . renderAlignedVoice . aligned 0 0 . fmap fromPitch

-- instance Inspectable (Voice ()) where
-- inspectableToMusic = inspectableToMusic . set pitches (c::Pitch)
instance Inspectable (Ambitus Pitch) where
  inspectableToMusic x = let (m, n) = x ^. from ambitus in glissando $ fromPitch m |> fromPitch n

instance Inspectable [Chord Pitch] where
  inspectableToMusic = scat . fmap inspectableToMusic

instance Inspectable (Mode Pitch) where
  inspectableToMusic = inspectableToMusic . modeToScale c

instance Inspectable (Scale Pitch) where
  inspectableToMusic = fmap fromPitch . scat . map (\x -> pure x :: Score Pitch) . scaleToList

instance Inspectable (Function Pitch) where
  inspectableToMusic = inspectableToMusic . functionToChord c

instance Inspectable (Chord Pitch) where
  inspectableToMusic = fmap fromPitch . pcat . map (\x -> pure x :: Score Pitch) . chordToList

-- instance Inspectable [Hertz] where
--   inspectableToMusic xs = pcat $ map fromPitch $ map (^.from pitchHertz) xs
-- instance Inspectable [[Hertz]] where
--   inspectableToMusic = scat . fmap inspectableToMusic
instance Inspectable [Pitch] where
  inspectableToMusic = compress 8 . scat . fmap fromPitch

-- instance Inspectable Hertz where
-- inspectableToMusic = inspectableToMusic . (:[])
instance Inspectable Pitch where
  inspectableToMusic = inspectableToMusic . (: [])

instance Inspectable Interval where
  inspectableToMusic v = stretch 8 $ inspectableToMusic [c :: Pitch, c .+^ v]

instance Inspectable Span where
  inspectableToMusic s = transform s c

instance Inspectable Time where
  inspectableToMusic t = inspectableToMusic (t >-> (1 / 4))

-- TODO use power of 2 related to time...
instance Inspectable Duration where
  inspectableToMusic d = inspectableToMusic (0 >-> d)

instance Inspectable [Span] where
  inspectableToMusic xs = ucat $ fmap inspectableToMusic xs

instance Inspectable [Voice Pitch] where
  inspectableToMusic = asScore . ucat . fmap (fmap fromPitch) . fmap (renderAlignedVoice . aligned 0 0)

instance Inspectable [Note Pitch] where
  inspectableToMusic = inspectableToMusic . fmap ((^. voice) . pure)
