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
import qualified Music.Time
import qualified System.Info
import qualified System.Process
import qualified System.Process


class Inspectable a where
  inspectableToMusic :: a -> Music

instance Inspectable (Score StandardNote) where
  inspectableToMusic = id

-- TODO not just Pitch
instance Inspectable (Pattern Pitch) where
  inspectableToMusic = fmap fromPitch'' . flip renderPattern (0<->1)

instance (IsPitch a, Reversible a) => IsPitch (Pattern a) where
  fromPitch = pureP . fromPitch


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


instance Inspectable (Mode Pitch) where
  inspectableToMusic = inspectableToMusic . modeToScale c

instance Inspectable (Scale Pitch) where
  inspectableToMusic = fmap fromPitch . pseq . map (\x -> pure x :: Score Pitch) . scaleToList

instance Inspectable (Function Pitch) where
  inspectableToMusic = inspectableToMusic . functionToChord c

instance Inspectable (Chord Pitch) where
  inspectableToMusic = fmap fromPitch . ppar . map (\x -> pure x :: Score Pitch) . chordToList

-- TODO should be on separate staves, but without left binding (implying simultanuety)
instance Inspectable [Mode Pitch] where
  inspectableToMusic = rcat . fmap inspectableToMusic

-- TODO should be on separate staves, but without left binding (implying simultanuety)
instance Inspectable [Scale Pitch] where
  inspectableToMusic = rcat . fmap inspectableToMusic

instance Inspectable [Function Pitch] where
  inspectableToMusic = pseq . fmap inspectableToMusic

instance Inspectable [Chord Pitch] where
  inspectableToMusic = pseq . fmap inspectableToMusic

-- instance Inspectable [Hertz] where
--   inspectableToMusic xs = ppar $ map fromPitch $ map (^.from pitchHertz) xs
-- instance Inspectable [[Hertz]] where
--   inspectableToMusic = pseq . fmap inspectableToMusic
instance Inspectable [Pitch] where
  inspectableToMusic = compress 8 . pseq . fmap fromPitch

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
  inspectableToMusic xs = rcat $ fmap inspectableToMusic xs

instance Inspectable [Voice Pitch] where
  inspectableToMusic = asScore . rcat . fmap (fmap fromPitch) . fmap (renderAlignedVoice . aligned 0 0)

instance Inspectable [Note Pitch] where
  inspectableToMusic = inspectableToMusic . fmap ((^. voice) . pure)
