{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

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
    InspectableNote (..),
  )
where

import Data.Foldable (toList)
import Music.Prelude.Standard
import Music.Score ()
import qualified Music.Time
import qualified System.Info
import qualified System.Process
import qualified System.Process

class Inspectable a where
  inspectableToMusic :: a -> Music

class InspectableNote a where
  -- inspectableToNote :: a -> StandardNote

  inspectableToMusicNote :: a -> Music

-- instance Inspectable (Score StandardNote) where
--   inspectableToMusic = id

-- TODO not just Pitch
instance InspectableNote a => Inspectable (Pattern a) where
  inspectableToMusic = inspectableToMusic . flip renderPattern (0 <-> 1)

instance Inspectable a => Inspectable (Maybe a) where
  inspectableToMusic = maybe mempty id . fmap inspectableToMusic

instance InspectableNote a => Inspectable (Score a) where
  inspectableToMusic = join . fmap inspectableToMusicNote

instance InspectableNote Pitch where
  inspectableToMusicNote = pure . fromPitch

instance InspectableNote Asp1a where
  inspectableToMusicNote = pure . pure

instance InspectableNote a => InspectableNote (Maybe a) where
  inspectableToMusicNote = maybe mempty id . fmap inspectableToMusicNote

instance InspectableNote () where
  inspectableToMusicNote () = fromPitch c

instance InspectableNote a => InspectableNote [a] where
  inspectableToMusicNote = mconcat . fmap inspectableToMusicNote

instance InspectableNote a => Inspectable (Voice a) where
  inspectableToMusic = join . fmap inspectableToMusicNote . renderAlignedVoice . aligned 0 0

instance InspectableNote a => Inspectable (Aligned (Voice a)) where
  inspectableToMusic = join . fmap inspectableToMusicNote . renderAlignedVoice

instance InspectableNote a => Inspectable [Aligned (Voice a)] where
  inspectableToMusic = rcat . fmap (join . fmap inspectableToMusicNote . renderAlignedVoice)

-- inspectableToMusic . rcat . fmap (preserveMeta $ asScore . fmap fromPitch . mcatMaybes . renderAlignedVoice)

instance InspectableNote a => Inspectable (Note a) where
  inspectableToMusic x = inspectableToMusic $ [x] ^. voice

instance InspectableNote a => Inspectable (Event a) where
  inspectableToMusic x = inspectableToMusic $ [x] ^. score

-- instance Inspectable (Voice ()) where
-- inspectableToMusic = inspectableToMusic . set pitches (c::Pitch)
instance Inspectable (Ambitus Interval Pitch) where
  inspectableToMusic x = stretch 0.5 $ let Ambitus m n = x in glissando $ fromPitch m |> fromPitch n

-- instance Inspectable (Mode Interval Pitch) where
--  inspectableToMusic = inspectableToMusic . scale c

instance Inspectable (ChordType Interval Pitch) where
  inspectableToMusic = inspectableToMusic . chord c

instance Inspectable (Scale Interval Pitch) where
  inspectableToMusic = pseq . fmap fromPitch . toList . getVoiced . voiced . scaleToChord

instance Inspectable (Chord Interval Pitch) where
  inspectableToMusic = inspectableToMusic . voiced

instance Inspectable (Voiced Chord Interval Pitch) where
  inspectableToMusic = fmap fromPitch . ppar . map (\x -> pure x :: Score Pitch) . toList . getVoiced

-- TODO should be on separate staves, but without left binding (implying simultanuety)
instance Inspectable [Mode Interval Pitch] where
   inspectableToMusic = inspectableToMusic . fmap (scale c)

-- TODO should be on separate staves, but without left binding (implying simultanuety)
instance Inspectable [Scale Interval Pitch] where
  inspectableToMusic = rcat . fmap inspectableToMusic

instance Inspectable [ChordType Interval Pitch] where
  inspectableToMusic = pseq . fmap inspectableToMusic

instance Inspectable [Chord Interval Pitch] where
  inspectableToMusic = pseq . fmap inspectableToMusic

instance Inspectable [Voiced Chord Interval Pitch] where
  inspectableToMusic = pseq . fmap inspectableToMusic

instance Inspectable [Interval] where
  inspectableToMusic = rcat . fmap inspectableToMusic

instance Inspectable [Ambitus Interval Pitch] where
  inspectableToMusic = rcat . fmap inspectableToMusic

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
  inspectableToMusic v = stretch 4 $ inspectableToMusic [c :: Pitch, c .+^ v]

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
  inspectableToMusic = rcat @Music . fmap (fmap fromPitch) . fmap (renderAlignedVoice . aligned 0 0)

instance Inspectable [Note Pitch] where
  inspectableToMusic = inspectableToMusic . fmap ((^. voice) . pure)
