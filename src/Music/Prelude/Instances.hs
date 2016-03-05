
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

------------------------------------------------------------------------------------
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
--
-------------------------------------------------------------------------------------

module Music.Prelude.Instances () where

import           Data.AffineSpace.Point
import           Data.Typeable
import           Control.Comonad (extract)

import           Music.Dynamics
import           Music.Parts
import           Music.Pitch
import           Music.Score            hiding (Fifths, Interval, Note, Pitch)

-- import qualified Data.Music.Lilypond         as Lilypond
-- import qualified Data.Music.MusicXml.Simple  as Xml
import qualified Music.Score            as Score
import Data.Functor.Adjunction (unzipR)




instance HasBackendNote Midi Semitones where
  exportNote b = exportNote b . fmap toInteger
  exportChord b = exportChord b . fmap (fmap toInteger)

instance HasBackendNote Midi Pitch where
  exportNote b = exportNote b . fmap (\p -> semitones (p .-. c))
  exportChord b = exportChord b . fmap (fmap (\p -> semitones (p .-. c)))

instance HasBackendNote SuperCollider Semitones where
  exportNote b = exportNote b . fmap toInteger
  exportChord b = exportChord b . fmap (fmap toInteger)

instance HasBackendNote SuperCollider Pitch where
  exportNote b = exportNote b . fmap (\p -> semitones (p .-. c))
  exportChord b = exportChord b . fmap (fmap (\p -> semitones (p .-. c)))


-- instance HasBackendNote MusicXml Pitch where
--   exportNote  _ (XmlContext d Nothing)    = Xml.rest (realToFrac d)
--   exportNote  _ (XmlContext d (Just x))   = (`Xml.note` realToFrac d) . snd3 Just . spellPitch 4 $ x
--
--   exportChord _ (XmlContext d Nothing)    = Xml.rest (realToFrac d)
--   exportChord _ (XmlContext d (Just xs))  = (`Xml.chord` (realToFrac d)) . fmap (snd3 Just . spellPitch 4) $ xs
--
-- instance HasBackendNote Lilypond Pitch where
--   exportNote  _ (LyContext d Nothing)    = (^*realToFrac (4*d)) Lilypond.rest
--   exportNote  _ (LyContext d (Just x))   = (^*realToFrac (d*4)) . Lilypond.note . pitchLilypond . Lilypond.Pitch . spellPitch 5 $ x
--
--   exportChord _ (LyContext d Nothing)    = (^*realToFrac (4*d)) Lilypond.rest
--   exportChord _ (LyContext d (Just xs))  = (^*realToFrac (d*4)) . Lilypond.chord . fmap (pitchLilypond . Lilypond.Pitch . spellPitch 5) $ xs

-- TODO move
snd3 f (a, b, c) = (a, f b, c)
-- pitchLilypond a = Lilypond.NotePitch a Nothing

spellPitch :: (Enum p, Num a, Num o) => Octaves -> Pitch -> (p, a, o)
spellPitch referenceOctave p = (pitchName, pitchAccidental, octave)
    where
        pitchName       = toEnum $ fromEnum $ name p
        pitchAccidental = fromIntegral $ accidental p
        octave          = fromIntegral $ (+ referenceOctave) $ octaves (p .-. c)

instance HasMidiProgram BasicPart where
    getMidiChannel _ = 0
    getMidiProgram _ = 0

instance HasMidiProgram Music.Parts.Part where
    getMidiChannel = defaultMidiChannel
    getMidiProgram = fixStrings . defaultMidiProgram
        where
            fixStrings x = case x of
                40 -> 48
                41 -> 48
                42 -> 48
                x  -> x

-- instance HasLilypondInstrument BasicPart where
--     getLilypondClef = 0
--
-- instance HasLilypondInstrument Music.Parts.Part where
--     getLilypondClef = defaultClef
--
-- instance HasMusicXmlInstrument BasicPart where
--     getMusicXmlClef = 0
--     getMusicXmlNumberOfStaves = 1
--
-- instance HasMusicXmlInstrument Music.Parts.Part where
--     getMusicXmlClef = defaultClef
--     getMusicXmlNumberOfStaves p
--       | p == harp                 = 2
--       | p^._instrument == piano   = 2
--       | p^._instrument == celesta = 2
--       | otherwise                 = 1


instance HasDuration Pitch where
  _duration = const 1
instance HasDuration a => HasDuration (PartT p a) where
  _duration = _duration . extract
instance HasDuration a => HasDuration (ColorT a) where
  _duration = _duration . extract
instance HasDuration a => HasDuration (TextT a) where
  _duration = _duration . extract
instance HasDuration a => HasDuration (TremoloT a) where
  _duration = _duration . extract
instance HasDuration a => HasDuration (HarmonicT a) where
  _duration = _duration . extract
instance HasDuration a => HasDuration (SlideT a) where
  _duration = _duration . extract
instance HasDuration a => HasDuration (ArticulationT b a) where
  _duration = _duration . extract
instance HasDuration a => HasDuration (DynamicT b a) where
  _duration = _duration . extract
instance HasDuration a => HasDuration (TieT a) where
  _duration = _duration . extract


instance Splittable Pitch where
  split _ x = (x,x)

instance Splittable a => Splittable (PartT p a) where
  split t = unzipR . fmap (split t)
instance Splittable a => Splittable (ColorT a) where
  split t = unzipR . fmap (split t)
instance Splittable a => Splittable (TextT a) where
  split t = unzipR . fmap (split t)
instance Splittable a => Splittable (TremoloT a) where
  split t = unzipR . fmap (split t)
instance Splittable a => Splittable (HarmonicT a) where
  split t = unzipR . fmap (split t)
instance Splittable a => Splittable (SlideT a) where
  split t = unzipR . fmap (split t)
instance Splittable a => Splittable (ArticulationT b a) where
  split t = unzipR . fmap (split t)
instance Splittable a => Splittable (DynamicT b a) where
  split t = unzipR . fmap (split t)
instance Splittable a => Splittable (TieT a) where
  split t = unzipR . fmap (split t) 


instance Reversible Pitch where
  rev = id
instance Reversible (Score a ) where
  rev = revDefault



