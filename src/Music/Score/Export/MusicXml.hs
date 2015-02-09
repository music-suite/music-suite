
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

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

module Music.Score.Export.MusicXml (
    -- * MusicXml backend
    HasMusicXmlInstrument(..),
    MusicXml,
    XmlContext(..),
    HasMusicXml,

    -- ** Converting to MusicXml
    toMusicXml,
    toMusicXmlString,

    -- ** MusicXml I/O
    showMusicXml,
    openMusicXml,
    writeMusicXml,
  ) where

import qualified Codec.Midi                              as Midi
import           Control.Applicative
import           Control.Comonad                         (Comonad (..), extract)
import           Control.Lens                            hiding (rewrite)
import           Control.Monad
import           Data.AffineSpace
import           Data.Bifunctor
import           Data.Colour.Names                       as Color
import           Data.Foldable                           (Foldable)
import qualified Data.Foldable
import           Data.Functor.Couple
import           Data.Functor.Identity
import qualified Data.List
import           Data.Maybe
import           Data.Ratio
import           Data.Semigroup
import           Data.Semigroup.Instances
import           Data.Traversable                        (Traversable,
                                                          sequenceA)
import           Data.VectorSpace                        hiding (Sum (..))
import           System.Process


import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Articulation
import           Music.Score.Color
import           Music.Time.Internal.Convert             (reactiveToVoice')
import           Music.Score.Dynamics
import           Music.Score.Export.ArticulationNotation
import           Music.Score.Export.Backend
import           Music.Score.Export.DynamicNotation
import           Music.Score.Harmonics
import           Music.Score.Internal.Export             hiding (MVoice)
import           Music.Score.Internal.Util               (composed,
                                                          retainUpdates, swap,
                                                          unRatio)
import           Music.Score.Meta
import           Music.Score.Meta.Attribution
import           Music.Score.Meta.Tempo
import           Music.Score.Meta.Time
import           Music.Score.Meta.Title
import           Music.Score.Part
import           Music.Score.Phrases
import           Music.Score.Slide
import           Music.Score.Text
import           Music.Score.Ties
import           Music.Score.Tremolo
import           Music.Time
import           Music.Score.Internal.Quantize

import qualified Text.Pretty                             as Pretty
import qualified Music.MusicXml.Simple                   as MusicXml


-- |
-- Extract instrument info as per "Music.Part"
-- This is really crude, needs rethinking!
--
class HasMusicXmlInstrument a where
  getMusicXmlClef :: a -> Int




-- TODO move
deriving instance Show MusicXml.Line
deriving instance Show MusicXml.ClefSign
-- deriving instance Eq MusicXml.PartList
-- deriving instance Show MusicXml.PartList
-- deriving instance Eq MusicXml.PartListElem
-- deriving instance Show MusicXml.PartListElem
-- deriving instance Show MusicXml.GroupBarLines
-- FIXME bogus
instance Eq MusicXml.PartList where
instance Show MusicXml.PartList where



-- | A token to represent the MusicXml backend.
data MusicXml

data ScoreInfo = ScoreInfo { scoreTitle    :: String,
                             scoreComposer :: String,
                             scorePartList :: MusicXml.PartList,
                             scoreTempo    :: Tempo
                             }
  deriving (Eq, Show)


data StaffInfo = StaffInfo { staffClef :: (MusicXml.ClefSign, MusicXml.Line) }
  deriving (Eq, Show)

data BarInfo = BarInfo { x_barTimeSignature :: Maybe TimeSignature }
  deriving (Eq, Show)

-- | Hierachical representation of a MusicXml score.
--   A score is a parallel composition of staves.
data XmlScore a = XmlScore { getXmlScore :: (ScoreInfo, [XmlStaff a]) }
  deriving (Functor, Eq, Show)

-- | A staff is a sequential composition of bars.
data XmlStaff a = XmlStaff { getXmlStaff :: (StaffInfo, [XmlBar a]) }
  deriving (Functor, Eq, Show)

-- | A bar is a sequential composition of chords/notes/rests.
data XmlBar a = XmlBar { getXmlBar :: (BarInfo, Rhythm a) }
  deriving (Functor, Eq, Show)

-- | Context passed to the note export.
--   Includes duration and note/rest distinction.
data XmlContext a = XmlContext Duration (Maybe a)
  deriving (Functor, Foldable, Traversable, Eq, Show)

-- instance Monoid Lilypond.Music where
  -- mempty      = pcatXml []
  -- mappend x y = pcatXml [x,y]

instance HasBackend MusicXml where
  type BackendScore MusicXml   = XmlScore
  type BackendContext MusicXml = XmlContext
  type BackendNote MusicXml    = MusicXml.Music
  type BackendMusic MusicXml   = MusicXml.Score

  finalizeExport _ = finalizeScore
    where

finalizeScore :: XmlScore MusicXml.Music -> MusicXml.Score
finalizeScore (XmlScore (info, x))
  = MusicXml.fromParts title composer partList
  . map (finalizeStaff tempo) $ x
  where
    title    = scoreTitle info
    composer = scoreComposer info
    partList = scorePartList info
    tempo    = scoreTempo info

-- TODO finalizeStaffGroup

finalizeStaff :: Tempo -> XmlStaff MusicXml.Music -> [MusicXml.Music]
finalizeStaff tempo (XmlStaff (info, x))
  = id
  -- Staff name is not added here as MusicXML uses a separate part list
  . addStartInfo
  . addClef (staffClef info)
  . map finalizeBar $ x
  where
    -- TODO name
    -- TODO clef

    -- Both of these stick to the first bar
    -- TODO clean
    addClef :: (MusicXml.ClefSign, MusicXml.Line) -> [MusicXml.Music] -> [MusicXml.Music]
    addClef _                []     = []
    addClef (clefSign, line) (x:xs) = (MusicXml.clef clefSign line <> x):xs

    addStartInfo :: [MusicXml.Music] -> [MusicXml.Music]
    addStartInfo []     = []
    addStartInfo (x:xs) = (startInfo <> x):xs

    startInfo :: MusicXml.Music
    startInfo = mempty
        <> MusicXml.defaultDivisions
        <> MusicXml.defaultKey
        <> MusicXml.metronome (realToFrac nv) (realToFrac bpm)
        -- <> Xml.commonTime
    (nv, bpm) = getTempo tempo


finalizeBar :: XmlBar MusicXml.Music -> MusicXml.Music
finalizeBar (XmlBar (BarInfo timeSignature, x))
  = maybe id setBarTimeSignature timeSignature
  . renderBarMusic $ x
  where
    -- TODO key signatures
    -- TODO rehearsal marks
    -- TODO bar number change
    -- TODO compound time signatures
    setBarTimeSignature (getTimeSignature -> (ms, n)) x = mconcat [MusicXml.time (fromIntegral $ sum ms) (fromIntegral n), x]

renderBarMusic :: Rhythm MusicXml.Music -> MusicXml.Music
renderBarMusic = go
  where
    go (Beat d x)            = setDefaultVoice x
    go (Dotted n (Beat d x)) = setDefaultVoice x
    go (Group rs)            = mconcat $ map renderBarMusic rs
    go (Tuplet m r)          = MusicXml.tuplet b a (renderBarMusic r)
      where
        (a,b) = bimap fromIntegral fromIntegral $ unRatio $ realToFrac m

setDefaultVoice :: MusicXml.Music -> MusicXml.Music
setDefaultVoice = MusicXml.setVoice 1

{-
Sibelius quirks

Only looks at the part-name attribute (use part-name-display/display-text to override name)
Expects transposition first, i.e. "C Trumpet, Bb Clarinet"
Expects "French Horn", "Cello" and "English Horn"
Does not like numbers (I, II etc) after the instrument name
-}
toAcceptableName :: Show a => a -> (String, String)
toAcceptableName x = (overrideName defName, defName)
  where
    defName = show x
    overrideName y
      | "Clarinet"      `Data.List.isPrefixOf` y = "Bb Clarinet"
      | "Clarinet in A" `Data.List.isPrefixOf` y = "A Clarinet"
      | "Bassoon"       `Data.List.isPrefixOf` y = "Bassoon"
      | "Violoncello"   `Data.List.isPrefixOf` y = "Cello"
      | "Trumpet in Bb" `Data.List.isPrefixOf` y = "Bb Trumpet"
      | "Trumpet in C"  `Data.List.isPrefixOf` y = "C Trumpet"
      | "Trumpet in C"  `Data.List.isPrefixOf` y = "C Trumpet"
      | "Horn"          `Data.List.isPrefixOf` y = "French Horn"
      | otherwise                                = y

instance (
  HasDynamicNotation a b c,
  HasOrdPart a, Transformable a, Semigroup a,
  HasOrdPart c, Tiable c, Show (Part a), HasMusicXmlInstrument (Part a)
  )
  => HasBackendScore MusicXml (Score a) where
  type BackendScoreEvent MusicXml (Score a) = SetDynamic DynamicNotation a
  exportScore b score = XmlScore
    . (ScoreInfo title composer partList tempo,)
    . map (uncurry $ exportPart timeSignatureMarks barDurations)
    . map (second (over dynamics notateDynamic))
    . map (second (preserveMeta addDynCon))
    . map (second (preserveMeta simultaneous))
    . extractPartsWithInfo
    . normalizeScore
    $ score
    where
      title    = fromMaybe "" $ flip getTitleAt 0              $ metaAtStart score
      composer = fromMaybe "" $ flip getAttribution "composer" $ metaAtStart score
      partList = MusicXml.partListDisplay (fmap toAcceptableName $ allParts score)
      tempo    = (metaAtStart score :: Tempo)
      (timeSignatureMarks, barDurations) = extractTimeSignatures score


      -- | Export a score as a single part. Overlapping notes will cause an error.
      exportPart :: (
        Tiable a,
        HasMusicXmlInstrument (Part a)
        )
        => [Maybe TimeSignature]
        -> [Duration]
        -> Part a
        -> Score a
        -> XmlStaff (XmlContext a)

      exportStaff :: Tiable a
        => [Maybe TimeSignature]
        -> [Duration]
        -> Int    -- ^ clef, as per Music.Parts
        -> MVoice a
        -> XmlStaff (XmlContext a)

      exportBar :: Tiable a
        => Maybe TimeSignature
        -> MVoice a
        -> XmlBar (XmlContext a)

      quantizeBar :: Tiable a
        => MVoice a
        -> Rhythm (XmlContext a)

      exportPart timeSignatureMarks barDurations part
        = exportStaff timeSignatureMarks barDurations (getMusicXmlClef part)
        . view singleMVoice

      exportStaff timeSignatures barDurations clefId
        = XmlStaff
        . addStaffInfo
        . zipWith exportBar timeSignatures
        . splitIntoBars barDurations
        where
          clef = case clefId of
            0 -> (MusicXml.GClef, 2)
            1 -> (MusicXml.CClef, 3)
            2 -> (MusicXml.FClef, 4)
          addStaffInfo  = (,) $ StaffInfo { staffClef = clef }
          splitIntoBars = splitTiesAt

      exportBar timeSignature
        = XmlBar
        . addBarInfo
        . quantizeBar
       where
         addBarInfo = (,) $ BarInfo timeSignature

      quantizeBar = mapWithDur XmlContext . rewrite . handleErrors . quantize . view pairs
        where
          -- FIXME propagate quantization errors
          handleErrors (Left e)  = error $ "Quantization failed: " ++ e
          handleErrors (Right x) = x

--------------------------------------------------------------------------------

{-
  Note:
    We want all note transformers to be applicative morphisms, i.e.

      notate (pure x)   = pure (notate x)

    Specifically
      notate (mempty,x) = id . notate x

  Note:
    We use these idioms:
      exportNote b = exportNote b . fmap extract
      exportNote b = uncurry notate . fmap (exportNote b) . getTieT . sequenceA

   The latter looks a lot like cotraverse. Generalization?


-}

instance HasBackendNote MusicXml a => HasBackendNote MusicXml [a] where
  exportNote = exportChord

instance HasBackendNote MusicXml Integer where
  -- TODO can we get rid of exportChord alltogether and just use XmlContext?
  exportNote  _ (XmlContext d Nothing)    = MusicXml.rest (realToFrac d)
  exportNote  _ (XmlContext d (Just x))   = (`MusicXml.note` realToFrac d)  . spellMusicXml . fromIntegral $ x

  exportChord _ (XmlContext d Nothing)    = MusicXml.rest (realToFrac d)
  exportChord _ (XmlContext d (Just xs))  = (`MusicXml.chord` realToFrac d) . fmap (spellMusicXml . fromIntegral) $ xs
  -- getMusicXml      d = (`Xml.note` realToFrac d)  . spellMusicXml . fromIntegral
  -- getMusicXmlChord d = (`Xml.chord` realToFrac d) . fmap (spellMusicXml . fromIntegral)


instance HasBackendNote MusicXml Int where
  exportNote b = exportNote b . fmap toInteger
  exportChord b = exportChord b . fmap (fmap toInteger)

instance HasBackendNote MusicXml Float where
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance HasBackendNote MusicXml Double where
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance Integral a => HasBackendNote MusicXml (Ratio a) where
  exportNote b = exportNote b . fmap (toInteger . round)

instance HasBackendNote MusicXml a => HasBackendNote MusicXml (Behavior a) where
  exportNote b = exportNote b . fmap (! 0)
  exportChord b = exportChord b . fmap (fmap (! 0))

instance HasBackendNote MusicXml a => HasBackendNote MusicXml (Sum a) where
  exportNote b = exportNote b . fmap getSum

instance HasBackendNote MusicXml a => HasBackendNote MusicXml (Product a) where
  exportNote b = exportNote b . fmap getProduct

instance HasBackendNote MusicXml a => HasBackendNote MusicXml (PartT n a) where
  -- Part structure is handled by HasMidiBackendScore instances, so this is just an identity
  exportNote b = exportNote b . fmap extract
  exportChord b = exportChord b . fmap (fmap extract)

instance HasBackendNote MusicXml a => HasBackendNote MusicXml (DynamicT DynamicNotation a) where
  exportNote b = uncurry notate . fmap (exportNote b) . getDynamicT . sequenceA
    where
      notate (DynamicNotation (crescDims, level))
        = composed (fmap notateCrescDim crescDims)
        . notateLevel level

      notateCrescDim crescDims = case crescDims of
        NoCrescDim -> id
        BeginCresc -> (<>) MusicXml.beginCresc
        EndCresc   -> (<>) MusicXml.endCresc
        BeginDim   -> (<>) MusicXml.beginDim
        EndDim     -> (<>) MusicXml.endDim

      -- TODO these literals are not so nice...
      notateLevel showLevel = case showLevel of
         Nothing -> id
         Just lvl -> (<>) $ MusicXml.dynamic (fromDynamics (DynamicsL (Just (fixLevel . realToFrac $ lvl), Nothing)))

      fixLevel :: Double -> Double
      fixLevel x = fromIntegral (round (x - 0.5)) + 0.5

      -- DO NOT use rcomposed as notateDynamic returns "mark" order, not application order
      -- rcomposed = composed . reverse

instance HasBackendNote MusicXml a => HasBackendNote MusicXml (ArticulationT b{-ArticulationNotation-} a) where
  exportNote b = exportNote b . fmap extract
  -- exportNote b = uncurry notate . fmap (exportNote b) . getArticulationT . sequenceA
  --   where
  --     notate (ArticulationNotation (slurs, marks))
  --       = composed (fmap notateMark marks)
  --       . composed (fmap notateSlur slurs)
  --
  --     notateMark mark = case mark of
  --       NoMark         -> id
  --       Staccato       -> MusicXml.staccato
  --       MoltoStaccato  -> MusicXml.staccatissimo
  --       Marcato        -> MusicXml.strongAccent
  --       Accent         -> MusicXml.accent
  --       Tenuto         -> MusicXml.tenuto
  --
  --     notateSlur slurs = case slurs of
  --       NoSlur    -> id
  --       BeginSlur -> MusicXml.beginSlur
  --       EndSlur   -> MusicXml.endSlur

instance HasBackendNote MusicXml a => HasBackendNote MusicXml (ColorT a) where
  exportNote b = exportNote b . fmap extract
{-
  exportNote b = uncurry notate . fmap (exportNote b) . getCouple . getColorT . sequenceA
    where
      -- TODO This syntax will change in future MusicXml versions
      -- TODO handle any color
      notate (Option Nothing)             = id
      notate (Option (Just (Last color))) = \x -> MusicXml.Sequential [
        MusicXml.Override "NoteHead#' color"
          (MusicXml.toLiteralValue $ "#" ++ colorName color),
        x,
        MusicXml.Revert "NoteHead#' color"
        ]

      colorName c
        | c == Color.black = "black"
        | c == Color.red   = "red"
        | c == Color.blue  = "blue"
        | otherwise        = error "MusicXml backend: Unkown color"
-}
instance HasBackendNote MusicXml a => HasBackendNote MusicXml (TremoloT a) where
  exportNote b = uncurry notate . fmap (exportNote b) . runTremoloT . sequenceA
    where
      notate n = case n of
        0 -> id
        n -> MusicXml.tremolo (fromIntegral n)

instance HasBackendNote MusicXml a => HasBackendNote MusicXml (TextT a) where
  exportNote b = uncurry notate . fmap (exportNote b) . getCouple . getTextT . sequenceA
    where
      notate texts a = mconcat (fmap MusicXml.text texts) <> a

instance HasBackendNote MusicXml a => HasBackendNote MusicXml (HarmonicT a) where
  exportNote b = uncurry notateX . fmap (exportNote b) . getCouple . getHarmonicT . sequenceA
    where
      notateX (Any isNat, Sum n) = notate isNat n

      notate _     0 = id
      notate True  n = notateNatural n
      notate False n = notateArtificial n

      -- notateNatural n = Xml.harmonic -- openString?
      notateNatural n = MusicXml.setNoteHead MusicXml.DiamondNoteHead
      -- Most programs do not recognize the harmonic tag
      -- We set a single diamond notehead instead, which can be manually replaced
      notateArtificial n = id -- TODO


instance HasBackendNote MusicXml a => HasBackendNote MusicXml (SlideT a) where
  exportNote b = uncurry notateX . fmap (exportNote b) . getCouple . getSlideT . sequenceA
    where
      notateX ((eg,es),(bg,bs)) = notate
          where
              notate = neg . nes . nbg . nbs
              neg    = if view _Wrapped' eg then MusicXml.endGliss else id
              nes    = if view _Wrapped' es then MusicXml.endSlide else id
              nbg    = if view _Wrapped' bg then MusicXml.beginGliss else id
              nbs    = if view _Wrapped' bs then MusicXml.beginSlide else id

instance HasBackendNote MusicXml a => HasBackendNote MusicXml (TieT a) where
  exportNote b  = uncurry notateTie . getTieT . fmap (exportNote b) . sequenceA
  exportChord b = uncurry notateTie . getTieT . fmap (exportChord b) . sequenceA . fmap sequenceA

notateTie (Any ta, Any tb)
  | ta && tb  = MusicXml.beginTie . MusicXml.endTie -- TODO flip order?
  | tb        = MusicXml.beginTie
  | ta        = MusicXml.endTie
  | otherwise = id

-- |
-- Constraint for types that has a MusicXML representation.
--
type HasMusicXml a = (HasBackendNote MusicXml (BackendScoreEvent MusicXml a), HasBackendScore MusicXml a)

-- |
-- Convert a score to a MusicXML score.
--
toMusicXml :: HasMusicXml a => a -> MusicXml.Score
toMusicXml = export (undefined::MusicXml)

-- |
-- Convert a score to a MusicXML string.
--
toMusicXmlString :: HasMusicXml a => a -> String
toMusicXmlString = MusicXml.showXml . toMusicXml

-- |
-- Convert a score to MusicXML string and print it on the standard output.
--
showMusicXml :: HasMusicXml a => a -> IO ()
showMusicXml = putStrLn . toMusicXmlString

-- |
-- Convert a score to MusicXML and write to a file.
--
writeMusicXml :: HasMusicXml a => FilePath -> a -> IO ()
writeMusicXml path = writeFile path . toMusicXmlString

-- |
-- Typeset a score using MusicXML and open it. (This is simple wrapper around
-- 'writeMusicXml' that may not work well on all platforms.)
--
openMusicXml :: HasMusicXml a => a -> IO ()
openMusicXml sc = do
    writeMusicXml "test.xml" sc
    -- TODO find out which program to use etc...
    void $ rawSystem "open" ["-a", "Sibelius 7", "test.xml"]


-- Internal
spellMusicXml :: Integer -> MusicXml.Pitch
spellMusicXml p = (
    toEnum $ fromIntegral pc,
    if alt == 0 then Nothing else Just (fromIntegral alt),
    fromIntegral oct
    )
    where (pc,alt,oct) = spellPitch (p + 60)
-- End internal

