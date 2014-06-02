
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

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

module Music.Score.Export.Lilypond (
    -- * Lilypond backend
    HasLilypondInstrument(..),
    Lilypond,
    LyContext(..),
    HasLilypond,
    
    -- ** Converting to Lilypond
    toLilypond,
    toLilypondString,    
    
    -- ** Lilypond I/O
    showLilypond,
    openLilypond,
    writeLilypond,

    -- ** Customize Lilypond backend
    LilypondOptions(..),
    openLilypond',
    writeLilypond',
  ) where

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import qualified Codec.Midi                    as Midi
import           Control.Arrow                 ((***))
import           Control.Comonad               (Comonad (..), extract)
import           Control.Applicative
import           Data.Colour.Names             as Color
import           Data.Default
import           Data.Foldable                 (Foldable)
import qualified Data.Foldable
import           Data.Functor.Couple
import           Data.Maybe
import           Data.Ratio
import           Data.Traversable              (Traversable, sequenceA)
import qualified Music.Lilypond                as Lilypond
import qualified Music.MusicXml.Simple         as MusicXml
import           Music.Score.Internal.Export   hiding (MVoice)
import           Music.Time.Internal.Transform (whilstLD)
import           System.Process
import           Music.Time.Internal.Quantize
import qualified Text.Pretty                   as Pretty
import qualified Data.List
import Music.Score.Convert (reactiveToVoice', voiceToScore) -- TODO
import           Music.Score.Internal.Util (composed, unRatio, swap, retainUpdates)
import Music.Score.Export.DynamicNotation
import Music.Score.Export.ArticulationNotation
import Data.Semigroup.Instances

import Music.Score.Export.Backend

import Data.Functor.Identity
import Data.Semigroup
import Control.Monad
import Data.VectorSpace hiding (Sum(..))
import Data.AffineSpace
import Control.Lens hiding (rewrite)
-- import Control.Lens.Operators hiding ((|>))

import Music.Time
import Music.Score.Meta
import Music.Score.Dynamics
import Music.Score.Articulation
import Music.Score.Part
import Music.Score.Tremolo
import Music.Score.Text
import Music.Score.Harmonics
import Music.Score.Slide
import Music.Score.Color
import Music.Score.Ties
import Music.Score.Export.Backend
import Music.Score.Meta.Time
import Music.Score.Phrases








-- | 
-- Extract instrument info as per "Music.Part"
-- This is really crude, needs rethinking!
--
class HasLilypondInstrument a where
  getLilypondClef :: a -> Int


{-
  TODO rewrite so that:
    - Each bar may include voices/layers (a la Sibelius)
    - Each part may generate more than one staff (for piano etc)
-}


-- | A token to represent the Lilypond backend.
data Lilypond

data ScoreInfo = ScoreInfo
  deriving (Eq, Show)

data StaffInfo = StaffInfo { staffName :: String, 
                             staffClef :: Lilypond.Clef } 
  deriving (Eq, Show)

data BarInfo = BarInfo { barTimeSignature :: Maybe TimeSignature } 
  deriving (Eq, Show)

-- | Hierachical representation of a Lilypond score.
--   A score is a parallel composition of staves.
data LyScore a = LyScore { getLyScore :: (ScoreInfo, [LyStaff a]) }
  deriving (Functor, Eq, Show)

-- | A staff is a sequential composition of bars.
data LyStaff a = LyStaff { getLyStaff :: (StaffInfo, [LyBar a]) }
  deriving (Functor, Eq, Show)

-- | A bar is a sequential composition of chords/notes/rests.
data LyBar a = LyBar { getLyBar :: (BarInfo, Rhythm a) } 
  deriving (Functor, Eq, Show)

-- | Context passed to the note export.
--   Includes duration and note/rest distinction.
data LyContext a = LyContext Duration (Maybe a)
  deriving (Functor, Foldable, Traversable, Eq, Show)

instance Monoid Lilypond.Music where
  mempty      = pcatLy []
  mappend x y = pcatLy [x,y]

type LyMusic = Lilypond.Music

instance HasBackend Lilypond where
  type BackendScore Lilypond   = LyScore
  type BackendContext Lilypond = LyContext
  type BackendNote Lilypond    = LyMusic
  type BackendMusic Lilypond   = LyMusic

  finalizeExport _ = finalizeScore
    where
      finalizeScore :: LyScore LyMusic -> Lilypond.Music
      finalizeScore (LyScore (info, x)) 
        = pcatLy 
        . map finalizeStaff $ x
        where
          extra = id

      -- TODO finalizeStaffGroup

      finalizeStaff :: LyStaff LyMusic -> LyMusic
      finalizeStaff (LyStaff (info, x)) 
        = addStaff 
        . addPartName (staffName info) 
        . addClef (staffClef info)
        . scatLy 
        . map finalizeBar $ x
        where
          addStaff                = Lilypond.New "Staff" Nothing
          addClef c x             = scatLy [Lilypond.Clef c, x]
          addPartName partName xs = scatLy [longName, shortName, xs]
            where
              longName  = Lilypond.Set "Staff.instrumentName" (Lilypond.toValue partName)
              shortName = Lilypond.Set "Staff.shortInstrumentName" (Lilypond.toValue partName)

      finalizeBar :: LyBar LyMusic -> LyMusic
      finalizeBar (LyBar (BarInfo timeSignature, x))
        = maybe id setBarTimeSignature timeSignature 
        . renderBarMusic $ x
        where
          -- TODO key signatures
          -- TODO rehearsal marks
          -- TODO bar number change
          -- TODO compound time signatures
          setBarTimeSignature (getTimeSignature -> (ms, n)) x = scatLy [Lilypond.Time (sum ms) n, x]
          
      renderBarMusic :: Rhythm LyMusic -> LyMusic
      renderBarMusic = go
        where
          go (Beat d x)            = Lilypond.removeSingleChords x
          go (Dotted n (Beat d x)) = Lilypond.removeSingleChords x
          go (Group rs)            = scatLy $ map renderBarMusic rs
          go (Tuplet m r)          = Lilypond.Times (realToFrac m) (renderBarMusic r)
            where
              (a,b) = fromIntegral *** fromIntegral $ unRatio $ realToFrac m

-- TODO move
second f (x, y) = (x, f y)

type HasArticulation' a = HasArticulation a a

type HasArticulation3 a a' a'' = (
  HasArticulation' a,
  HasArticulation a  a',
  HasArticulation a' a'',
  HasArticulation a  a''
  )

type HasArticulationNotation a b c = (
  HasArticulation3 a b c,
  Articulation b  ~ Ctxt (Articulation a),
  Articulation c ~ ArticulationNotation,
  Real (Articulation a),
  Part (SetArticulation (Articulation a) a) ~ Part (SetArticulation ArticulationNotation b)
 )


instance (         
  SetArticulation
                          ArticulationNotation (SetDynamic DynamicNotation b)
                        ~ SetArticulation
                            ArticulationNotation
                            (SetArticulation
                               (Maybe (Articulation (SetDynamic DynamicNotation b)),
                                Articulation (SetDynamic DynamicNotation b),
                                Maybe (Articulation (SetDynamic DynamicNotation b)))
                               (SetDynamic DynamicNotation b)),
  Part
                        (SetArticulation
                           ArticulationNotation
                           (SetArticulation
                              (Maybe (Articulation (SetDynamic DynamicNotation b)),
                               Articulation (SetDynamic DynamicNotation b),
                               Maybe (Articulation (SetDynamic DynamicNotation b)))
                              (SetDynamic DynamicNotation b)))
                      ~ Part (SetDynamic DynamicNotation b),
  Articulation
                        (SetArticulation
                           ArticulationNotation
                           (SetArticulation
                              (Maybe (Articulation (SetDynamic DynamicNotation b)),
                               Articulation (SetDynamic DynamicNotation b),
                               Maybe (Articulation (SetDynamic DynamicNotation b)))
                              (SetDynamic DynamicNotation b)))
                      ~ ArticulationNotation,
  Articulation
                        (SetArticulation
                           (Maybe (Articulation (SetDynamic DynamicNotation b)),
                            Articulation (SetDynamic DynamicNotation b),
                            Maybe (Articulation (SetDynamic DynamicNotation b)))
                           (SetDynamic DynamicNotation b))
                      ~ (Maybe (Articulation (SetDynamic DynamicNotation b)),
                         Articulation (SetDynamic DynamicNotation b),
                         Maybe (Articulation (SetDynamic DynamicNotation b))),
  Tiable
                        (SetArticulation
                           ArticulationNotation
                           (SetArticulation
                              (Maybe (Articulation (SetDynamic DynamicNotation b)),
                               Articulation (SetDynamic DynamicNotation b),
                               Maybe (Articulation (SetDynamic DynamicNotation b)))
                              (SetDynamic DynamicNotation b))),
  HasArticulation
                        (SetDynamic DynamicNotation b)
                        (SetArticulation
                           (Maybe (Articulation (SetDynamic DynamicNotation b)),
                            Articulation (SetDynamic DynamicNotation b),
                            Maybe (Articulation (SetDynamic DynamicNotation b)))
                           (SetDynamic DynamicNotation b)),
  HasArticulations
                        (SetArticulation
                           (Maybe (Articulation (SetDynamic DynamicNotation b)),
                            Articulation (SetDynamic DynamicNotation b),
                            Maybe (Articulation (SetDynamic DynamicNotation b)))
                           (SetDynamic DynamicNotation b))
                        (SetArticulation
                           ArticulationNotation
                           (SetArticulation
                              (Maybe (Articulation (SetDynamic DynamicNotation b)),
                               Articulation (SetDynamic DynamicNotation b),
                               Maybe (Articulation (SetDynamic DynamicNotation b)))
                              (SetDynamic DynamicNotation b))),
  HasArticulation
                        (SetDynamic DynamicNotation b) (SetDynamic DynamicNotation b),


  -- TODO generalize
  (Articulation (SetDynamic DynamicNotation b)) ~ (Product Double, Product Double),

  -- HasArticulationNotation c d e,

  HasDynamicNotation a b c,
  HasOrdPart a, Transformable a, Semigroup a,
  HasOrdPart c, Show (Part c), HasLilypondInstrument (Part c), Tiable c
  )
  => HasBackendScore Lilypond (Score a) where
  type BackendScoreEvent Lilypond (Score a) = SetArticulation ArticulationNotation (SetDynamic DynamicNotation a)
  exportScore b score = LyScore 
    . (ScoreInfo,)
    . map (uncurry $ exportPart timeSignatureMarks barDurations)
    . map (second (over articulations notateArticulation)) 
    . map (second (preserveMeta addArtCon))
    . map (second (over dynamics notateDynamic)) 
    . map (second (preserveMeta addDynCon))
    . map (second (preserveMeta simultaneous)) 
    . extractParts'
    $ score
    where
      (timeSignatureMarks, barDurations) = extractTimeSignatures score 


      -- | Export a score as a single part. Overlapping notes will cause an error.
      exportPart :: (
        Show (Part a),
        HasLilypondInstrument (Part a),
        Tiable a
        ) 
        => [Maybe TimeSignature] 
        -> [Duration] 
        -> Part a 
        -> Score a 
        -> LyStaff (LyContext a)

      exportStaff :: Tiable a 
        => [Maybe TimeSignature] 
        -> [Duration] 
        -> String -- ^ name
        -> Int    -- ^ clef, as per Music.Parts
        -> MVoice a 
        -> LyStaff (LyContext a)

      exportBar :: Tiable a 
        => Maybe TimeSignature 
        -> MVoice a 
        -> LyBar (LyContext a)

      quantizeBar :: Tiable a 
        => MVoice a 
        -> Rhythm (LyContext a)

      exportPart timeSignatureMarks barDurations part
        = exportStaff timeSignatureMarks barDurations (show part) (getLilypondClef part)
        . view singleMVoice

      exportStaff timeSignatures barDurations name clefId 
        = LyStaff 
        . addStaffInfo
        . zipWith exportBar timeSignatures 
        . splitIntoBars barDurations
        where
          clef = case clefId of
            0 -> Lilypond.Treble
            1 -> Lilypond.Alto
            2 -> Lilypond.Bass
          addStaffInfo  = (,) $ StaffInfo { staffName = name, staffClef = clef }
          splitIntoBars = splitTiesVoiceAt

      exportBar timeSignature
        = LyBar 
        . addBarInfo 
        . quantizeBar
       where
         addBarInfo = (,) $ BarInfo timeSignature

      quantizeBar = mapWithDur LyContext . rewrite . handleErrors . quantize . view eventsV
        where
          -- FIXME propagate quantization errors
          handleErrors (Left e)  = error $ "Quantization failed: " ++ e
          handleErrors (Right x) = x

-- TODO move
addArtCon :: (
  HasPhrases s t a b, HasArticulation a a, HasArticulation a b, 
  Articulation a ~ d, Articulation b ~ Ctxt d
  ) => s -> t
addArtCon = over (phrases.varticulation) withContext
varticulation = lens (fmap $ view articulation) (flip $ zipVoiceWithNoScale (set articulation))



{-
-- TODO customize and remove extraction-related constraints
instance (
  HasDynamicNotation a b c,
  HasOrdPart a, Transformable a, Semigroup a,
  HasOrdPart c, Show (Part c), HasLilypondInstrument (Part c), Tiable c
  )
  => HasBackendScore Lilypond (Voice a) where
  type BackendScoreEvent Lilypond (Voice a) = SetDynamic DynamicNotation a
  exportScore b = exportScore b . voiceToScore
-}

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

instance HasBackendNote Lilypond a => HasBackendNote Lilypond [a] where
  exportNote = exportChord

instance HasBackendNote Lilypond Integer where
  -- TODO can we get rid of exportChord alltogether and just use LyContext?
  exportNote  _ (LyContext d Nothing)    = (^*realToFrac (4*d)) Lilypond.rest
  exportNote  _ (LyContext d (Just x))   = (^*realToFrac (4*d)) $ Lilypond.note $ spellLy x

  exportChord _ (LyContext d Nothing)    = (^*realToFrac (4*d)) Lilypond.rest
  exportChord _ (LyContext d (Just xs))  = (^*realToFrac (4*d)) $ Lilypond.chord $ fmap spellLy xs

instance HasBackendNote Lilypond Int where
  exportNote b = exportNote b . fmap toInteger
  exportChord b = exportChord b . fmap (fmap toInteger)

instance HasBackendNote Lilypond Float where
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance HasBackendNote Lilypond Double where
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance Integral a => HasBackendNote Lilypond (Ratio a) where
  exportNote b = exportNote b . fmap (toInteger . round)

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (Behavior a) where
  exportNote b = exportNote b . fmap (! 0)
  exportChord b = exportChord b . fmap (fmap (! 0))

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (Sum a) where
  exportNote b = exportNote b . fmap getSum

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (Product a) where
  exportNote b = exportNote b . fmap getProduct

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (PartT n a) where
  -- Part structure is handled by HasMidiBackendScore instances, so this is just an identity
  exportNote b = exportNote b . fmap extract
  exportChord b = exportChord b . fmap (fmap extract)

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (DynamicT DynamicNotation a) where
  exportNote b = uncurry notate . fmap (exportNote b) . getDynamicT . sequenceA
    where
      notate :: DynamicNotation -> LyMusic -> LyMusic
      notate (DynamicNotation (crescDims, level)) 
        = rcomposed (fmap notateCrescDim crescDims) 
        . notateLevel level

      notateCrescDim crescDims = case crescDims of
        NoCrescDim -> id
        BeginCresc -> Lilypond.beginCresc
        EndCresc   -> Lilypond.endCresc
        BeginDim   -> Lilypond.beginDim
        EndDim     -> Lilypond.endDim

      -- TODO these literals are not so nice...
      notateLevel showLevel = case showLevel of
         Nothing -> id
         Just lvl -> Lilypond.addDynamics (fromDynamics (DynamicsL (Just (fixLevel . realToFrac $ lvl), Nothing)))
      
      fixLevel :: Double -> Double
      fixLevel x = fromIntegral (round (x - 0.5)) + 0.5

      -- Use rcomposed as notateDynamic returns "mark" order, not application order
      rcomposed = composed . reverse

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (ArticulationT ArticulationNotation a) where
  exportNote b = uncurry notate . fmap (exportNote b) . getArticulationT . sequenceA
    where
      notate :: ArticulationNotation -> LyMusic -> LyMusic
      notate _ = id
      -- TODO
    
instance HasBackendNote Lilypond a => HasBackendNote Lilypond (ColorT a) where
  exportNote b = uncurry notate . fmap (exportNote b) . getCouple . getColorT . sequenceA
    where
      -- TODO This syntax will change in future Lilypond versions
      -- TODO handle any color
      notate (Option Nothing)             = id
      notate (Option (Just (Last color))) = \x -> Lilypond.Sequential [
        Lilypond.Override "NoteHead#' color" 
          (Lilypond.toLiteralValue $ "#" ++ colorName color),
        x,
        Lilypond.Revert "NoteHead#' color"
        ]

      colorName c
        | c == Color.black = "black"
        | c == Color.red   = "red"
        | c == Color.blue  = "blue"
        | otherwise        = error "Lilypond backend: Unkown color"

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (TremoloT a) where
  -- TODO can this instance use the new shorter idiom?
  exportNote b (LyContext d x) =
    fst (notate x d) $ exportNote b $ LyContext (snd $ notate x d) (fmap extract x)
    where
      notate Nothing d                               = (id, d)
      notate (Just (TremoloT (Couple (Max 0, _)))) d = (id, d)
      notate (Just (TremoloT (Couple (Max n, _)))) d = let
        scale   = 2^n
        newDur  = (d `min` (1/4)) / scale
        repeats = d / newDur
        in (Lilypond.Tremolo (round repeats), newDur)     

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (TextT a) where
  exportNote b = uncurry notate . fmap (exportNote b) . getCouple . getTextT . sequenceA
    where
      notate texts = composed (fmap Lilypond.addText texts)

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (HarmonicT a) where
  exportNote b = uncurry notate . fmap (exportNote b) . getCouple . getHarmonicT . sequenceA
    where
      notate (Any isNat, Sum n) = case (isNat, n) of
        (_,     0) -> id
        (True,  n) -> notateNatural n
        (False, n) -> notateArtificial n
      notateNatural n = Lilypond.addFlageolet -- addOpen?
      notateArtificial n = id -- TODO

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (SlideT a) where
  exportNote b = uncurry notate . fmap (exportNote b) . getCouple . getSlideT . sequenceA
    where
      notate ((Any eg, Any es),(Any bg, Any bs))
        | bg  = Lilypond.beginGlissando
        | bs  = Lilypond.beginGlissando
        | otherwise = id

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (TieT a) where
  exportNote b = uncurry notate . fmap (exportNote b) . getTieT . sequenceA
    where
      notate (Any ta, Any tb)
        | ta && tb  = Lilypond.beginTie
        | tb        = Lilypond.beginTie
        | ta        = id
        | otherwise = id

-- |
-- Constraint for types that has a Lilypond representation.
--
type HasLilypond a = (HasBackendNote Lilypond (BackendScoreEvent Lilypond a), HasBackendScore Lilypond a)

-- |
-- Convert a score to a Lilypond string.
--
toLilypondString :: HasLilypond a => a -> String
toLilypondString = show . Pretty.pretty . toLilypond

-- |
-- Convert a score to a Lilypond representation.
--
toLilypond :: HasLilypond a => a -> Lilypond.Music
toLilypond = export (undefined::Lilypond)


-- |
-- Convert a score to a Lilypond representaiton and print it on the standard output.
--
showLilypond :: HasLilypond a => a -> IO ()
showLilypond = putStrLn . toLilypondString

-- |
-- Convert a score to a Lilypond representation and write to a file.
--
writeLilypond :: HasLilypond a => FilePath -> a -> IO ()
writeLilypond = writeLilypond' def

data LilypondOptions
  = LyInlineFormat
  | LyScoreFormat

instance Default LilypondOptions where
  def = LyInlineFormat

-- |
-- Convert a score to a Lilypond representation and write to a file.
--
writeLilypond' :: HasLilypond a => LilypondOptions -> FilePath -> a -> IO ()
writeLilypond' options path sc = writeFile path $ (lyFilePrefix ++) $ toLilypondString sc
  where
    -- title    = fromMaybe "" $ flip getTitleAt 0                  $ metaAtStart sc
    -- composer = fromMaybe "" $ flip getAttribution "composer"     $ metaAtStart sc
    title = ""
    composer = ""
    -- TODO generalize metaAtStart!

    lyFilePrefix = case options of
        LyInlineFormat -> lyInlinePrefix
        LyScoreFormat  -> lyScorePrefix

    lyInlinePrefix = mempty                                        ++
        "%%% Generated by music-score %%%\n"                       ++
        "\\include \"lilypond-book-preamble.ly\"\n"                ++
        "\\paper {\n"                                              ++
        "  #(define dump-extents #t)\n"                            ++
        "\n"                                                       ++
        "  indent = 0\\mm\n"                                       ++
        "  line-width = 210\\mm - 2.0 * 0.4\\in\n"                 ++
        "  ragged-right = ##t\n"                                   ++
        "  force-assignment = #\"\"\n"                             ++
        "  line-width = #(- line-width (* mm  3.000000))\n"        ++
        "}\n"                                                      ++
        "\\header {\n"                                             ++
        "  title = \"" ++ title ++ "\"\n"                          ++
        "  composer = \"" ++ composer ++ "\"\n"                    ++
        "}\n"                                                      ++
        "\\layout {\n"                                             ++
        "}"                                                        ++
        "\n\n"

    lyScorePrefix = mempty                                         ++
        "\\paper {"                                                ++
        "  indent = 0\\mm"                                         ++
        "  line-width = 210\\mm - 2.0 * 0.4\\in"                   ++
        "}"                                                        ++
        "\\header {\n"                                             ++
        "  title = \"" ++ title ++ "\"\n"                          ++
        "  composer = \"" ++ composer ++ "\"\n"                    ++
        "}\n"                                                      ++
        "\\layout {"                                               ++
        "}" ++
        "\n\n"


-- |
-- Typeset a score using Lilypond and open it.
--
-- (This is simple wrapper around 'writeLilypond' that may not work well on all platforms.)
--
openLilypond :: HasLilypond a => a -> IO ()
openLilypond = openLilypond' def

-- |
-- Typeset a score using Lilypond and open it.
--
-- (This is simple wrapper around 'writeLilypond' that may not work well on all platforms.)
--
openLilypond' :: HasLilypond a => LilypondOptions -> a -> IO ()
openLilypond' options sc = do
  writeLilypond' options "test.ly" sc
  runLilypond >> cleanLilypond >> runOpen
    where    
      runLilypond = void $ runCommand 
        "lilypond -f pdf test.ly"  >>= waitForProcess
      cleanLilypond = void $ runCommand 
        "rm -f test-*.tex test-*.texi test-*.count test-*.eps test-*.pdf test.eps"
      runOpen = void $ runCommand 
        $ openCommand ++ " test.pdf"

































-- Internal stuff
-- TODO This function is a workaround
-- Whenever it is used, we should make the original function preserve meta instead
preserveMeta :: (HasMeta a, HasMeta b) => (a -> b) -> a -> b
preserveMeta f x = let m = view meta x in set meta m (f x)

pcatLy :: [Lilypond.Music] -> Lilypond.Music
pcatLy = pcatLy' False

pcatLy' :: Bool -> [Lilypond.Music] -> Lilypond.Music
pcatLy' p = foldr Lilypond.simultaneous (Lilypond.Simultaneous p [])

scatLy :: [Lilypond.Music] -> Lilypond.Music
scatLy = foldr Lilypond.sequential (Lilypond.Sequential [])

spellLy :: Integer -> Lilypond.Note
spellLy a = Lilypond.NotePitch (spellLy' a) Nothing

spellLy' :: Integer -> Lilypond.Pitch
spellLy' p = Lilypond.Pitch (
  toEnum $ fromIntegral pc,
  fromIntegral alt,
  fromIntegral oct
  )
  where (pc,alt,oct) = spellPitch (p + 72)
-- End internal
