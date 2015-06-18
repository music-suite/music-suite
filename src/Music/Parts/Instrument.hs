
-- | Representation of musical instruments.
module Music.Parts.Instrument (
        Instrument(..),
        -- TODO hide impl
        
        -- * Name etc
        -- instrumentName,
        fullName,
        shortName,

        -- * Clefs and transposition
        transposition,
        transpositionString,
        standardClef,
        allowedClefs,

        -- * Playing range
        playableRange,
        comfortableRange,
        -- playableDynamics,
        
        -- * Legacy
        gmClef,
        gmMidiChannel,
        gmScoreOrder,
        -- gmInstrName,
  ) where

import           Control.Applicative
import Music.Pitch.Clef
import           Control.Lens                    (toListOf)
import           Data.Default
import           Data.Functor.Adjunction         (unzipR)
import qualified Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Option.Instances
import           Data.Set                        (Set)
import           Data.Map                        (Map)
import           Data.Traversable                (traverse)
import           Data.Typeable
import           Music.Dynamics                  (Dynamics)
import           Music.Pitch                     (Ambitus, Clef, trebleClef, bassClef)
import           Music.Pitch.Common              (Interval, Pitch)
import           Text.Numeral.Roman              (toRoman)

import Music.Parts.Internal.Data as Data
{-
Semantically, our instrument type is superset of the MusicXML Standard Sounds 3.0
  See http://www.musicxml.com/for-developers/standard-sounds/

All extensions has ".x." as part of their ID!
-}




-- | An 'Instrument' represents the set of all instruments of a given type.
data Instrument
    = StdInstrument Int
    | OtherInstrument String
    deriving (Eq)

instance Show Instrument where
    show (StdInstrument x) = fromMaybe "(unknown)" $ gmInstrName x
    show (OtherInstrument str) = go str
        where
            go "wind.flutes.flute.alto"   = "Alto Flute"
            go "wind.flutes.flute.bass"   = "Bass Flute"
            go "wind.reed.clarinet.eflat" = "Clarinet in Eb"
            go "wind.reed.clarinet.bass"  = "Bass Clarinet in Bb"
            go "wind.reed.contrabassoon"  = "Contrabassoon"
            go x = x
instance Enum Instrument where
    toEnum = StdInstrument
    fromEnum (StdInstrument x) = x
    fromEnum (OtherInstrument _) = error "Instrument.fromEnum used on unknown instrument"

instance Ord Instrument where
    StdInstrument x   `compare` StdInstrument   y = gmScoreOrder x `compare` gmScoreOrder y
    OtherInstrument x `compare` OtherInstrument y = x `compare` y
    StdInstrument x   `compare` OtherInstrument y = LT
    OtherInstrument x `compare` StdInstrument   y = GT

-- | This instance is quite arbitrary but very handy.
instance Default Instrument where
    def = StdInstrument 0










allowedClefs      :: Instrument -> Set Clef
allowedClefs = error "No allowedClefs"

standardClef      :: Instrument -> Maybe Clef
standardClef = error "No standardClef"

data BracketType = Bracket | Brace | SubBracket
data StaffLayout = Staff Clef | Staves BracketType [StaffLayout]

pianoStaff :: StaffLayout
pianoStaff = Staves Brace [Staff trebleClef, Staff bassClef]


playableRange     :: Instrument -> Ambitus Pitch
playableRange = error "No playableRange"

comfortableRange  :: Instrument -> Ambitus Pitch
comfortableRange = error "No comfortableRange"

-- playableDynamics :: Instrument -> Pitch -> Dynamics
-- playableDynamics = error "No playableDynamics"

-- instrumentName              :: Instrument -> String
-- instrumentName = error "No name"

fullName          :: Instrument -> String
fullName = error "No fullName"

shortName         :: Instrument -> String
shortName = error "No shortName"

-- sounding .-. written, i.e. -P5 for horn
transposition     :: Instrument -> Interval
transposition = error "No transposition"

-- pitch sounding when c is notated (i.e. F for Horn in F)
transpositionString :: Instrument -> String
transpositionString = error "No transpositionString"










-- Legacy

gmClef :: Int -> Int
gmMidiChannel :: Int -> Int
gmScoreOrder :: Int -> Double
gmInstrName :: Int -> Maybe String


gmClef x = fromMaybe 0 $ fmap (go . _standardClef) $ Data.getInstrumentDefByGeneralMidiProgram x
  where
    go cs | cs == [trebleClef] = 0
    go cs | cs == [altoClef]   = 1
    go cs | cs == [bassClef]   = 2
gmScoreOrder x = fromMaybe 0 $ fmap (_scoreOrder) $ Data.getInstrumentDefByGeneralMidiProgram x
gmMidiChannel x = fromMaybe 0 $ (=<<) (_defaultMidiChannel) $ Data.getInstrumentDefByGeneralMidiProgram x
gmInstrName x = (=<<) (_longName) $ Data.getInstrumentDefByGeneralMidiProgram x
  

