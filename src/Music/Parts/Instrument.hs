
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
import qualified Data.Set
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
import Music.Pitch
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
    show x = fromMaybe "(unknown)" $ fullName x
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
allowedClefs = Data.Set.fromList . _allowedClefs . fetchInstrumentDef

standardClef      :: Instrument -> Maybe Clef
standardClef = listToMaybe . _standardClef . fetchInstrumentDef

data BracketType = Bracket | Brace | SubBracket
data StaffLayout = Staff Clef | Staves BracketType [StaffLayout]

pianoStaff :: StaffLayout
pianoStaff = Staves Brace [Staff trebleClef, Staff bassClef]


playableRange     :: Instrument -> Ambitus Pitch
playableRange = fromMaybe (error "Missing comfortableRange for instrument") . _playableRange . fetchInstrumentDef

comfortableRange  :: Instrument -> Ambitus Pitch
comfortableRange = fromMaybe (error "Missing comfortableRange for instrument") . _comfortableRange . fetchInstrumentDef

-- playableDynamics :: Instrument -> Pitch -> Dynamics
-- playableDynamics = error "No playableDynamics"

-- instrumentName              :: Instrument -> String
-- instrumentName = error "No name"

-- | Full instrument name.
fullName          :: Instrument -> Maybe String
-- for now use _sibeliusName if present
fullName x = _sibeliusName (fetchInstrumentDef x) `first` _longName (fetchInstrumentDef x)
  where
    first (Just x) _ = Just x
    first _ (Just x) = Just x
    first Nothing Nothing = Nothing

-- | Instrument name abbrevation.
shortName         :: Instrument -> Maybe String
shortName = _shortName . fetchInstrumentDef

-- sounding .-. written, i.e. -P5 for horn
-- | Transposition interval.
transposition :: Instrument -> Interval
transposition = _transposition . fetchInstrumentDef
  where

-- | A string representing transposition such as "Bb" or "F".
transpositionString :: Instrument -> String
transpositionString x = pitchToPCString (c .+^ transposition x)
pitch class sounding when c is notated (i.e. F for Horn in F)




-- TODO move
pitchToPCString :: Pitch -> String
pitchToPCString x = show (name x) ++ showA (accidental x)
  where
    showA 1    = "#"
    showA 0    = ""
    showA (-1) = "b"



-- internal
fetchInstrumentDef :: Instrument -> InstrumentDef
fetchInstrumentDef (StdInstrument x)   = fromMaybe (error "Bad instr") $ Data.getInstrumentDefByGeneralMidiProgram (x + 1)
fetchInstrumentDef (OtherInstrument x) = fromMaybe (error "Bad instr") $ Data.getInstrumentDefById x





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
  

