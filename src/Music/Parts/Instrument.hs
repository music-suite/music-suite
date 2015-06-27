
{-| Representation of musical instruments.

The 'Instrument' type represent any instrument in the MusicXML Standard Sounds 3.0 set,
with some extensions. See <http://www.musicxml.com/for-developers/standard-sounds>.
-}
module Music.Parts.Instrument (
        Instrument,
        
        -- * Name
        -- instrumentName,
        fullName,
        shortName,
        fromMidiProgram,
        toMidiProgram,
        fromMusicXmlSoundId,
        toMusicXmlSoundId,

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
import qualified Data.Aeson
import Data.Aeson (ToJSON(..))
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

instance Show Instrument where
    show x = fromMaybe "(unknown)" $ fullName x

-- TODO remove this instrance
instance Enum Instrument where
    toEnum = StdInstrument
    fromEnum (StdInstrument x) = x
    fromEnum (OtherInstrument _) = error "Instrument.fromEnum used on unknown instrument"

instance Eq Instrument where
  x == y = soundId x == soundId y

instance Ord Instrument where
  compare x y = compare (scoreOrder x) (scoreOrder y)

-- | This instance is quite arbitrary but very handy.
instance Default Instrument where
    def = StdInstrument 0

instance ToJSON Instrument where
  toJSON (StdInstrument x) = Data.Aeson.object [("midi-instrument", toJSON x)]
  toJSON (OtherInstrument x) = Data.Aeson.object [("instrument-id", toJSON x)]


-- | Create an instrument from a MIDI program number.
-- Given number should be in the range 0 - 127.
fromMidiProgram :: Int -> Instrument
fromMidiProgram = StdInstrument

-- | Convert an instrument to a MIDI program number.
-- If the given instrument is not representable as a MIDI program, return @Nothing@.
toMidiProgram :: Instrument -> Maybe Int
toMidiProgram = fmap pred . listToMaybe . _generalMidiProgram . fetchInstrumentDef

-- | Create an instrument from a MusicXML Standard Sound ID.
fromMusicXmlSoundId :: String -> Instrument
fromMusicXmlSoundId = OtherInstrument

-- | Convert an instrument to a MusicXML Standard Sound ID.
-- If the given instrument is not in the MusicXMl standard, return @Nothing@.
toMusicXmlSoundId :: Instrument -> Maybe String
toMusicXmlSoundId = Just . soundId
-- TODO filter everything with .x. in them

soundId :: Instrument -> String
soundId = _soundId . fetchInstrumentDef

-- | Clefs allowed for this instrument.
allowedClefs      :: Instrument -> Set Clef
allowedClefs = Data.Set.fromList . _allowedClefs . fetchInstrumentDef

-- | Standard clef used for this instrument.
standardClef      :: Instrument -> Maybe Clef
standardClef = listToMaybe . _standardClef . fetchInstrumentDef
-- TODO what about multi-staves?

data BracketType = Bracket | Brace | SubBracket
data StaffLayout = Staff Clef | Staves BracketType [StaffLayout]

pianoStaff :: StaffLayout
pianoStaff = Staves Brace [Staff trebleClef, Staff bassClef]

-- | Playable range for this instrument.
playableRange     :: Instrument -> Ambitus Pitch
playableRange = fromMaybe (error "Missing comfortableRange for instrument") . _playableRange . fetchInstrumentDef

-- | Comfortable range for this instrument.
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
-- pitch class sounding when c is notated (i.e. F for Horn in F)




-- TODO move
pitchToPCString :: Pitch -> String
pitchToPCString x = show (name x) ++ showA (accidental x)
  where
    showA 1    = "#"
    showA 0    = ""
    showA (-1) = "b"


scoreOrder :: Instrument -> Double
scoreOrder = _scoreOrder . fetchInstrumentDef

-- internal
fetchInstrumentDef :: Instrument -> InstrumentDef
fetchInstrumentDef (StdInstrument x)   = fromMaybe (error "Bad instr") $ Data.getInstrumentDefByGeneralMidiProgram (x + 1)
fetchInstrumentDef (OtherInstrument x) = fromMaybe (error "Bad instr") $ Data.getInstrumentDefById x





-- Legacy
-- TODO remove

gmClef :: Int -> Int
gmMidiChannel :: Int -> Int
gmScoreOrder :: Int -> Double
gmInstrName :: Int -> Maybe String


gmClef x = fromMaybe 0 $ fmap (go . _standardClef) $ Data.getInstrumentDefByGeneralMidiProgram (x + 1)
  where
    go cs | head cs == trebleClef = 0
          | head cs == altoClef   = 1
          | head cs == bassClef   = 2
          | otherwise = error "gmClef: Unknown clef"

gmScoreOrder x = fromMaybe 0 $ fmap (_scoreOrder) $ Data.getInstrumentDefByGeneralMidiProgram (x + 1)
gmMidiChannel x = fromMaybe 0 $ (=<<) (_defaultMidiChannel) $ Data.getInstrumentDefByGeneralMidiProgram (x + 1)
gmInstrName x = (=<<) (_longName) $ Data.getInstrumentDefByGeneralMidiProgram (x + 1)
  

