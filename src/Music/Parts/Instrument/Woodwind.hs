
module Music.Parts.Instrument.Woodwind (
        WoodwindInstrument,
        woodwindInstrument,
        isWoodwindInstrument,

        GlissandoRange,
        Glissando,
        allowedGlissandi,
        effectiveGlissandi,
        isAllowedGlissando,
  ) where

import Control.Lens
import Control.Monad.Plus (partial)
import Data.List (isPrefixOf)
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype WoodwindInstrument = WoodwindInstrument { getWoodwindInstrument :: Instrument }

woodwindInstrument :: Prism' Instrument WoodwindInstrument
woodwindInstrument = prism' getWoodwindInstrument (fmap WoodwindInstrument . partial isWoodwindInstrument)

v :: Instrument -> Bool
isWoodwindInstrument x = case toMusicXmlSoundId x of
  Nothing -> False
  Just i  -> Data.List.isPrefixOf "wind" i


type GlissandoRange = Ambitus Pitch
type Glissando      = Ambitus Pitch

allowedGlissandi   :: Set GlissandoRange
allowedGlissandi = error "No allowedGlissandi"

effectiveGlissandi :: Set GlissandoRange
effectiveGlissandi = error "No effectiveGlissandi"


isAllowedGlissando :: WoodwindInstrument -> Glissando -> Bool
isAllowedGlissando = error "No isAllowedGlissandi"

-- TODO correspondance of range/sound/dynamic
