
module Music.Parts.Instrument.Woodwind (
        WoodwindInstrument,
        woodwindInstrument,
        
        GlissandoRange,
        Glissando,
        allowedGlissandi,
        effectiveGlissandi,
        isAllowedGlissando,        
  ) where

import Control.Lens
import Music.Parts.Instrument
import Data.Set (Set)
import Music.Pitch.Common (Pitch, Interval)
import Music.Pitch (Ambitus, Clef)

newtype WoodwindInstrument = WoodwindInstrument Instrument

woodwindInstrument :: Prism' Instrument WoodwindInstrument
woodwindInstrument = undefined
-- TODO

type GlissandoRange = Ambitus Pitch
type Glissando      = Ambitus Pitch

allowedGlissandi   :: Set GlissandoRange
allowedGlissandi = error "No allowedGlissandi"

effectiveGlissandi :: Set GlissandoRange
effectiveGlissandi = error "No effectiveGlissandi"


isAllowedGlissando :: WoodwindInstrument -> Glissando -> Bool
isAllowedGlissando = error "No isAllowedGlissandi"

-- TODO correspondance of range/sound/dynamic
