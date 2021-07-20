{-# OPTIONS_GHC -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

module Music.Parts.Group where

import Control.Applicative
import Control.Lens (Lens, Lens', toListOf, (^.))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson
import Data.Default
import qualified Data.List
import Data.Maybe
import Data.Semigroup
import Data.Traversable (traverse)
import Data.Typeable
import Music.Parts.Division
import Music.Parts.Instrument
import Music.Parts.Instrument.Brass
import Music.Parts.Instrument.Keyboard
import Music.Parts.Instrument.Percussion
import Music.Parts.Instrument.Strings
import Music.Parts.Instrument.Vocal
import Music.Parts.Instrument.Woodwind
import Music.Parts.Part
import Music.Parts.Solo
import Music.Parts.Subpart
import Text.Numeral.Roman (toRoman)

-- | How to draw to barlines between staff groups.
data BarLines = HideBarlines | ShowBarlines
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Staff group types.
data GroupType
  = Invisible
  | Bracket -- ly: StaffGroup,  xml: GroupSymbol=bracket
  | SubBracket -- ly: PianoStaff?, xml: GroupSymbol=line
  | PianoStaff -- ly: PianoStaff,  xml: GroupSymbol=brace
  | GrandStaff -- ly: GrandStaff,  xml: GroupSymbol=brace
  deriving (Eq, Ord, Show)

-- | A tree of values with instruments and bracketing.
data ScoreLayout a
  = Single (Instrument, a)
  | Many GroupType BarLines [ScoreLayout a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Default score layout with woodwinds at the top, strings at the bottom.
groupDefault :: [(Instrument, a)] -> ScoreLayout a
groupDefault xs =
  Many
    Invisible
    HideBarlines
    [ Many Bracket ShowBarlines $ fmap Single ww,
      Many Bracket ShowBarlines $ fmap Single br,
      Many Bracket ShowBarlines $ fmap Single pc,
      Many PianoStaff ShowBarlines $ fmap Single kb,
      Many Bracket HideBarlines $ fmap Single voc,
      Many Bracket ShowBarlines $ fmap Single str
    ]
  where
    ww = filter (isWoodwindInstrument . fst) xs
    br = filter (isBrassInstrument . fst) xs
    pc = filter (isPercussionInstrument . fst) xs
    kb = filter (isKeyboardInstrument . fst) xs
    voc = filter (isVocalist . fst) xs
    str = filter (isStringInstrument . fst) xs
