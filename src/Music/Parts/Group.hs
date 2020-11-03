{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

module Music.Parts.Group where

import Control.Applicative
import Control.Lens (Lens, Lens', (^.), toListOf)
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

{-
    ## Terminology: Voice vs Part

    A voice is a container of notes (non-overlapping)

    A part is an identifier for a set of singers/musicians AND all the notes in a score
    designated for this set of performers. Part extraction has the type

        extractParts :: HasPart a => Score a -> [Score a]

    I.e. in a score for piano and ensemble, certain notes may be *in the piano part*, i.e.
    designated for the piano. Typically, a part is monophonic or polyphonic. A monophonic
    part is a voice, i.e.

        -- | Are there overlapping notes?
        isMonophonic :: Score a -> Bool

        -- | Fails for polyphonic scores.
        s_oreToVoice :: Score a -> Voice (Maybe a)

    A polyphonic score contains several voices, i.e.

        s_oreToVoices :: Score a -> [Voice (Maybe a)]


    A part is any type a that satisfies (Ord a, Show a).
    Optionally, we may add a contraint (HasPartName a), i.e.

        class HasPartName a where
            partName :: a -> String
            partAbbr :: a -> String


    These contraints are used when printing scores (to get the order of the parts and their name).

        Vln1, Vln2 etc.

    Often we want to group parts, i.e.

        Chorus {
            Sop  { Sop1 Sop2 }
            Alto { Alto1 Alto2 }
            Ten  { Ten1 Ten 2 }
            Bass { Bass1 Bass2 }
        }
        Orchestra {
            Woodwinds { ... }
            Brass { ... }
            Perc { ... }
            Strings { ... }
        }


    isInGroup :: Group -> Part -> Bool
    partGroups :: Part -> [Group]


    partGroup :: (Part -> [Group] -> a) -> Group -> a
    tree :: (a -> [Tree a] -> b) -> Tree a -> b


    data MyPart
        = Fl
        | Sop
        | Vl1
        | Vl2
        | Pno

-}

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
      Many Invisible ShowBarlines $ fmap Single kb,
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
