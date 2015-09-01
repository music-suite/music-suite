
module Music.Parts.Group where

import           Control.Applicative
import           Control.Lens                    (toListOf, Lens, Lens', (^.))
import           Data.Aeson                      (ToJSON (..), FromJSON(..))
import qualified Data.Aeson
import           Data.Default
-- import           Data.Monoid
-- import           Control.Lens (set)
import           Data.Functor.Adjunction         (unzipR)
import qualified Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Option.Instances
import           Data.Traversable                (traverse)
import           Data.Typeable
import           Text.Numeral.Roman              (toRoman)

import           Music.Parts.Basic
import           Music.Parts.Division
import           Music.Parts.Solo
import           Music.Parts.Instrument
import           Music.Parts.Part
import           Music.Parts.Subpart

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

-- data GroupType
  -- = Bracket
  -- | 
