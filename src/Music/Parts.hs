
module Music.Parts -- ()
where

-- |
-- A division represents a subset of a finite group of performers.
--
-- For example a group may be divided into three equal divisions,
-- designated @(0, 3)@, @(1, 3)@ and @(2, 3)@ respectively.
--
newtype Division = Division { getDivision :: (Int, Int) }

-- |
-- A subpart is a potentially infinite sequence of divisions, each typically
-- designated using a new index type, i.e. @I.1.2@.
-- 
type Subpart = [Division]


data Section
    = Woodwind
    | Brass
    | Percussion
    | Keyboard
    | Voices
    | Strings

data VoicePart
    = Soprano
    | MezzoSoprano
    | Alto
    | Tenor
    | Baritone
    | Bass


data GMInstrumentType
    = GMPiano
    | GMChromaticPercussion
    | GMOrgan
    | GMGuitar
    | GMBass
    | GMStrings
    | GMEnsemble
    | GMBrass
    | GMReed
    | GMPipe
    | GMSynthLead
    | GMSynthPad
    | GMSynthEffects
    | GMEthnic
    | GMPercussive
    | GMSoundEffects


    
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
        scoreToVoice :: Score a -> Voice (Maybe a)
    
    A polyphonic part contains several voices, i.e.

        scoreToVoices :: Score a -> [Voice (Maybe a)]  


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
        | Sop
        | Vl1
        | Vl2
        | Pno

-}