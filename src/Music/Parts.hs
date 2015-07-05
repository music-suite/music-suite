{-# LANGUAGE FlexibleContexts #-}

-- | Representation of musical instruments, parts and playing techniques.
module Music.Parts (
        -- * Terminology
        -- $terminology


        -- * Subparts
        module Music.Parts.Division,
        module Music.Parts.Subpart,

        -- * Solo vs. tutti
        module Music.Parts.Solo,
        
        -- * Instruments
        module Music.Parts.Instrument,

        -- * Parts
        Part(..),
        _solo,
        _subpart,
        _instrument,
        divide,
        containsPart,
        smallestPart,
        smallestSubpart,
        largestPart,
        largestSubpart,
        distinctFrom,
        allDistinct,
        solo,
        tutti,

        -- ** Instruments etc
        piccoloFlute,
        flute,
        altoFlute,
        bassFlute,

        oboe,
        corAnglais,
        heckelphone,

        ebClarinet,
        clarinet,
        aClarinet,
        bassClarinet,

        sopranoSax,
        altoSax,
        tenorSax,
        baritoneSax,

        bassoon,
        contraBassoon,

        horn,
        piccoloTrumpet,
        trumpet,
        bassTrumpet,
        altoTrombone,
        tenorTrombone,
        trombone,
        bassTrombone,
        tuba,

        timpani,
        piano,

        celesta,
        glockenspiel,
        vibraphone,
        marimba,
        xylophone,
        xylorimba,
        tubularBells,
        dulcimer,

        accordion,
        harmonica,

        violin,
        viola,
        cello,
        doubleBass,

        -- ** Ensembles
        piccoloFlutes,
        flutes,
        altoFlutes,
        oboes,
        corAnglaises,
        clarinets,
        ebClarinets,
        bassClarinets,
        bassoons,
        contraBassoons,

        flutes1,
        flutes2,
        oboes1,
        oboes2,
        clarinets1,
        clarinets2,

        horns,
        highHorns,
        lowHorns,
        trumpets,
        trombones,
        trumpets1,
        trumpets2,
        trombones1,
        trombones2,
        tubas,

        violins,
        violins1,
        violins2,
        violas,
        cellos,
        doubleBasses,

        harp,

        -- ** Default values
        defaultClef,
        defaultMidiProgram,
        defaultMidiChannel,
        defaultMidiNote,

        -- -- * Orchestration
        -- doubleParts,
        -- doublePartsInOctave,

        -- * Basic
        module Music.Parts.Basic

  ) where

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
import           Music.Parts.Subpart
import           Music.Parts.Division
import           Music.Parts.Solo
import           Music.Parts.Instrument

{- $terminology

Parts represent a subset of a group of performers. It is mainly used for instrumental and
vocal music, but some concetps may be useful in electronic music as well.

-   'Section' refers to a set of instrumentfamilies related by sound production method (i.e. woodwind).

-   'Family' refers to a set of instrument or voice types, which typically differ in size (i.e. saxophones).

-   'Instrument' refers to a set of instruments or voice types of a given type (i.e. soprano saxophones).
    Perhaps confusingly, this includes vocal types such as alto, tenor etc as well. However, there is
    no good general term that incorporate both /instrument/ and /voice type/.

-   A 'Part' is made up of an 'Instrument' and a 'Division' (i.e. Violin I). Solo parts are treated
    separately, so i.e. /Violin solo II/ (as in a double concerto) is distinct from /Violin II/.

-}

-- | A part is a subdivided group of instruments of a given type.
data Part = Part Solo Instrument Subpart
    deriving (Eq, Ord)

instance Show Part where
    show (Part Solo instr subp) = "Solo " ++ show instr ++ addS (show subp)
        where
            addS "" = ""
            addS x = " " ++ x
    show (Part _ instr subp)    = show instr ++ addS (show subp)
        where
            addS "" = ""
            addS x = " " ++ x

-- Bad instance (?)
instance Enum Part where
    toEnum x = Part Tutti (toEnum x) def
    fromEnum (Part solo instr subp) = fromEnum instr

-- Semantics: Monoid (Option . First)
instance Monoid Part where
  mempty = def
  mappend x y
    | x == mempty = y
    | otherwise   = x 

instance Semigroup Part where
  (<>) = mappend

instance Default Part where
  def = Part def def def

instance ToJSON Part where
  toJSON p = Data.Aeson.object [
    ("instrument", toJSON $ p^._instrument),
    ("subpart",    toJSON $ p^._subpart),
    ("solo",       toJSON $ p^._solo)
    ]

instance FromJSON Part where
  parseJSON (Data.Aeson.Object v) = do
    s <- v Data.Aeson..: "solo"
    i <- v Data.Aeson..: "instrument"
    u <- v Data.Aeson..: "subpart"
    return $ Part s i u
  parseJSON _ = empty

-- |
-- @a \`containsPart\` b@ holds if the set of players represented by a is an improper subset of the
-- set of players represented by b.
containsPart :: Part -> Part -> Bool
Part solo1 instr1 subp1 `containsPart` Part solo2 instr2 subp2 =
        solo1 == solo2
        && instr1 == instr2
        && subp1 `containsSubpart` subp2

smallestPart :: Part -> Part -> Part
smallestPart p1@(Part _ _ sp1) p2@(Part _ _ sp2)
  | sp1 `smallestSubpart` sp2 == sp1 = p1
  | sp1 `smallestSubpart` sp2 == sp2 = p2

smallestSubpart :: Subpart -> Subpart -> Subpart
smallestSubpart x y
  | x `isProperSubpartOf` y = x
  | y `isProperSubpartOf` x = y
  -- arbitrarily:
  | otherwise               = x

largestPart :: Part -> Part -> Part
largestPart p1@(Part _ _ sp1) p2@(Part _ _ sp2)
  | sp1 `largestSubpart` sp2 == sp1 = p1
  | sp1 `largestSubpart` sp2 == sp2 = p2

largestSubpart :: Subpart -> Subpart -> Subpart
largestSubpart x y
  | x `isProperSubpartOf` y = y
  | y `isProperSubpartOf` x = x
  -- arbitrarily:
  | otherwise               = x


-- | Returns 'True' iff all given parts are distinct (as per 'distinctFrom').
allDistinct :: [Part] -> Bool
allDistinct []     = True
allDistinct (x:xs) = all (distinctFrom x) xs && allDistinct xs

-- | Returns 'True' iff x and y are completely distinct, i.e. neither contains the other.
-- 
-- >>> violins `distinctFrom` trumpets
-- True
-- >>> violins `distinctFrom` violins
-- False
-- >>> violins `distinctFrom` violins1
-- False
-- >>> violins1 `distinctFrom` violins
-- False
-- >>> violins1 `distinctFrom` violins2
-- True
--
distinctFrom :: Part -> Part -> Bool
distinctFrom (Part s1 i1 sp1) (Part s2 i2 sp2) = s1 /= s2 || i1 /= i2 || noneSubpart
  where
    -- Is this needed?
    noneSubpart = not (sp1 `isSubpartOf` sp2) && not (sp2 `isSubpartOf` sp1)


    
    -- if equal
    -- [pa',pb'] = divide 2 pa

_solo :: Lens' Part Solo
_solo f (Part s i u) = fmap (\s -> Part s i u) $ f s

_subpart :: Lens' Part Subpart
_subpart f (Part s i u) = fmap (\u -> Part s i u) $ f u

_instrument :: Lens' Part Instrument
_instrument f (Part s i u) = fmap (\i -> Part s i u) $ f i

-- | Divide a part into @n@ subparts.
divide :: Int -> Part -> [Part]
divide n (Part solo instr subp) = fmap (\x -> Part solo instr (subp <> Subpart [x])) $ divisions n

solo instr      = Part Solo instr def
tutti instr     = Part Tutti instr def


piccoloFlute    = fromMidiProgram 72
flute           = fromMidiProgram 73
altoFlute       = fromMusicXmlSoundId "wind.flutes.flute.alto"
bassFlute       = fromMusicXmlSoundId "wind.flutes.flute.bass"

oboe            = fromMidiProgram 68
corAnglais      = fromMidiProgram 69
heckelphone     = fromMusicXmlSoundId "wind.reed.oboes.heckelphone"

ebClarinet      = fromMusicXmlSoundId "wind.reed.clarinet.eflat"
clarinet        = fromMidiProgram 71
aClarinet       = fromMusicXmlSoundId "wind.reed.clarinet.a"
bassClarinet    = fromMusicXmlSoundId "wind.reed.clarinet.bass"

sopranoSax      = fromMidiProgram 64
altoSax         = fromMidiProgram 65
tenorSax        = fromMidiProgram 66
baritoneSax     = fromMidiProgram 67

bassoon         = fromMidiProgram 70
contraBassoon   = fromMusicXmlSoundId "wind.reed.contrabassoon"

horn            = fromMidiProgram 60
piccoloTrumpet  = fromMusicXmlSoundId "brass.trumpet.piccolo"
trumpet         = fromMidiProgram 56
bassTrumpet     = fromMusicXmlSoundId "brass.trumpet.bass"
altoTrombone    = fromMusicXmlSoundId "brass.trombone.alto"
trombone        = tenorTrombone
tenorTrombone   = fromMidiProgram 57
bassTrombone    = fromMusicXmlSoundId "brass.trombone.bass"
tuba            = fromMidiProgram 58

timpani         = fromMidiProgram 47
piano           = fromMidiProgram 0

celesta         = fromMidiProgram 8
glockenspiel    = fromMidiProgram 9
vibraphone      = fromMidiProgram 11
marimba         = fromMidiProgram 12
xylophone       = fromMidiProgram 13
xylorimba       = fromMusicXmlSoundId "pitched-percussion.xylorimba"
tubularBells    = fromMidiProgram 14
dulcimer        = fromMidiProgram 15

accordion       = fromMidiProgram 21
harmonica       = fromMidiProgram 22

violin          = fromMidiProgram 40
viola           = fromMidiProgram 41
cello           = fromMidiProgram 42
doubleBass      = fromMidiProgram 43



defaultMidiProgram :: Part -> Int
defaultMidiProgram (Part _ instr _) = fromMaybe 0 $ toMidiProgram instr

defaultMidiNote :: Part -> Int
defaultMidiNote _ = 0

defaultMidiChannel :: Part -> Int
defaultMidiChannel = gmMidiChannel . defaultMidiProgram

defaultScoreOrder :: Part -> Double
defaultScoreOrder = gmScoreOrder . defaultMidiProgram

defaultClef :: Part -> Int
defaultClef = gmClef . defaultMidiProgram
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

piccoloFlutes = tutti piccoloFlute
flutes = tutti flute
oboes = tutti oboe
clarinets = tutti clarinet
bassoons = tutti bassoon

[flutes1, flutes2] = divide 2 flutes
altoFlutes     = tutti altoFlute
[oboes1, oboes2] = divide 2 oboes
corAnglaises = tutti corAnglais
[clarinets1, clarinets2] = divide 2 clarinets
ebClarinets    = tutti ebClarinet
bassClarinets  = tutti bassClarinet
contraBassoons = tutti contraBassoon


horns = tutti horn
highHorns = zipWith (!!) (repeat $ divide 4 horns) [0,2]
lowHorns = zipWith (!!) (repeat $ divide 4 horns) [1,3]
trumpets = tutti trumpet
trombones = tutti trombone
[trumpets1, trumpets2] = divide 2 trumpets
[trombones1, trombones2] = divide 2 trombones
tubas = tutti tuba

violins = tutti violin
[violins1, violins2] = divide 2 violins
violas = tutti viola
cellos = tutti cello
doubleBasses = tutti doubleBass

harp' = fromMidiProgram 46
harp = tutti harp'

