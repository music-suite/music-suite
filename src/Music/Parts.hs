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
        module Music.Parts.Part,
        module Music.Parts.Group,

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

        -- * Legacy
        defaultClef,
        defaultMidiProgram,
        defaultMidiChannel,
        defaultMidiNote,

        -- * Basic
        module Music.Parts.Basic

  ) where

import           Control.Lens                    (toListOf, Lens, Lens', (^.))
import           Data.Maybe

import           Music.Parts.Basic
import           Music.Parts.Division
import           Music.Parts.Solo
import           Music.Parts.Instrument
import           Music.Parts.Part
import           Music.Parts.Subpart
import           Music.Parts.Group

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
defaultMidiProgram p = fromMaybe 0 $ toMidiProgram (p^._instrument)

defaultMidiNote :: Part -> Int
defaultMidiNote _ = 0

defaultMidiChannel :: Part -> Int
defaultMidiChannel = gmMidiChannel . defaultMidiProgram

defaultScoreOrder :: Part -> Double
defaultScoreOrder = gmScoreOrder . defaultMidiProgram

defaultClef :: Part -> Int
defaultClef = gmClef . defaultMidiProgram

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

