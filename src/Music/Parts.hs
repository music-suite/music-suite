{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Representation of musical instruments, parts and playing techniques.
module Music.Parts
  ( -- * Subparts
    module Music.Parts.Division,
    module Music.Parts.Subpart,

    -- * Solo vs. tutti
    module Music.Parts.Solo,

    -- * Instruments
    module Music.Parts.Instrument,
    module Music.Parts.Instrument.Brass,
    module Music.Parts.Instrument.Keyboard,
    module Music.Parts.Instrument.Percussion,
    module Music.Parts.Instrument.Strings,
    module Music.Parts.Instrument.Vocal,
    module Music.Parts.Instrument.Woodwind,

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
    cTrumpet,
    dTrumpet,
    bassTrumpet,
    altoTrombone,
    tenorTrombone,
    trombone,
    bassTrombone,
    tuba,
    timpani,
    snareDrum,
    kickDrum,
    crashCymbal,
    rideCymbal,
    clashCymbal,
    orchestralCymbal,
    piatti,
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

    -- ** Parts
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
    claves,
    maracas,

    -- ** Ensembles (TODO move to separate module)
    stringOrchestra,
  )
where

import Music.Parts.Division
import Music.Parts.Group
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

piccoloFlute :: Instrument
piccoloFlute = fromMidiProgram 72

flute :: Instrument
flute = fromMidiProgram 73

altoFlute :: Instrument
altoFlute = fromMusicXmlSoundId "wind.flutes.flute.alto"

bassFlute :: Instrument
bassFlute = fromMusicXmlSoundId "wind.flutes.flute.bass"

oboe :: Instrument
oboe = fromMidiProgram 68

corAnglais :: Instrument
corAnglais = fromMidiProgram 69

heckelphone :: Instrument
heckelphone = fromMusicXmlSoundId "wind.reed.oboes.heckelphone"

ebClarinet :: Instrument
ebClarinet = fromMusicXmlSoundId "wind.reed.clarinet.eflat"

clarinet :: Instrument
clarinet = fromMidiProgram 71

aClarinet :: Instrument
aClarinet = fromMusicXmlSoundId "wind.reed.clarinet.a"

bassClarinet :: Instrument
bassClarinet = fromMusicXmlSoundId "wind.reed.clarinet.bass"

sopranoSax :: Instrument
sopranoSax = fromMidiProgram 64

altoSax :: Instrument
altoSax = fromMidiProgram 65

tenorSax :: Instrument
tenorSax = fromMidiProgram 66

baritoneSax :: Instrument
baritoneSax = fromMidiProgram 67

bassoon :: Instrument
bassoon = fromMidiProgram 70

contraBassoon :: Instrument
contraBassoon = fromMusicXmlSoundId "wind.reed.contrabassoon"

horn :: Instrument
horn = fromMidiProgram 60

piccoloTrumpet :: Instrument
piccoloTrumpet = fromMusicXmlSoundId "brass.trumpet.piccolo"

trumpet :: Instrument
trumpet = fromMidiProgram 56

cTrumpet :: Instrument
cTrumpet = fromMusicXmlSoundId "brass.trumpet.c"

dTrumpet :: Instrument
dTrumpet = fromMusicXmlSoundId "brass.trumpet.d"

bassTrumpet :: Instrument
bassTrumpet = fromMusicXmlSoundId "brass.trumpet.bass"

altoTrombone :: Instrument
altoTrombone = fromMusicXmlSoundId "brass.trombone.alto"

trombone :: Instrument
trombone = tenorTrombone

tenorTrombone :: Instrument
tenorTrombone = fromMidiProgram 57

bassTrombone :: Instrument
bassTrombone = fromMusicXmlSoundId "brass.trombone.bass"

tuba :: Instrument
tuba = fromMidiProgram 58

timpani :: Instrument
timpani = fromMidiProgram 47

snareDrum :: Part
snareDrum = tutti $ fromMusicXmlSoundId "drum.snare-drum"

kickDrum :: Part
kickDrum = tutti $ fromMusicXmlSoundId "drum.kick-drum"

baseDrum :: Part
baseDrum = tutti $ fromMusicXmlSoundId "drum.bass-drum"

crashCymbal :: Part
crashCymbal = tutti $ fromMusicXmlSoundId "cymbal.crash"

suspendedCymbal :: Part
suspendedCymbal = crashCymbal

rideCymbal :: Part
rideCymbal = tutti $ fromMusicXmlSoundId "cymbal.ride"

clashCymbal :: Part
clashCymbal = tutti $ fromMusicXmlSoundId "cymbal.clash"

orchestralCymbal :: Part
orchestralCymbal = clashCymbal

piatti :: Part
piatti = orchestralCymbal

piano :: Instrument
piano = fromMidiProgram 0

celesta :: Instrument
celesta = fromMidiProgram 8

glockenspiel :: Instrument
glockenspiel = fromMidiProgram 9

vibraphone :: Instrument
vibraphone = fromMidiProgram 11

marimba :: Instrument
marimba = fromMidiProgram 12

xylophone :: Instrument
xylophone = fromMidiProgram 13

xylorimba :: Instrument
xylorimba = fromMusicXmlSoundId "pitched-percussion.xylorimba"

claves :: Part
claves = tutti $ fromMusicXmlSoundId "wood.claves"

maracas :: Part
maracas = tutti $ fromMusicXmlSoundId "rattle.maraca"

tubularBells :: Instrument
tubularBells = fromMidiProgram 14

dulcimer :: Instrument
dulcimer = fromMidiProgram 15

accordion :: Instrument
accordion = fromMidiProgram 21

harmonica :: Instrument
harmonica = fromMidiProgram 22

violin :: Instrument
violin = fromMidiProgram 40

viola :: Instrument
viola = fromMidiProgram 41

cello :: Instrument
cello = fromMidiProgram 42

doubleBass :: Instrument
doubleBass = fromMidiProgram 43

piccoloFlutes :: Part
piccoloFlutes = tutti piccoloFlute

flutes :: Part
flutes = tutti flute

oboes = tutti oboe

clarinets = tutti clarinet

bassoons = tutti bassoon

(flutes1, flutes2) = divide2 flutes

altoFlutes = tutti altoFlute

(oboes1, oboes2) = divide2 oboes

corAnglaises = tutti corAnglais

(clarinets1, clarinets2) = divide2 clarinets

ebClarinets = tutti ebClarinet

bassClarinets = tutti bassClarinet

contraBassoons = tutti contraBassoon

horns = tutti horn

highHorns = zipWith (!!) (repeat $ divide 4 horns) [0, 2]

lowHorns = zipWith (!!) (repeat $ divide 4 horns) [1, 3]

trumpets = tutti trumpet

trombones = tutti trombone

(trumpets1, trumpets2) = divide2 trumpets

(trombones1, trombones2) = divide2 trombones

tubas = tutti tuba

violins = tutti violin

(violins1, violins2) = divide2 violins

violas = tutti viola

cellos = tutti cello

doubleBasses = tutti doubleBass

harp' = fromMidiProgram 46

harp = tutti harp'

stringOrchestra = divide 2 violins ++ [violas, cellos] -- TODO define somewhere

divide2 x = case divide 2 x of
  [a, b] -> (a, b)
  _ -> error "Expected 2"
