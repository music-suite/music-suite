{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-} -- TODO get rid of this!
{-# LANGUAGE FlexibleContexts #-}

-- | Representation of musical instruments, parts and playing techniques.
module Music.Parts
  (
    -- * Subparts
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

piccoloFlute = fromMidiProgram 72

flute = fromMidiProgram 73

altoFlute = fromMusicXmlSoundId "wind.flutes.flute.alto"

bassFlute = fromMusicXmlSoundId "wind.flutes.flute.bass"

oboe = fromMidiProgram 68

corAnglais = fromMidiProgram 69

heckelphone = fromMusicXmlSoundId "wind.reed.oboes.heckelphone"

ebClarinet = fromMusicXmlSoundId "wind.reed.clarinet.eflat"

clarinet = fromMidiProgram 71

aClarinet = fromMusicXmlSoundId "wind.reed.clarinet.a"

bassClarinet = fromMusicXmlSoundId "wind.reed.clarinet.bass"

sopranoSax = fromMidiProgram 64

altoSax = fromMidiProgram 65

tenorSax = fromMidiProgram 66

baritoneSax = fromMidiProgram 67

bassoon = fromMidiProgram 70

contraBassoon = fromMusicXmlSoundId "wind.reed.contrabassoon"

horn = fromMidiProgram 60

piccoloTrumpet = fromMusicXmlSoundId "brass.trumpet.piccolo"

trumpet = fromMidiProgram 56

cTrumpet = fromMusicXmlSoundId "brass.trumpet.c"

dTrumpet = fromMusicXmlSoundId "brass.trumpet.d"

bassTrumpet = fromMusicXmlSoundId "brass.trumpet.bass"

altoTrombone = fromMusicXmlSoundId "brass.trombone.alto"

trombone = tenorTrombone

tenorTrombone = fromMidiProgram 57

bassTrombone = fromMusicXmlSoundId "brass.trombone.bass"

tuba = fromMidiProgram 58

timpani = fromMidiProgram 47

snareDrum = tutti $ fromMusicXmlSoundId "drum.snare-drum"

kickDrum = tutti $ fromMusicXmlSoundId "drum.kick-drum"

baseDrum = tutti $ fromMusicXmlSoundId "drum.bass-drum"

crashCymbal = tutti $ fromMusicXmlSoundId "cymbal.crash"

suspendedCymbal = crashCymbal

rideCymbal = tutti $ fromMusicXmlSoundId "cymbal.ride"

clashCymbal = tutti $ fromMusicXmlSoundId "cymbal.clash"

orchestralCymbal = clashCymbal

piatti = orchestralCymbal

piano = fromMidiProgram 0

celesta = fromMidiProgram 8

glockenspiel = fromMidiProgram 9

vibraphone = fromMidiProgram 11

marimba = fromMidiProgram 12

xylophone = fromMidiProgram 13

xylorimba = fromMusicXmlSoundId "pitched-percussion.xylorimba"

claves = tutti $ fromMusicXmlSoundId "wood.claves"

maracas = tutti $ fromMusicXmlSoundId "rattle.maraca"

tubularBells = fromMidiProgram 14

dulcimer = fromMidiProgram 15

accordion = fromMidiProgram 21

harmonica = fromMidiProgram 22

violin = fromMidiProgram 40

viola = fromMidiProgram 41

cello = fromMidiProgram 42

doubleBass = fromMidiProgram 43

piccoloFlutes = tutti piccoloFlute

flutes = tutti flute

oboes = tutti oboe

clarinets = tutti clarinet

bassoons = tutti bassoon

[flutes1, flutes2] = divide 2 flutes

altoFlutes = tutti altoFlute

[oboes1, oboes2] = divide 2 oboes

corAnglaises = tutti corAnglais

[clarinets1, clarinets2] = divide 2 clarinets

ebClarinets = tutti ebClarinet

bassClarinets = tutti bassClarinet

contraBassoons = tutti contraBassoon

horns = tutti horn

highHorns = zipWith (!!) (repeat $ divide 4 horns) [0, 2]

lowHorns = zipWith (!!) (repeat $ divide 4 horns) [1, 3]

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

stringOrchestra = divide 2 violins ++ [violas, cellos] -- TODO define somewhere
