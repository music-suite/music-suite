Tremolo
Ties
Text
Slide
Harmonics
Color

Pitch
Dynamics
Articulation
Part


data ManyTree b a = Many b [MultiTree b a] | One a
data BracketType = Bracket | Sub | Brace

type GScore                  = (Globals, Staves)
type Staves                  = ManyTree (Maybe BracketType, Maybe String) Staff

type Globals                 = [(number=BarNumber, Maybe TimeSignature, Maybe KeySignature, Maybe RehearsalMark)]
type MultiStaffInstruments   = [Staff]
type Staff                   = (longName=String,shortName=String,musicXMLpartId=String,[Bar])

type Bar                     = [Rhythm Chord] -- layers(par), rhythms(seq tree), chords(par)
{-
Things that attach to chords
  Articulation marks, lyrics, technique, expression, instrument change text (like pizz.)

Everything that is not taking up time is positioned in a separate sub-layer?
  I.e. dynamic marks start/stops, caesuras, slur lines.
  Mind that most backends will treat these as start/stop/continue marks (i.e. no separate layer!)
-}
type Chord                   = [Pitch] -- TODO articularion marks etc, see above


type BarNumber -- numbers, letters etc as in sibelius