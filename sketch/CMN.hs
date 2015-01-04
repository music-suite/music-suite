
-- TODO replace doube with specific type
newtype DynamicNotation
  = DynamicNotation {getDynamicNotation :: ([CrescDim],
                                            Maybe Double)}
newtype ArticulationNotation
  = ArticulationNotation {getArticulationNotation :: ([Slur],
                                                      [Mark])}
type Chord =

  TextT (
  TremoloT (
  HarmonicT (
  SlideT (
  
  PartT Part (
  ArticulationT ArticulationNotation (
  DynamicT Dynamics    
    [Maybe (ColorT (NoteheadT (TieT Pitch)))] 
  ))))))

-- TODO use Rhythm or similar
data TupletVoice a = ATuplet DurationRatio (TupletVoice a) | AVoice (Voice a)

type Layer = TupletVoice Chord'

type Measure = [Layer]
type Staff = [Measure]
type StaffGroup = [Staff]
type Music = [StaffGroup]
-- TODO outer structure (meta-data such as titles, backend-specific templates etc)

{-
Misc TODO
clefs
free-floating text
key signatures
time signatures
tempo marks
repeat signs
breath signs (commas, caesuras etc)
jump signs (segno, to coda, vide etc)
rehearsal marks, text cues etc
fermatas
chord symbols
figured bass
fingering

up/down-bow sign
stop/open sign
ornaments
cue notes
appogiatura notes

-}