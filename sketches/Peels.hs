
module Peels where



type = NotatedScore a
  -- Part
  (ScorePar     -- with part list etc 
    (VoiceSeq   -- with bar numbers etc
      (BarSeq   -- rhythm
        (Maybe
          -- (Part)
          -- Dynamic
          -- Articulation
          -- Text
          -- Tremolo
          -- Harmonic
          -- TODO Arpeggio
          (ChordPar 
            -- Slides
            -- Color
            -- TODO Notehead type
            -- Pitch
            -- NONLENS Tie
            a
            )))))


-- Assume we have already converted whatever to a notated score
toLilypond        :: NotatedScore a -> Lilypond
-- XXX how to handle slide/color/notehead type inside chord?
toLilypondPitch   :: [Pitch a]      -> Lilypond
toLilypondDyn     :: Dynamic a      -> Lilypond -> Lilypond
toLilypondArt     :: Articulation a -> Lilypond -> Lilypond
toLilypondColor   :: Color          -> Lilypond -> Lilypond
toLilypondArp     :: Arpeggio       -> Lilypond -> Lilypond
toLilypondHarm    :: Harmonic       -> Lilypond -> Lilypond
toLilypondText    :: Tremolo        -> Lilypond -> Lilypond
toLilypondTrem    :: Tremolo        -> Lilypond -> Lilypond
toLilypondSlide   :: Slide          -> Lilypond -> Lilypond
-- TODO tie
-- TODO clef
-- TODO staff/staffgroup
-- TODO instrument name/short name/time signature/key singnature
-- TODO tuplets

{-

To *display* a score-like value in a notation backend we need to:


- Translate to the above structure
  - Translate to ScoreVoiceChord
    - Make rests (empty chords explicit)
    - For voice (without chord):  add inner chord and outer score
    - For voice (with chord):     add outer score
    - For chord:                  add outer voice and score
- With dynamics/articulation/pitch
  - Contextualize
  - Convert the internal (linear/continous)
- With text/tremolo/harmonic/arpeggio
- With slide/color/notehead/tie
- Quantize

-}