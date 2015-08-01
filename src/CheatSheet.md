
# Music Suite – Cheat Sheet

Simplified. For the full version, see types in the API.

    c d e f g a b
    cs ds es fs gs as bs
    cb db eb fb gb ab bb

    c' d' bb' etc.
    c_ d_ bb_ etc.
    c'' d'' bb'' etc.
    c__ d__ bb__ etc.
  
    _P1 _P4 _P5 _P8 _P11 _P12 _P15 etc.
    _M2 _M3 _M6 _M7 _M9 _M10 _M13 _M14 etc.
    m2 m3 m6 m7 m9 m10 m13 m14 etc.
    _A1 _A2 _A3 _A4 _A5 _A6 _A7 _A8 etc.
    d1 d2 d3 d4 d5 d6 d7 d8 etc.

    (sharpen|flatten) <pitch>
    (augment|diminish) <interval>
  
    <interval> ^* <interval-scaling>
    <interval> ^/ <interval-scaling>
    <pitch> .+^ <interval>
    <pitch> .-^ <interval>
    <pitch> .-. <pitch>

    <duration> ^* <duration-scaling>
    <duration> ^/ <duration-scaling>
    <time-point> .+^ <duration>
    <time-point> .-^ <duration>
    <time-point> .-. <time-point>


    level <level> <music>
    crescendo <level> <level> <music>
    diminuendo <level> <level> <music>
      <level> = pppppp ppppp pppp ppp pp _p mp mf _f ff fff ffff fffff ffffff
    (louder|softer) <level> <music>
    compressor <level-threshold> <level-scale> <music>

    accent <music>
    accentAll <music>
    accentLast <music>
    marcato <music>
    marcatoAll <music>
    marcatoLast <music>
  
    legatissimo <music>
    legato <music>
    portato <music>
    separated <music>
    spiccato <music>
    staccatissimo <music>
    staccato <music>
    tenuto <music>

    set pitch' <value> <music>
    set piches' <value> <music>
    set dynamic' <value> <music>
    set dynamics' <value> <music>
    set articulation' <value> <music>
    set articulations' <value> <music>

    over pitch' <value> <music>
    over piches' <value> <music>
    over dynamic' <value> <music>
    over dynamics' <value> <music>
    over articulation' <value> <music>
    over articulations' <value> <music>
  
    rest
    removeRests <music>
  
    <dur>   *| <music>
    <music> |* <dur>
    stretch <dur> <music>

    <music> |/ <dur>
    compress <dur> <music>

    delay <dur> <music>
    undelay <dur> <music>

    transform <span> <music>

    view duration <music>
    view (onset|offset|midpoint) <music>
    view (position 0.3) <music>
    view era <music>
    set duration <time> <music>
    set (onset|offset|midpoint) <time> <music>
    set (position 0.3) <time> <music>
    set era <span> <music>
  
    (1,())^.note
    (1<->,())^.event
    [(1,())^.note]^.voice
    [(1<->,())^.event]^.score
  
    (up|down|above|below) <interval> <music>
    invertPitches <relative-pitch> <music>
    invertPitchesDiatonically <relative-pitch> <music>
    invertPitchesChromatically <relative-pitch> <music>
    (fifthsUp|fifthsDown|octavesUp|octavesDown|fifthsAbove|fifthsBelow|octavesAbove|octavesBelow) n <music>
    (_8va|_8vb) <music>
    (upDiatonic|downDiatonic|upChromatic|downChromatic) <relative-pitch> <steps> <music>
    over pitches' (relative c $ spell usingSharps) <music>

    TODO augment/dim intervals
    TODO pitch mapping
    TODO randomize order
    TODO rotate/shuffle pitch/rhythm
    TODO combine tied notes
    TODO filtering
  
    music |> music
    music <> music
    music </> music
    scat [<music>,...]
    pcat [<music>,...]
    ucat [<music>,...]

    TODO
    split
    rev
         