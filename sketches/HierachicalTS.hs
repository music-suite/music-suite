verticalSlice :: Chord (Voice a) -> Voice (Chord a)
-- Natural transformation?

reactiveSwithPoints :: Reactive a -> [Time]
splitReactive :: Time -> Reactive a -> Reactive a
splitReactives :: [Reactive a] -> [Reactive a]


Before export to notation formats, generate a full hierarchical representation
of the (visual) score. Something like:

  Score
    StaffGroup/Staff x Bar
      BarVoice
        BeatGroup
          Beat = (HLine begin/end, Beam)
            Chord = (Tremolo, Harmonic, Articulation, Dynamic, IsGrace)
              Note = (VLine begin/end, Tie begin/end, Color, NHType, NHSize)

