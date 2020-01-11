
Times redesign
    Separate types/TimeDur/Span for POLA
    Stop using Span and transformation as synonyms (?)
    Clean up split and reverse defs
    Stop claiming that mconcat is pcat
        - pcat should look at eras and put them together just like pseq
        - Many other ways of laying out eras, could be a sub-language
        - Maybe use other mconcat synonym (like "atop")
        - This confusion comes from using Hudak-style names (seq vs par composition – UNALIGNED)
          with Diagrams-style semantics
    Get rid of "rest"?
    Clean Note/Voice/Score API
    Remove Placed/Track (or check semantics)?
    Remove Future/Past
    Remove Segment?