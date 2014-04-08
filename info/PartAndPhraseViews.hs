
Duration
Time
Span

Segment a
Behavior a
Linear a  = (a, a)
  -- Note that (Span === Linear Time)
Spline a
  focusedOn :: Span -> Lens (Behavior a) (Segment a)



Score a
  -- single voice, starting at 0
  note :: Prism (Score a) (Note a)   
  -- single restless voice, starting at 0
  voice :: Prism (Score a) (Voice a) 

  -- restless voices starting at 0 (no overlap but possibly different length)
  voices :: Eq (Part a) => Prism (Score a) [Voice a]


-- XXX Rename Voice to Phrase, use 'Voice a' to mean [Phrase a]  

Score a       -- Arbitrary position, transl-sens
    Note a    -- Gives something a position and duration, transl-sens    
    Delayed a -- Gives something a position, transl-sens    
    
    
    Stretched a -- Single value, transl-inv
    Voice a     -- Sequential values, transl-inv
    Chord a     -- Parallell values, transl-inv
    Voices a    -- Full recursive voice structure, transl-inv
    
data Voices a
  = Voice (Voice a)
  | Seq [Voices a]
  | Par [Voices a]
vls = Par [
  Seq [Voice ..., Par [...], Voice ...],
  Seq [Voice ..., Par [...], Voice ...]
  ]



-- Supports vl, vla etc
Part a ~ Z  => Score a -> Par [Voice a]

-- Supports vl1, vl2, vla etc
Part a ~ Z2 => Score a -> Seq [Par [Voice a]]

-- Supports vl1.1, vl2.1, vla etc
Part a ~ Z3/4?? => Score a -> Seq [Par [Seq [Par [Voice a]]]]




------------------------------------------------------------------------

(Part a ~:~ Int) => Score a `Iso` [Voice (Maybe a)]
Voice (Maybe a) `Iso` [Either Duration (Voice a)]