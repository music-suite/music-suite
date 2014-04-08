


Score         -- Arbitrary position, transl-sens
    Note      -- Gives something a position and duration, transl-sens    
    Delayed   -- Gives something a position, transl-sens    
    
    Stretched -- Single value, transl-inv
    Voice     -- Sequential values, transl-inv
    Chord     -- Parallell values, transl-inv
    Voices    -- Full recursive voice structure, transl-inv


    
data Voices a
  = Voice (Voice a)
  | Seq [Voices a]
  | Par [Voices a]


vls = Par [
  Seq [Voice ..., Par [...], Voice ...],
  Seq [Voice ..., Par [...], Voice ...]
  ]
