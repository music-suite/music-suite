import Music.Prelude ; main = defaultMain music

-- c d e f g a b
music =
  -- c <> e <> g
  -- c |> e |> g
  -- stretch 0.5 (c </> e </> g)
  timeSignature 20 $ pseq [stretch 1 c, stretch 0.5 d,e,f,g]
