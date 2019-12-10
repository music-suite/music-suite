
import Music.Prelude
import Music.Score.Export2.StandardNotation (Asp, exportLilypond)

example :: Asp -- TODO make 'Prelude.Music' work
example = times 50 $ (c |> d |> e) |* (1/4)

main =
  -- print $ length $ show example
  exportLilypond example
