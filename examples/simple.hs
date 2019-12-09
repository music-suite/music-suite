
import Music.Prelude
import Music.Score.Export2.StandardNotation (Asp, exportLilypond)

example :: Asp -- TODO make 'Prelude.Music' work
example = (c |> d |> e) |* (1/4)

main = exportLilypond example
