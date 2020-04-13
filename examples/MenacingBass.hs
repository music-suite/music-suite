
-- |
-- This is the "Hello, world!" of Music Suite.
--
-- The 'import' statement allow us to use Music Suite in this Haskell file.
import Music.Prelude

-- | Here we define a simple piece of music.
--
-- The first line "music :: Music" is a type hint, saying that 'music'
-- is a piece of music.
--
-- The notes c, d and e are composed sequentially using the operator |>.
--
-- The stretch operator |* is used to stretch all of the notes to 1/4
-- (e.g. crotchets/quarter notes).
--
-- The 'times' function repeats the 3-note figure 5 times.




-- This line creates a standard Haskell main function that exports the piece
-- 'music'. This means we can run this file as a standard Haskell program,
-- or even compile it into a standalone program.
main = defaultMain music


-- c,d,e,f,g,a,b
music :: Music
music = x1 |> x1 |> stretch 0.5 x1

x1 =
    mempty
    </> foo [f,eb,d,eb]
    </> stretch 4 (set parts' violins c'')
    </> level pp (set parts' doubleBasses (times 4 (compress 2 rest |> compress 2 (_8vb g__))))

-- ppar = mconcat = foldr mempty (<>)
-- pseq = foldr mempty (|>)

foo =
  pseq . fmap (\x -> ppar [c, x, g])
