
-- |
-- This is the "Hello, world!" of Music Suite.
--
-- The 'import' statement allow us to use Music Suite in this Haskell file.
import Music.Prelude

main = defaultMain music


-- c,d,e,f,g,a,b
music :: Music
music =
     x1
  |> x1
  |> stretch 0.5 x1
  |> stretch 0.5 x1

x1 =
    mempty
    </> foo [f,eb,d,eb]
    </> stretch 4 (set parts' violins c'')
    </> level pp (set parts' doubleBasses (times 4 (compress 2 rest |> compress 2 (_8vb g__))))

-- ppar = mconcat = foldr mempty (<>)
-- pseq = foldr mempty (|>)

foo =
  pseq . fmap (\x -> ppar [c, x, g])
