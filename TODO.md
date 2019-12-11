
## TODO

**We use this file instead of an issue tracker, for now**

- [X] Replace all uses of `data` directory with quasi-quoters (ideally: fail at compile-time if
  not existing/not parsing correctly)

- Get rid of all CPP

- Port issues from the old tracker

- Never fail on overlapping events

- Make all examples compile with the new build system

- Make documentation generation compile/work with the new build system
  - Make sure doctests are run in CI
  - Use hslinks or similar (disabled in Makefile for now)

- Add more examples/tests

- Rename the core sequential combinator (to what?)

- Replace (Option :. Last) with Maybe

- Improve rcat: do not use Enum
  - Instead define something akin to (HasPosition, Transformable), or Diagram's Juxtaposable:

  ```
  data Includes :: Type -> Type -> Type where
    C :: c -> c -> Includes c
  class Disamb a where
    disamb :: Set a -> a -> a
    -- not (disamb rs x `overlapsWithMemberOf` rs)
    -- not (x `overlapsWithMemberOf` rs) -> (disamb rs x = x)
    -- where
    --   x `overlapsWithMemberOf` rs = forall (r `elem` rs) . not (x `C` r) && not (r `C` x)
  ```

  - Get rid of divisor component in Division, just mark everything as "I", "I.1" etc.
    - Trivial poset (prefix on strings)
    - How many players are allocated for each note depend on context
    - Backends may forbid/reallocate concurrent notes of overlapping parts (e.g. "Sop 1" and "Sop 1.1")
    - The `</>` operator makes sure no overlapping notes have overlapping parts. For example:
      ```
        [Tpt, Cl] Tbn -> Tbn
        [Pno I], Pno I -> Pno II
        [Vln I, Vla], Vln I.1 -> Vln II
        [Vln], Vln solo -> Vln solo -- no overlap
      ```

- Finish/document new export code ("Export2")
  - Make Export2 support:
    - Colored noteheads
    - Playing techinques: pizz, trem, harmonics, slide/gliss
    - Expression text (e.g. "dolce")
    - Lyrics
    - Free-form text
    - Meta-data (does it work?)
  - Add unit tests for all export formats/backends:
    - MIDI
    - Standard notation (common part of Lilypond/MusicXML)
    - Lilypond
    - MusicXML
  - Benchmark!

- New backends (ideas):
  - ABC notation
  - Vextab
  - csound-expression
  - scsynth
  - Graphical backends
    - Piano roll

- Decide on top-level interface
  - By default recommend *no IO*

  - All data is is in the DSL/Haskell code. For *import formats* (such as Sibelius), we'll generate code.

  - Normal GHCI can *evaluate/normalize* music expression and print the result as text using `Show`. Add more type classes and provide a GHCI alternative that also allow rendering (e.g. via MIDI and Lilypond).

  - MVP: When moving cursor to an expression, show it visualized in Window, with caching.
    - Should work out of the box for all common types (e.g. Common.Pitch, Music.Prelude.Music etc)
    - Also show playback controls/audio rendering?
    - For functions, show controls for inputs (with option of reflecting selected value back into the code)

  - Previous work to copy/use:
    - Jupyter/Jupyterlab?
    - Interactive Haskell environments (e.g. "IDEs")
      - Can any of them show/eval/typecheck expression on hover/select?
      - ghcide/hie-engine
        - Both of these have Nix packaging, but the packager assume that the IDE env is
          to be installed at the top-level (e.g. outside any build environment), which
          means we can't depend on it for any user-facing functionality.
      - Customize/fork GHCi
    - Other DSLs (Diagrams, Haskell for Mac)

  - Make `examples` use this

  - What is the value is big/slow to render?

- Replace Aeson with typed serialization

- Get rid of Option (can use plain Maybe/First/Last now)

- Get rid of duplication in music-suite.cabal
