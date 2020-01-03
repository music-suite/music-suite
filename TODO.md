
## TODO

**We use this file instead of an issue tracker, for now**

- [ ] Secure Hackage

- [X] Replace all uses of `data` directory with quasi-quoters (ideally: fail at compile-time if
  not existing/not parsing correctly)

- Add more examples (e.g from Piece1, Piece2 etc)

- Finish UMTS (Unofficial MusicXML Test Suite)
  - We have manual Haskell encodings of UMTS data which we used to test the export pipeline from StandardNotation.Work to Lilypond/XML
  - Remaining work:
    - Make sure output look like the official Lilypond output (produced through running musicxml2ly on the official XML files) for both
      our Lilypond and MusicXML exports (using Sibelius/MuseScore).
  - Turn into golden/regression tests assuring the XML/Ly output of Haskell encoded UMTS cases does not affect visual appearance. If the
    output changes the goldens will flag and the developer has to manually ensure that the visuals are unaffected.
  - Later: Maybe use approximate image diffs (comparing the entire rendering pipeline to musicxml2ly on the original XML files) instead
  - Run as part of CI builds

- Fix compiler warnings

- Get rid of all CPP

- Port issues from the old tracker

- Never fail on overlapping events

- Make all examples compile with the new build system

- fromMidiProgram/fromMusicXmlSoundId are unsafe, rename accordingly. Only use these internally (users can use Music.Parts.<instrumentName> instead).
  - Could potentially use a quasiquoter for compile-time checked arbitrary instrument IDs

- Make documentation generation compile/work with the new build system
  - Make sure doc generation/doctests are run in CI
  - Use hslinks or similar (disabled in Makefile for now)
  - Make transf generate music files:
    - Depends on [entry-point] above. Specifically the invocation in doc-tools/src/Text/Transf.hs has to work. It used to invoke music2ly etc which no longer exist. To make it work again:
    - [ ] Add something to Transf.hs to turn E into "main = Music.IO.defaultMain (E)"
    - [ ] Define Music.IO.defaultMain to invoke the new export and expose a CLI similar to what's expected in Transf.hs (dynamically dispatching on output format etc).

- Add more examples/tests

- Use Records (e.g. Vinyl) instead of Note transformers (e.g. Music.Score.DynamicT)
  - See `sketch`
  - Check compatibility with current HasPitch/HasPitches etc and new GHC record proposals

- Rename the core composition operators (was: scat/pcat/rcat)
  - seq/par/div - nice, probably simplest. The Prelude conflict is OK.
  - sequence/parallel/staves - also nice. Conflict with Control.Applicative is arguably worse (though sequenceA
    should be preferred in modern Haskell)
  - line/chord/staves, melody/chord? - Used in temporal-media, but both too specific?
  - sequentially/simultaneously - Too long?
  - follow/overlap/divisi

- Replace (Option :. Last) with Maybe now that Semigroup is a superclass of Monoid

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

  - [ ] !! Get rid of divisor component in Division, just mark everything as "I", "I.1" etc.
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

- Regression test:
  - Quantization
  - Voice separation

- Finish/document new export code ("Export2")
  - Make Export2 support:
    - Colored noteheads?
        To be added to Asp1.
        Could be part of pitch type.
    - Playing techinques: pizz, trem, harmonics, gliss
      - Gliss should be added to Asp1 (part of pitch).
      - Tremolo to be added to Asp1 (new layer: LocalTime or similar)
      - Everything else to be added to part as a "technique" component
        - Note "tremolo as fast as possible" could be seen as a pure coloristic effects: fit in part
    - Expression text (e.g. "dolce")
    - Lyrics
    - Free-form text?
    - The stuff in Score.Meta (e.g. key/time signatures)
      - A lot of this is already handled, needs proper testing
    - More advanced articulation?
      - Loudness dimension
      - Time dimension
      - Pitch dimension (e.g. bends)
  - Add unit tests for all export formats/backends:
    - MIDI
    - Standard notation (common part of Lilypond/MusicXML)
    - Lilypond
    - MusicXML
  - Benchmark!
  - Depends on [entrypoint]

- New backends (ideas):
  - ABC notation
  - Vextab
  - Audio engines:
    - csound-expression/temporal-media
    - scsynth
  - Other symbolic Haskell/Music libraries:
    - Euterpea
    - TidalCycles
  - Graphical backends
    - Piano roll



- $entrypoint Decide on top-level interface
  - By default recommend *no IO*

  - All data is is in the DSL/Haskell code. For *import formats* (such as Sibelius), we'll generate either 1) Haskell code or 2) TIDL serialized data, which can be automatically converted to Haskell code as per TIDL semantics.

  - Normal GHCI can *evaluate/normalize* music expression and print the result as text using `Show`.

  - *Design:*
    - Each backend has a *main rendering* function taking (`Score Asp1` etc) or similar to some other type A, possibly inside an effect F for failure/logging/parameters etc. `Score Asp1 -> F A`. This can be composed out of intermediate types, e.g. `Work` is used by Lilypond/MusicXML. We have `Score Asp1 -> F Work` and `Work -> F Lilypond`.
    - Overloaded function rendering `Score Asp1` or `Work`, suitable for preview use (e.g. show chords as a simple one-chord score, vocal ranges as a simple two note score with a slide, etc.). Similarly for audio preview. These functions have a role similar to `Show` and should not be used for "real" export (warn about this in the docs!).
    - Finally, some basic utilities for IO: `Score Asp1 -> IO ()` to render a score as a CLI program taking output format, etc. Similarly for the various intermediate types.


- Interactive editing/preview (see also preview class in $entrypoint).
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

    - Alternatives
      - defaultMain a la Diagrams
        - generates a CLI program that renders the result (can take arguments)
        - TODO what types should be renderable
      - Make a

  - Make `examples` use this

  - What is the value is big/slow to render?

- Replace Aeson with typed serialization

- Get rid of Option (can use plain Maybe/First/Last now)

- Better syntax for entering pitch, maybe using a quasi-quoter
  - See e.g. Mozart example
  - See syntax sketches, also compare Lilypond
  - OTOH the language "just basic Haskell" is maybe more important than simple syntax?

- Fix lawless (HasPosition (Score a)) by adding default/empty position to class

- Proper scale/chord type supporting all common use-cases
  - Making (infinite) octave-repeating scales from pitches/intervals

- Get rid of duplication in music-suite.cabal

- https://github.com/music-suite/music-score/issues/340

- https://github.com/music-suite/music-score/issues/298

- IDE allowing "preview on hover"
  - Normal text editor
  - When moving around in a single file (with/without a 'main' function) any hovered expression should
    trigger a preview (visual/audial) in an editor window, as if the expression had been applied to
    defaultMain (see above).

- For each part we want to know:
        - Classification:
            - Type: (i.e. woodwind)
            - Family: (i.e. saxophone)
            - Range: (i.e. tenor)
        - Range (i.e. [c_:e'])
        - Transposition:
            sounding = written .+^ transp
        - Suggested clefs

