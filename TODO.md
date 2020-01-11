
## TODO

**Note: We use this file instead of an issue tracker, for now**

Consider switching to a decentralized issue tracker such as:

- https://github.com/dspinellis/git-issue
- https://github.com/MichaelMure/git-bug

---

- [X] Replace all uses of `data` directory with quasi-quoters (ideally: fail at compile-time if
  not existing/not parsing correctly)

- [X] Phrase traversal exampl in User Guide is broken (missing slurs and notes!)

- [ ] New (current) export does not render tremolo/gliss/harmonics/text/color

- [X] Add more examples (e.g from Piece1, Piece2 etc)
  - [ ] Make them all compile (add to cabal file!)
  - [ ] Make all examples compile with the new build system

- [X] Can not build docs in CI (pushd missing from shell). Why? The nix-shell is meant to be reproducible.
  - This might work post 6694359cbe17bf3880714f8b3cd8b018da083b43, try reverting b11b2593b12a9d3014b36651f5094a12be0631f8 and test in CI again

- [X] Restore all examples in User-Guide.md (marked TODO)

- Finish UMTS (Unofficial MusicXML Test Suite)
  - We have manual Haskell encodings of UMTS data which we used to test the export pipeline from StandardNotation.Work to Lilypond/XML
  - Remaining work:
    - Make sure output look like the official Lilypond output (produced through running musicxml2ly on the official XML files) for both
      our Lilypond and MusicXML exports (using Sibelius/MuseScore).
  - Turn into golden/regression tests assuring the XML/Ly output of Haskell encoded UMTS cases does not affect visual appearance. If the
    output changes the goldens will flag and the developer has to manually ensure that the visuals are unaffected.
  - Later: Maybe use approximate image diffs (comparing the entire rendering pipeline to musicxml2ly on the original XML files) instead
  - Run as part of CI builds

- Use modern type-level nats in Music.Pitch.Equal

- Fix all compiler warnings

- Get rid of all CPP

- Remove/fix code stubs/undefined

- Port issues from the old tracker

- Never fail export on overlapping/simultaneously events
  $needsTests
  - What is the correct behavior if a score is exported where a some part has overlapping notes?
  - Also test phrase traverals in the precense of overlapping
  - Generally this should be fine, though currently the backend/export code does not handle it
    correctly.
  - Putting overlapping events in monophonic instruments (e.g. flute) should be a linting error,
    similar to range etc.

- Music.Score.Export contains internal modules and should be renamed accordingly
  - What top-level interface should we support other than defaultMain?

- In Parts: extracted/extractedWithInfo should be Traversals, not lenses to lists (the latter
  is generally law-breaking)

- Cache in transf uses hash of expression only (should be expr + music-suite itself)

- New features:
  - Constraints
  - Counterpoint
  - Harmonic systems (harmonic spaces?)
  - Randomization/markov etc.
  - Orchestration
  - Melodic shapes
  - Check "playing difficulty" (most basically range, but also breathing, range over time etc)
  - "Snap to closest beat/beat group/bar" to model pickups etc. In combination with concat.

- Render all examples (see music-suite.cabal and examples/ directory for documentation/web site)

- Articualtion
  - [ ] Make sure tenuto/portato is rendered
  - [ ] Remove 'separated'

- Lilypond export
  - [ ] For LilypondInline, hide instrument names (at least if they're all "Piano X")
  - [ ] Inline examples do not render composer/title

- fromMidiProgram/fromMusicXmlSoundId are unsafe, rename accordingly. Only use these internally (users can use Music.Parts.<instrumentName> instead).
  - Could potentially use a quasiquoter for compile-time checked arbitrary instrument IDs

- [ ] Large scores makes Lilypond segfault

- Test generating all examples/documentation (and add more) in CI (nightly?)
  - [ ] Make CI validate MIDI output (how?)
  - [ ] Make CI validate MusicXML 3.0 output (using its XSD schema)
  - [ ] Make CI run Lilypond on all examples to assure there are no errors

- [ ] Make documentation generation compile/work with the new build system
  - [X] Make sure doc generation/doctests are run in CI
  - [ ] Use hslinks or similar (disabled in Makefile for now)
  - [X] Make transf generate music files:
    - [X] Use [entry-point] as per above.
    - [X] Add something to Transf.hs to turn E into "main = Music.IO.defaultMain (E)"
    - [X] Define Music.IO.defaultMain to invoke the new export and expose a CLI similar to what's expected in Transf.hs (dynamically dispatching on output format etc).

- Add more examples/tests

- Use Records (e.g. Vinyl) instead of Note transformers (e.g. Music.Score.DynamicT)
  - See `sketch`
  - Check compatibility with current HasPitch/HasPitches etc and new GHC record proposals

- Rename the core composition operators
  - [X] pseq, ppar
  - [ ] Rename rcat to pdiv (or similar)

- Replace (Option :. Last) with Maybe now that Semigroup is a superclass of Monoid

- [X] Improve rcat: do not use Enum
  - [X] Make Common.Part use NonEmpty division list for Subpart (part I is the default for all instruments)
  - [X] New type and impl of rcat and </>
  - [ ] Document, as per below:
    - This is a utility combinator. It should:
      - Be a binary operator with a foldr variant (ract, </>)
      - Work on anything with parts
      - Only affect the subpart component. It does not matter what it does to the instrument component.
      - If it binds to the left, it should set the part of the RHS to the max part of the LHS + 1
        - Flipping RHS and LHS above would also work
        - Examples:
            - (c </> c </> c)
                = (set part piano1 c <> set part piano2 c <> set part piano3 c)
                as piano1 is the default part (mempty)
                  type Subpart Common.Part = NonEmptyList Integer
                  maxSubpart {piano1} = [1]
                  incrSubpart [1] = [2]

      - Note the meaning of maxSubpart for Common.Subpart (~ [Integer]):
          maxSubpart = pure . maximum . fmap head
      - In other words, the part component must allow the operations
          maxSubpart :: Set a -> Subpart a
          incrSubpart :: Subpart a -> Subpart a
          subpart :: Lens' a (Subpart a)

- Check `examples` dir for code that can be moved upstream to the main library

- $splitSemantics
  - [ ] Check split semantics for Voice/Note. Should be possble to write a simple instance for both not and voice,
    not requiring a Splittable instance for the contained note.
  - [ ] Compile examples/Piece/Piece8.hs

- Regression test:
  - Quantization
  - Voice separation

- Finish/document new export code (formerly "Export2")
  - Make new export support:
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
  - Audio engines:
    - csound-expression/temporal-media
    - scsynth
  - Other symbolic Haskell/Music libraries:
    - Euterpea
    - TidalCycles
  - Graphical backends
    - Piano roll



- [X]  $entrypoint Decide on top-level interface
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

- [X] Reexport (set, over) from lens in default prelude? (see examples!)

- Better syntax for entering pitch/time, maybe using a quasi-quoter
  - See e.g. Mozart example
  - See syntax sketches, also compare Lilypond
  - OTOH the language "just basic Haskell" is maybe more important than simple syntax?

- Fix lawless (HasPosition (Score a))
  - Idea 1: by adding default/empty position to class (a HasEnvelope in Diagrams)
    - Pros: simple. Same operators (e.g. for juxtaposition) can be used for Scores, Notes, Spans, etc.
    - Cons: Disallows the onset/offset lenses Music.Time.Position
      - E.g. what happens if you set the era of a Note to Nothing?
  - Idea 2: Make a separate class for things which may have empty positions (e.g. Scores)
    - Pros:
      - Can retain lenses in Music.Time.Position
      - Allow a convenient definition of 'rest' as a synonym for 'mempty'. No more removeRests/mcatMaybes!
    - Cons: Different juxtaposition operators (e.g. scat) for scores/notes etc.
  - Idea 3: Decore empty type with an extra era, e.g. semantically:
      type Score a = Either Span (NonEmpty (Event a))
      instance Monoid (Score a) where
        mempty = Left mempty
        mappend (Left _) x = x
        mappend x (Left _) = x
        mappend (Right x) (Right y) = Right (x <> y)
    - Pros:
      - Allows onset/offset lenses, etc
      - Same juxtaposition operators everywhere



- Proper scale/chord type supporting all common use-cases
  - Represent functions/modes and chords/scales
  - Making (infinite) octave-repeating scales from pitches/intervals
  - Looking up pitches
  - Transposable instance for chords/scales (pitch-wise)

- Get rid of asNote/asScore/asVoice/asTrack

- [X] Get rid of Prelude.StandardNote et al, use Asp1 (renamed!) instead

- Get rid of duplication in music-suite.cabal

- https://github.com/music-suite/music-score/issues/340

- https://github.com/music-suite/music-score/issues/298

- [X] Replace ucat with (new) rcat

- $minorAspect
  - There's a philosophical difference between "major" aspects (pitch, dynamics, articulation, part)
    and "minor" ones (tremolo, slide/gq
    liss, freeform text, colour, harmonics).
  - Some of these are being abused in current examples, e.g. freeform text is used to denote pizz/arco
  - The representation of tremolo, slide/gliss, harmonics could be smarter. E.g. we should maybe use
    (Reactive Pitch) or similar instead of context-specific begin/end marks. Note this might require
    countext-bound rewriting as we currently do with dynamics and articulation.
  - Simple playing techniques should be standarized, similarly to what we do with instruments.
    The technique/instrument/rage relations should be availible somewhere.

- Purge lawless instances, do more property testing

- If possible, derive everything in Music.Score.Internal.Instances

- IDE allowing "preview on hover"
  - Normal text editor
  - When moving around in a single file (with/without a 'main' function) any hovered expression should
    trigger a preview (visual/audial) in an editor window, as if the expression had been applied to
    defaultMain (see above).

- [X] $reactiveSemantics
  - Briefly: Simultaneous events should not be allowed with Reactive. Semantically:
      type Reactive a = ([Time], Time -> a)

- [X] For each instrument we want to know:
        - Classification:
            - Type: (i.e. woodwind)
            - Family: (i.e. saxophone)
            - Range: (i.e. tenor)
        - Range (i.e. [c_:e'])
        - Transposition:
            sounding = written .+^ transp
        - Suggested clefs

