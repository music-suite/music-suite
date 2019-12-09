
## TODO

**We use this file instead of an issue tracker, for now**

- Replace all uses of `data` directory with quasi-quoters (fail at compile-time if
  not existing/not parsing correctly)

- Make all examples compile with the new build system

- Make documentation generation compile/work with the new build system

- Add more examples/tests

- Rename the core sequential combinator (to what?)

- Improve rcat: do not use Enum

- Finish/document new export code
  - Add unit tests for all export formats/backends:
    - MIDI
    - Standard notation (common part of Lilypond/MusicXML)
    - Lilypond
    - MusicXML

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
  - MVP: When moving cursor to an expression, show it visualized in Window
    - Should work out of the box for all common types (e.g. Common.Pitch, Music.Prelude.Music etc)
    - Also show playback controls/audio rendering?
    - For functions, show controls for inputs (with option of reflecting selected value back into the code)
  - Previous work to copy/use:
    - Jupyter/Jupyterlab?
    - Interactive Haskell environments (e.g. "IDEs")
      - Can any of them show/eval/typecheck expression on hover/select?
    - Other DSLs (Diagrams, Haskell for Mac)
  - Make `examples` use this
  - What is the value is big/slow to render?

- Replace Aeson with typed serialization

- Get rid of duplication in music-suite.cabal
