
## TODO

**We use this file instead of an issue tracker, for now**

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

- Decide on top-level interface
  - By default recommend *no IO*
  - All data is is in the DSL/Haskell code. For *import formats* (such as Sibelius), we'll generate code.
  - Normal GHCI can *evaluate/normalize* music expression and print the result as text using `Show`. Add more type classes and provide a GHCI alternative that also allow rendering (e.g. via MIDI and Lilypond).
  - Use Jupyter for this?
  - Make `examples` use this

- Get rid of duplication in music-suite.cabal
