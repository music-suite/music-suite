
## TODO

### Note

We use this file instead of an issue tracker. Issues filed in Github etc should be copied here.

Consider switching to a decentralized issue tracker such as:

- https://github.com/dspinellis/git-issue
- https://github.com/MichaelMure/git-bug

- [ ] Single, append-only release note document

- [ ] Make defaultMain expose a type-safe CLI (e.g. for integration into build/test/gen
  pipelines).

- [ ] Remove Makefiles (none of them run in CI, mostly dead code)

- [ ] Get rid of remaining orphans

- [ ] Data.Monoid.Average should use (Sum `Product` Count), not a list.
  Could have massive performance impact, given that this is used for dynamics/articulation.

---

- [ ] Visual regression tests. Design:
    - We'll refer to as examples `union` test/music (extracted from docs) as Examples

    - All Examples must be expressions compatible with `defaultMain`

    - Check all Examples generate something with all backends (or randomize)

    - For each backend B in testbackends
      - Render a set of `Music` expressions using some backend B
      - Check if output is identical to the "committed" version
        - If the source has changed, allow user to "commit" without preview
        - Otherwise, make the user check preview and commit if they are unchanged

    - Generate nice docs + Example gallery + "Visual issue tracker"


- [ ] Use dynamics map
    {-
        ppp -42    -36
        pp  -36    -36
        p   -30    -24
        mp  ?
        mf  -24    -18
        f   -18    -12
        ff  -18    -12




        Midi velocities according to Apple:
            16  32  48  64  80  96  112  127
            ppp pp  p   mp  mf  f   ff   fff

        Nakamura (1987) The communication of dynamics between musicians and listeners through musical performance
    -}

- [ ] Verify that harmonics can be played on the relevant instrument

- [ ] Add snapTo

    {-
    -- |
    -- Compose sequentially by aligning the nominal position of each value to the
    -- first available time value.
    --
    -- TODO this requires another constraint for nominal position. For (Aligned ((t,_),_))
    -- the nominal position is t.
    --
    -- @
    -- length xs = length (snapTo ts xs)
    -- @
    snapTo :: (HasPosition a, Transformable a) => Stream Time -> [a] -> [a]
    -}


- [ ] Rescue this comment?

    -- TODO: resture _position to HasPosition, then restore this comment:
    --
    -- Many values such as notes, envelopes etc can in fact have many positions such as onset,
    -- attack point, offset, decay point time etc. Rather than having separate methods for a
    -- fixed set of cases, this class provides an interpolation from a /local/ position to
    -- a /global/ position. While the local position goes from zero to one, the global position
    -- goes from the 'onset' to the 'offset' of the value.



- [ ] Semantics of Reactive/Behavior

    {-
      TODO Semantic fuzz

      Reactive implies that values change at switchpoints, but should not make assumptions about what the value is *at* the
      switchpoint.

      Behavior represents continous values, so it knows the value at each switchpoint (semantically: Time -> a).
      Hence the combinator (switch' :: Time -> B a -> B a -> B a -> B a) makes sense.

      Reactive can do this as well (i.e. with the current semantics: ([Time], Behavior a)), however this is not necessarily
      desirable.

      Bad:
          updates - Promotes association of Time with value (though it makes no assumption that the Reactive *is* this value at the given time).
          discrete/atTime/continous - Forces implementation to choose arbitrary value at switchpoint
    -}



- [ ] More Span combinators:

    {-
    afterOnset :: Time -> Span -> Bool
    t `afterOnset` s = t >= _onsetS s

    strictlyAfterOnset :: Time -> Span -> Bool
    t `strictlyAfterOnset` s = t > _onsetS s

    beforeOnset :: Time -> Span -> Bool
    t `beforeOnset` s = t <= _onsetS s

    strictlyBeforeOnset :: Time -> Span -> Bool
    t `strictlyBeforeOnset` s = t < _onsetS s

    afterOffset :: Time -> Span -> Bool
    t `afterOffset` s = t >= _offsetS s

    strictlyAfterOffset :: Time -> Span -> Bool
    t `strictlyAfterOffset` s = t > _offsetS s

    beforeOffset :: Time -> Span -> Bool
    t `beforeOffset` s = t <= _offsetS s

    strictlyBeforeOffset :: Time -> Span -> Bool
    t `strictlyBeforeOffset` s = t < _offsetS s
    -}
    -- Param order OK

    {-
    -- Name?
    startsWhenStarts :: Span -> Span -> Bool
    a `startsWhenStarts` b = _onsetS a == _onsetS b

    -- Name?
    startsWhenStops :: Span -> Span -> Bool
    a `startsWhenStops` b = _onsetS a == _offsetS b

    -- Name?
    stopsWhenStops :: Span -> Span -> Bool
    a `stopsWhenStops` b = _offsetS a == _offsetS b

    -- Name?
    stopsWhenStarts :: Span -> Span -> Bool
    a `stopsWhenStarts` b = _offsetS a == _onsetS b

    startsBefore :: Span -> Span -> Bool
    a `startsBefore` b = _onsetS a < _onsetS b

    startsLater :: Span -> Span -> Bool
    a `startsLater` b = _onsetS a > _onsetS b

    stopsAtTheSameTime :: Span -> Span -> Bool
    a `stopsAtTheSameTime` b = _offsetS a == _offsetS b

    stopsBefore :: Span -> Span -> Bool
    a `stopsBefore` b = _offsetS a < _offsetS b

    stopsLater :: Span -> Span -> Bool
    a `stopsLater` b = _offsetS a > _offsetS b
    -}
    {-
    contains
    curtails
    delays
    happensDuring
    intersects
    trisects
    isCongruentTo
    overlapsAllOf
    overlapsOnlyOnsetOf
    overlapsOnlyOffsetOf
    overlapsOnsetOf
    overlapsOffsetOf



    -}

    -- timespantools.timespan_2_starts_during_timespan_1
    -- timespantools.timespan_2_starts_when_timespan_1_starts
    -- timespantools.timespan_2_starts_when_timespan_1_stops
    -- timespantools.timespan_2_stops_after_timespan_1_starts
    -- timespantools.timespan_2_stops_after_timespan_1_stops
    -- timespantools.timespan_2_stops_before_timespan_1_starts
    -- timespantools.timespan_2_stops_before_timespan_1_stops
    -- timespantools.timespan_2_stops_during_timespan_1
    -- timespantools.timespan_2_stops_when_timespan_1_starts
    -- timespantools.timespan_2_stops_when_timespan_1_stops
    -- timespantools.timespan_2_trisects_timespan_1

    {-
    Two alternative definitions for midpoint:

    midpoint x = onset x + duration x / 2
    midpoint x = (onset x + offset x) / 2

    Both equivalent. Proof:

      let d = b - a
      (a + b)/2 = a + d/2
      (a + b)/2 = a + (b - a)/2
      a + b     = 2a + (b - a)
      a + b     = a + b
    -}



- [ ] More voice combinators:

    -- changeCrossing   :: Ord a => Voice a -> Voice a -> (Voice a, Voice a)
    --
    -- changeCrossingBy :: Ord b => (a -> b) -> Voice a -> Voice a -> (Voice a, Voice a)
    --
    -- processExactOverlaps :: (a -> a -> (a, a)) -> Voice a -> Voice a -> (Voice a, Voice a)
    --
    -- processExactOverlaps' :: (a -> b -> Either (a,b) (b,a)) -> Voice a -> Voice b -> (Voice (Either b a), Voice (Either a b))

    -- TODO could also use (zipVoiceWith' max) or (zipVoiceWith' min)

    -- -- |
    -- -- Split all notes of the latter voice at the onset/offset of the former.
    -- --
    -- -- >>> ["a",(2,"b")^.note,"c"]^.voice
    -- -- [(1,"a")^.note,(2,"b")^.note,(1,"c")^.note]^.voice
    -- --
    -- splitLatterToAssureSameDuration :: Voice b -> Voice b -> Voice b
    -- splitLatterToAssureSameDuration = splitLatterToAssureSameDurationWith dup
    --   where
    --     dup x = (x,x)
    --
    -- splitLatterToAssureSameDurationWith :: (b -> (b, b)) -> Voice b -> Voice b -> Voice b

    -- polyToHomophonic      :: [Voice a] -> Maybe (Voice [a])
    --
    -- polyToHomophonicForce :: [Voice a] -> Voice [a]


- [ ] Proper velcoity table for e.g. MIDI backend

{-
    ppp -42    -36
    pp  -36    -36
    p   -30    -24
    mp  ?
    mf  -24    -18
    f   -18    -12
    ff  -18    -12




    Midi velocities according to Apple:
        16  32  48  64  80  96  112  127
        ppp pp  p   mp  mf  f   ff   fff

    Nakamura (1987) The communication of dynamics between musicians and listeners through musical performance
-}

- [ ] Better articulation type
{-

  References

    Keller: Phrasing and Articulation: A Contribution to a Rhetoric of Music

      Keller distinguishes between articulation and phrasing:
        - Phrasing is related to the structure or grammar of the music,
          how hierarchical relationsship emerges.
        - Articulation is "everything else", the individual interpretation of
          the melodic line.
        - Keller consider phrasing objective. (It is at least non-deterministic!)

    http://www.speech.kth.se/publications/masterprojects/2004/Jerkert.pdf

      - Articulation is a *local* alteration of other properties
        - There are multiple interacting hierarchical relationships
        - Articulation has to do with *emphasis*: alteration of properties leads
          to more or less emphasis (compare Laws of Perception)
      - Most common:
        - Time (common: stacatissimo, staccato, portato, legato)
          - "articulation ratio", i.e. duration/IOI
          - "relative IOI", i.e. prolongation of a note
        - Dynamics (common: accent, marcato)
          - fp/sharp attacks etc (i.e. in wind, string)
          - Relative level (i.e. in piano music)
        - Pitch
          - Vibrato, local adjustments (i.e. brighter notes), "slide-in"
        - Timbre
          - Dryness/spectral richness (i.e. more overtones)

    http://www.jbiomech.com/article/S0021-9290%2898%2900113-4/abstract
-}

{-

From music21:
  Accent
  Bowing
  BrassIndication
  BreathMark
  Caesura
  DetachedLegato
  Doit
  DoubleTongue
  DownBow
  DynamicArticulation
  Falloff
  FretBend
  FretIndication
  FretTap
  FrettedPluck
  HammerOn
  Harmonic
  HarpFingerNails
  HarpIndication
  IndeterminantSlide
  LengthArticulation
  NailPizzicato
  OpenString
  OrganHeel
  OrganIndication
  OrganToe
  PitchArticulation
  Pizzicato
  Plop
  PullOff
  Scoop
  SnapPizzicato
  Spiccato
  Staccatissimo
  Staccato
  Stopped
  Stress
  StringFingering
  StringHarmonic
  StringIndication
  StringThumbPosition
  StrongAccent
  TechnicalIndication
  Tenuto
  TimbreArticulation
  TonguingIndication
  TripleTongue
  Unstress
  UpBow
  WindIndication
  WoodwindIndication






----------------


  Stress
  Unstress
  Accent
  StrongAccent

  Tenuto
  Spiccato
  Staccato
  Staccatissimo

  Harmonic
  OpenString
  Stopped
  HammerOn
  PullOff

  OrganIndication
  OrganHeel
  OrganToe
  UpBow
  DownBow

  DetachedLegato (laisser vibrer)
  IndeterminantSlide



  Doit (grace?)
  Falloff?
  StringFingering
  HarpFingerNails

  NailPizzicato
  Pizzicato
  SnapPizzicato

  Bowing?

  TonguingIndication
  DoubleTongue
  TripleTongue


  HarpIndication
  FretIndication
  FrettedPluck
  FretTap
  LengthArticulation
  StringIndication
  StringThumbPosition


  Plop
  Scoop

  FretBend

  WindIndication
  BrassIndication
  WoodwindIndication
  TechnicalIndication
  TimbreArticulation

  BreathMark
  Caesura

-}

- [ ] Expose Common.Part internals
  - As a record
  - Remove HasSubpart and expose standard lens

- [ ] Remove empty methods in Parts.Instrument

- [X] MusicXML Parser
  - [X] Make all tests pass (music-suite-test-xml-parser)
  - [ ] Full ornament support, see https://github.com/hanshoglund/music-suite/pull/22#issuecomment-633314407

- [X] Bug: Regression in 2b8bb331098eac1e14b6f0cc6a7a8833ca2fb533
  Intervals not displayed properly

- [ ] Bug: Scores with harp do not render in Lilypond

- [ ] Bug: since GHC 8.8.3 upgrade, doctester does not compile
  - Try cabal-doctest instead https://github.com/phadej/cabal-doctest

- $doctests
  - [X] Run locally (README)
    - [X] Tool works
    - [ ] Fix hack for default-extensions (see Doctester.hs)
    - [ ] Make this work for all modules. See what modules are not run in the CI for an up-to-date reference.
    - [X] Try property-based testing (see doctest/README)
  - [X] Run in CI

- [ ] Assure a script to run *all* builds, tests and doc gens *from scratch*
  - Should ideally be invoked in CI
  - Because of overeager caching in doc gens ($transfCache), requires `rm -rf docs/build` to be reproducible

- [X] Rename Music.Score.Pitch -> GetPitch
  - [X] Pitch
  - [X] Dynamics
  - [X] Articulation
  - [X] Technique
  - [X] Part
  - [X]  Also rename SomeTechnique -> Music.Technique.Technique or similar (a la pitch et al)

- Move Music.Pitch.Literal to Music.Pitch.Common (as they rely on Common(Pitch, Interval))

- `tremolo` should take a duration, not an integer!

- [X] Won't fix!
  - Rename meta-information to "global"?

- [X] Remove whilstLT etc as well as Transformable constraints from HasPitch/HasDynamic/HasArticulations etc

- [X] Do not expose IntervalL (hide or remove completely)

- [X] Replace all uses of `data` directory with quasi-quoters (ideally: fail at compile-time if
  not existing/not parsing correctly)

- [ ] Long cresc/dim should render as text by default.

- Show instance for Duration/Time should use decimal form when short (e.g. 1.5, not 3/2)

- API improvements:
  - Move varticulation, addArticulation out of public API
  - Move vdynamic, addDynCon out of public API
  - Move vtechnique, etc out of public API
  - Make Aligned a HKT, e.g. instead of (Aligned . Voice), we'd use (Aligned Voice)
    (with the same Applicative)
  - Put combinators (not classes) on top of the following modules
    - Music.Score.Articulation
    - [X] Music.Score.Dynamics
    - Music.Score.Technique
    - Music.Score.Part
    - Music.Score.Color
    - Music.Score.Harmonics
    - Music.Score.Slide
    - Music.Score.StaffNumber
    - Music.Score.Text
    - Music.Score.Ties
    - Music.Score.Tremolo
  - Music.Articulation.Articulation and Music.Dynamics.Common should be "record types"
    with proper field names.
  - [X] Remove Alignable class (use Aligned the type)
  - [X] Remove upChromatic/downChromatic (use up/down instead!)
  - [X] Remove Time parameter from aligned/alignTo (use delay)
    - Won't fix
  - Rename eventee/notee to transformed or similar (could be a class)
  - withContext/addCtxt can be generalized to any Traversable
  - Lunga is a Monad (though maybe a confusing one)
  - Aligned is an Applicative/Monad (also maybe confusing)
  - (Placed . Lunga) and (Aligned . Lunga) is an Applicative
  - Pattern is an Applicative (at least)
    - Compare Tidal instace for patterns
  - Remove Placed in favor of Aligned
  - Remove Track in place of Score and ([] . Aligned)
  - Get rid of DynamicsL (unify with standard type a la Pitch/Interval/String)
  - [X] Remove meta-data extraction (withTempo et al) from public API
  - Remove HasOctaves/HasQuality/HasNumber (retain interval instance)
  - Remove more classes/instances from StandardNote
    - Hide Tiable from public API
    - The following should be moved to constraints on the technique type:
      - HasHarmonic
      - HasSlide
      - HasTremolo
  - Remove Augmentable/Alterable (?)
  - Remove triples/pairs in favor of explicit traversals (see example in Time.Score)
  - [X] Pattern does not need Reversible! (Just use forward version: think modulo arithmetic.)
  - Chord/Scale:
    - [X] Use DataKinds/phantom type to distinguish chord vs scale (only difference is Inspectable instance)
      - Single GADT ScaleChord with "3 dimensions"
        - Orientation: seq vs par (scale vs. chord)
        - Voiced vs unvoiced (the latter has "only" the default close voicing and the "full" voicing)
        - Rooted vs not (scale vs. mode)
    - [X] Bifunctor instance for Scale/Chord
      - Pitch/Interval containers should be bifunctors (taking pitch and interval).
        Use AffinePair constraint on operations.
    - Add union/intersection/set diff (of the "infinite set") for chords/scales
      - Implement by finding all the points in the LCM(argDurations)
  - Make PVoice and MVoice newtype wrappers (hiding instances)
  - Remove Cons/Snoc instances for Voice
  - Hide FromField Ambitus instance (by using newtype wrappers)
  - Instruments
    - Conventionally primary instrument (eg, flute) vs. doubling (eg, piccolo)
    - Conventional staff layout (e.g. piano, organ)
  - Model (in high-level API)
    - Arpeggios
    - LaissezVibrer
    - Trills
  - Pitch
    - PitchClass, IntervalClass
    - https://en.wikipedia.org/wiki/Interval_vector
    - Time
    - RTM tree support?

- Scale/Chord and Pattern are isomorphic.
  Common implementation?
  Share operations in both directions?

- [ ] render e.g. snare drum parts correctly on single-line staff

- Internal improvements:
  - [X] Replace Control.Monad.Compose uses with (WriterT []) iso-deriving

- [ ] Docs: User-Guide
  - Title/Attribution not shown in examples (because the example uses the inline LY template?)
  - [X] Key sigs not shown
  - Tempi not shown
  - Fermatas not shown
  - Caesura/Breathing mark not shown
  - Rit/Acc not shown
  - Special barlines not shown
  - Clef override not shown
  - Reh marks
  - Annotations

- Voice/parsing ideas:
  - Voice/Score ideas:
    - Something like this for parsing voices out of "less structured" representations
      https://twitter.com/taylorfausak/status/1238584847536467969/photo/1
  - Maybe add new time type, a la Haskore/Mezzo:
      data Mus a = Note Duration a | Rest Duration | Par (V2 (Mus a)) | Seq (V2 (Mus a))
      - Would behave much like Time.Voice, e.g.
          instance HasDuration (Mus a)
          instance Transformable (Mus a) -- translation invariant
      - Can be aligned, rendered to a score etc.

  - Add parsers for aligned values, e.g.
      parseAlignedVoice :: MonadError e m => Score a -> m [Aligned (Voice a)]
      parseAlignedVoice :: MonadError e m => Score a -> m (Aligned (Mus a))
      etc.
    See $voiceSeparation

- [X] Phrase traversal exampl in User Guide is broken (missing slurs and notes!)

- [ ] New (current) export does not render tremolo/gliss/harmonics/text/color
  - Color can use ColorT as before
  - Text can use TextT as before
  - Harmonics should be recast using $playingTechniques
    - The current top-level combinators (in HasHarmonic) can stay, HarmonicT should go (use SomeTechnique)
  - Gliss should use $playingTechniques
    - Any consequtive pair of notes *in the same phrase* both marked with a gliss technique to be rendered
      using a line + the text "gliss"
  - Tremolo should be a combination of playing techniques (TODO) and *chords*
    - The current top-level combinators (in HasTremolo) can stay, HarmonicT should go (use SomeTechnique)
    - For both chord/non-chord tremolo, support measured (with duration) and unmeasured "z" or "three beams"
    - Non-chord trem: Easy, same as a "roll".
    - Chord trem:
      - All perfectly overlapping (e.g. having identical span) notes *in the same part* having a ChordTremolo
      playing technique to be rendered as chord tremolo.
        data ChordTremolo
          = Simultaneous          -- ^ double stop + bow tremolo
          | AlternatingHalfHarm   -- ^ left hand tremolo on one string
          | AlternatingBariolage  -- ^ left hand tremolo pn two strings
        - For e.g. piano Simultaneous chord trem makes no sense and there is only one form of alternating.

- [X] Add more examples (e.g from Piece1, Piece2 etc)
  - [ ] Make them all compile (add to cabal file!)
  - [ ] Make all examples compile with the new build system

- [X] Can not build docs in CI (pushd missing from shell). Why? The nix-shell is meant to be reproducible.
  - This might work post 6694359cbe17bf3880714f8b3cd8b018da083b43, try reverting b11b2593b12a9d3014b36651f5094a12be0631f8 and test in CI again
    - Could be due to "bash from env" warning shown when entring the nix-shell locally

- [X] Restore all examples in User-Guide.md (marked TODO)

- Finish UMTS (Unofficial MusicXML Test Suite)
  - Move from StandardNotation (and break up that module in general)
  - We have manual Haskell encodings of UMTS data which we used to test the export pipeline from StandardNotation.Work to Lilypond/XML
  - Remaining work:
    - Make sure output look like the official Lilypond output (produced through running musicxml2ly on the official XML files) for both
      our Lilypond and MusicXML exports (using Sibelius/MuseScore).
    output changes the goldens will flag and the developer has to manually ensure that the visuals are unaffected.
  - Later: Maybe use approximate image diffs (comparing the entire rendering pipeline to musicxml2ly on the original XML files) instead
  - Run as part of CI builds

- Visual golden/regression tests
  - Currently we often do "rm docs/build" and build the documentation as a form of regression test
  - This is wasteful because it re-invokes Lilypond
  - Better would be to extract all documentation snippets (+ the other examples?) and use them as *golden* tests
    - By default, check output output hash of each example snippiet
    - If this has changed, generate visual view comparing the old to the new version
      - Requires storing (checked-in) the *old output* with its input and output has

- Use modern type-level nats in Music.Pitch.Equal
  - E.g. KnownNat

- Fix all compiler warnings

- [X] Get rid of all CPP

- [X] Remove/fix code stubs/undefined

- [ ] Unify DynamicLensLaws, ArticulationLensLaws etc, if possible
  - Note Pitch only states one of the laws (because it usually is the innermost?)

- Articulation representation can't handle consequtive legato phrases
  E.g. `legato a |> legato b` is transformed into `legato (a |> b)` which is not always right

- Add log levels/warnings to fromAspects log. Use cases:
  - Overlapping notes when not expected (e.g. solo monophonic instrument)
  - Playing techniques not support for an instrument (see $playingTechniques)
  - Other unplayable things

- [ ] $playingTechniques Playing techniques
  - Design:
    - Separate aspect from part
    - Make instrument types (woodwind, brass) etc into a kind using DataKinds
      - Aside: Recast PercussionInstrument etc to be (Instr Percussion) or similar.
    - Make `Technique` a typecon `[InstrumentFamily] -> Type`, e.g:
        - `Pizz :: Technique '[Strings]`
        - `FlutterTongue :: Technique '[Brass, Woodwind]`
        - etc.
    - For `StandardNote/TechniqueT`, use `SomeTechnique ~ (exists xs . Technique xs)` for now.
      - Gather technique along with parts and throw away unplayable techniques in fromAspects (emitting warnings)
    - Checking playability, for example:
      - String natural harmonics
      - String double stops
    - Stateful notations
      - Examples
        - Mutes (none, straight mute, plunger etc)
        - Plenty others in strings, e.g. pizz/arco, sul tasto/nat/pont etc
      - As with dynamics we notate these "per note" in the logical representation
        - In fromAspects, traverse each part looking for changes

- [ ] Replace note transformer stack with Vinyl or similar (when possible)
  - Try a polymorphic transformer first, e.g. PitchT (the rest should be easy)


- Maybe add new time type, a la Haskore/Mezzo:
    data Mus a = Note Duration a | Rest Duration | Par (V2 (Mus a)) | Seq (V2 (Mus a))
    - Would behave much like Time.Voice, e.g.
        instance HasDuration (Mus a)
        instance Transformable (Mus a) -- translation invariant
    - Can be aligned, rendered to a score etc.

- Add parsers for aligned values, e.g.
    parseAlignedVoice :: MonadError e m => Score a -> m [Aligned (Voice a)]
    parseAlignedVoice :: MonadError e m => Score a -> m (Aligned (Mus a))
    etc.
  See $voiceSeparation

- Issues from the old tracker
  - We have a CLI interface for dynamically exporting to various backends and providing options.
  - Could use typed serialization for providing this. Would allowing combination of files/CLI etc
    and safer invocation from warapper programs (e.g. transf).
    - https://github.com/music-suite/music-suite/issues/11

  - More 12-tone/equal temperament stuff
    - Essentially combinatorics
    - Binary scales (?)
    https://github.com/music-suite/music-pitch/issues/56
  - [X] Pitch normalization which preserves spelling direction
    https://github.com/music-suite/music-pitch/issues/55
  - Parse Helmholtz, SPN etc
    https://github.com/music-suite/music-pitch/issues/54
  - Remove current invertChromatic, rename current invertPitches to invertChromatic
    https://github.com/music-suite/music-pitch/issues/51
  - [X] Iso `interval` is partial
    https://github.com/music-suite/music-pitch/issues/46
  - Harmony support
    https://github.com/music-suite/music-pitch/issues/35
  - More seamless conversions between 12-TET/Semitones and Pitch/Interval
  - Articulation should track agogic prolongation in addition to shortining/separation and accentuation
    https://github.com/music-suite/music-articulation/issues/3

  - [X] Phrase traversals currently fail at runtime if there are overlapping notes in a single part.
    https://github.com/music-suite/music-score/issues/208

  - $reversibleMeta
    https://github.com/music-suite/music-score/issues/119

  - [X] Port issues from all old Github trackers:
    - [X] music-pitch
    - [X] music-suite
    - [X] music-dynamics
    - [X] music-articulation

- [ ] Pitch/Interval is a module, not a vector space.

- Multi-staff customization: The current state will gracefully handle overlapping notes in a single part, drawing them on separate staves, however it may not distribute things ideally across the staves. The final state should do better by default *and* allow customization.

- [ ] Putting overlapping events in monophonic instruments (e.g. flute) should be a linting error,
    similar to range etc.

- [ ] Draw celesta/piano/organ on >1 staff by default

- [ ] Rename Inspectable -> Exportable?

- [ ] Make parts such as "Piano 0", "Piano (-1)", etc, unrepresentable

- [ ] we should never see Music/StandardNote in the user guide (specific/nice-looking types instead). The only purpose of Music/StandardNote is to be defaults/final objects.

- [ ] Make parts such as "Piano 0", "Piano (-1)", etc, unrepresentable

- [ ] Bug in rendering of "con sord" (see User Guide)

- [X] Never fail export on overlapping/simultaneously events
  - [X] Basic voice sepration added
    - What is the correct behavior if a score is exported where a some part has overlapping notes?
    - Generally this should be fine, though currently the backend/export code does not handle it
      correctly.
    - Putting overlapping events in monophonic instruments (e.g. flute) should be a linting error,
      similar to range etc.
    - The general problem of breaking up a score: $voiceSeparation

- Orchestration checks
  - Range checks
  - Preventing a playing technique being used with the "wrong" instrument.

- $voiceSeparation
  - Basic things work
  - [ ] Render (>1) voice per staff, e.g. for keyboard music
  - We currently use a simple greedy interval partitioning algorithm. Look into music-specific approaches,
    e.g. https://archives.ismir.net/ismir2002/paper/000005.pdf

- Music.Score.Export contains internal modules and should be renamed accordingly
  - What top-level interface should we support other than defaultMain?
      MonadLog, MonadError,
      IOExportM, PureExportM, runPureExportM, runIOExportM,
      toMidi, fromAspects, toLy, toXml
      Score, Asp1 (renamed!)

- In Parts: extracted/extractedWithInfo should be Traversals, not lenses to lists (the latter
  is generally law-breaking)

- $transfCache [ ] Cache in transf uses hash of expression only (should be expr + music-suite itself)

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

- Articulation
  - [ ] Make sure tenuto/portato is rendered
  - [ ] Remove 'separated'

- Lilypond export
  - [ ] For LilypondInline, hide instrument names (at least if they're all "Piano X")
  - [ ] Inline examples do not render composer/title

- fromMidiProgram/fromMusicXmlSoundId are unsafe, rename accordingly. Only use these internally (users can use Music.Parts.<instrumentName> instead).
  - Could potentially use a quasiquoter for compile-time checked arbitrary instrument IDs

- [X] Large scores makes Lilypond segfault
  - Solved by upgrading to Lilypond 2.20

- [X] Examples should not be Cabal executables
  - Fixed by adding -fno-code
  - Saves the slow linking step when doing Cabal build
    - Could also just disable codegen/linking!
    - Test in CI with cabal runhaskell
      - Note we need to *run* the examples to make sure the expressions don't
        diverge.

- [X] Get rid of (Transformable...) constraints in Pattern API

- [ ] Get rid of duplication in music-suite.cabal

- Test generating all examples/documentation (and add more) in CI (nightly?)
  - [ ] Make CI validate MIDI output (how?)
  - [ ] Make CI validate MusicXML 3.0 output (using its XSD schema)
  - [ ] Make CI run Lilypond on all examples to assure there are no errors

- [X] Make documentation generation compile/work with the new build system
  - [X] Make sure doc generation/doctests are run in CI
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

  - Though note in Score.Meta, all types should be refactored to be monoids in
    themselves, rendering the wrapper obsolete.

- [ ] "drum kit" staff support

- [ ] Do not draw cresc/dim for voices like `[mf, f, p, mf]` (e.g. only a single note local max/min)
  - In other words, for a line to be drawn there must be a monotonic increase/decrease spanning >2 notes
  - OTOH, if the durations are long enough (e.g. all >1/2) we can render the marks as we do now:
    - Draw cresc/dim: `pseq [c,level ff d,level _p e, d] |* (3/4)`
    - Should not draw cresc/dim:  `pseq [c,level ff d,level _p e, d] |* (1/8)`
  - Levels should be drawn as they are now

- hslinks:
  - Restore hslinks functionality (commented out in source), maybe using hasktags
    - hslinks steals syntax from TypeApplications
      - E.g. `inspectableToMusic @[Pitch]`
  - Make it work for the *code examples* too


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

- Finalize and test laws for
  - [ ] Splittable
  - [X] Reversible (WONT FIX: Reversible has been removed)

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
  - Guido
  - Audio engines:
    - csound-expression/temporal-media
    - scsynth
  - Other symbolic Haskell/Music libraries:
    - Euterpea
    - TidalCycles
  - Graphical backends
    - Piano roll

- [ ] Make AddMeta more similar to PartT, ArticulationT, etc
  - Style: move towards separating *types with exposed implementation* and *types with hidden implementations*
    - Former is useful for structural typing, flat module hierarchies, type driven development etc
      - Should export Generic
      - Can be used with Coercible, iso-deriving etc
      - Prefer whenever possible
    - Latter is useful for performance and runtime verification
      - Should not export Generic/unwrapping etc
      - Only use when necessary

- [X] Bug: TimeSignatures and similar only show up if providing span
  E.g. timeSignatureDuring works, timeSignature does not
  - Can not reproduce on cb0cdd1a5adb6d9e7f23659beda603450f66ddc0

- Save example data from https://github.com/hanshoglund/.stash

- [X]  $entrypoint Decide on top-level interface
  - By default recommend *no IO*

  - All data is is in the DSL/Haskell code. For *import formats* (such as Sibelius), we'll generate either 1) Haskell code or 2) TIDL serialized data, which can be automatically converted to Haskell code as per TIDL semantics.

  - Normal GHCI can *evaluate/normalize* music expression and print the result as text using `Show`.

  - *Design:*
    - Each backend has a *main rendering* function taking (`Score Asp1` etc) or similar to some other type A, possibly inside an effect F for failure/logging/parameters etc. `Score Asp1 -> F A`. This can be composed out of intermediate types, e.g. `Work` is used by Lilypond/MusicXML. We have `Score Asp1 -> F Work` and `Work -> F Lilypond`.
    - Overloaded function rendering `Score Asp1` or `Work`, suitable for preview use (e.g. show chords as a simple one-chord score, vocal ranges as a simple two note score with a slide, etc.). Similarly for audio preview. These functions have a role similar to `Show` and should not be used for "real" export (warn about this in the docs!).
    - Finally, some basic utilities for IO: `Score Asp1 -> IO ()` to render a score as a CLI program taking output format, etc. Similarly for the various intermediate types.

- Add WTC C major prelude example
  - Data: https://gist.github.com/hanshoglund/4058cfb08906379fd2da

- $interactive: Interactive editing/preview (see also preview class in $entrypoint).
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

  - New design (Keep It Simple):
    - Single server process (EvalServer) which evaluates expressions and provides a window view, visualizing/audifuingwhat you evaluate.
    - No interactivity/edits in the window, just view.
    - EvalServer has access to GHC+MusicSuiteDependencies+MusicSuiteTheLibrary (either being compiled with music-suite and `hint`, or by running e.g. `cabal exec -- runhaskell examples/part.hs -f ly -o t.ly`).
    - Client/EditorEnvironments (e.g. NeoVim, VSCode etc) send file name + Pos.
      - TODO NeoVim hooks:
        getcurpos() -- returns [_,line,col,...]
        expand('%:p') -- returns abs path of focused file
      - TODO proper transport, for now just use `writefile(["JSONData"], ".inspector")`
    - Server fetches file and inserts an appropriate `defaultMain` (selected expression must be `Inspectable`)
    - Server should try both with and without `:: Music` specialization (poor man's defaulting, to make type signatures
      for simple cases like `c` redundant)
    - The `defaultMain` (see $entrypoint) renders XML, MIDI, ABC, etc
    - The window view is updated (use TeaTime?)
    - Percentage indicator for rendering time (formal streaming support in $entrypoint)?


- Replace Aeson with typed serialization (or just GHC.Generic/Typeable instances)

- Piano/multi-stave/automatic voice separation support.
  Test cases:
    - Render examples/bartok correctly
    - Render Beethoven sonata, e.g. beginning of Waldstein

- [X] Reexport (set, over) from lens in default prelude? (see examples!)

- Split up `Music.Score.X`
  - This module hierarchy exists for historical reasons. Move as per:
    - Phrase traversals: move to Music.Time
    - Export/Import: move to Music.Export, Music.Import
    - Music.Score.ASPECT (e.g. where ASPECT is Pitch etc): merge with Music.ASPECT

- [X] $timeSignatureInLastBar In `fromAspects`, never change time signature in the last bar
  - E.g. in the example `pseq [c,d,e,f,g] |/ 4`, this should render as two 4/4 bars, not as one 4/4 followed by 1/4

- [X] Better syntax for entering pitch/time, maybe using a quasi-quoter
  - See e.g. Mozart example
  - See syntax sketches, also compare Lilypond
  - OTOH the language "just basic Haskell" is maybe more important than simple syntax?

- Interactive shell
  - Move source tree into this repo
  - [X] Bug: crashes on multi-page Lilypond output
  - Repeat works, should look at cache/output dir
  - See $interactive
  - Make feedback loop faster:
    - Optimize "Compiling" (Haskell) phase
    - Run Typesetting/RenderingAudio in parallel
    - "Interpret at selection" a la most Lisp interpreters


- [X] Fix lawless (HasPosition (Score a))
  - Preference: Idea 1!!
  - Idea 1: by adding default/empty position to class (a HasEnvelope in Diagrams)
    - Pros: simple. Same operators (e.g. for juxtaposition) can be used for Scores, Notes, Spans, etc.
      - **Allows makeing |> a monoid with mempty**!!
    - Cons: Disallows the onset/offset lenses Music.Time.Position
      - OTOH these lenses are probably not that necessary
      - Most other combinators (e.g. |>) can manage Nothing too
  - Idea 2: Remove (HasPosition (Score a)) and make a separate class for things which may have empty positions (e.g. Scores)
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
    - Cons:
      - No monad instance
  - Idea 4: Disallow empty scores

- [ ] partitionSimplistic is dead code, use or remove

- [X] Proper scale/chord type supporting all common use-cases
  - [X] Represent functions/modes and chords/scales
  - [X] Making (infinite) octave-repeating scales from pitches/intervals
  - [X] Looking up pitches
  - [X] Transposable instance for chords/scales (pitch-wise)
    - We don't really have a Transposable class, so this means
      defining `HasPitches a b => HasPitches (C a) (C b)`.
      This is possible to define for Chord/Scale by:
        - Placing all pitches relative the origin
        - Run the traversal
        - Recalculate the inverval sequence relative the new/transformed origin and save that
        - Save the new origin
  - [X] Define reflection

- [X] document maths vs. music terminology!
  - Constant source of problems
    - "Transposition" - Musical sense (maths: translation)
    - "Inversion"
      - This is inconsistently used in music
      - Chord "inversions" means rotations or (cyclic) permutations
      - Inverval inversion means negation modulo octave
      - Melody inversion means (point-wise) negation


- [X] Get rid of asNote/asScore/asVoice/asTrack

- [X] Get rid of Prelude.StandardNote et al, use Asp1 (renamed!) instead


- [X] https://github.com/music-suite/music-score/issues/340

- Try alternative quantization algorithms, e.g. fomus or ksquant2
  https://github.com/music-suite/music-score/issues/298

- [X] Replace ucat with (new) rcat

- [X] $minorAspects
  - Definition: the "major" aspects are (pitch, dynamics, articulation, part)
  - Some minor aspects these are being abused in current examples, e.g. freeform text is used to denote pizz/arco
  - The representation of tremolo, slide/gliss, harmonics could be smarter. E.g. we should maybe use
    (Reactive Pitch) or similar instead of context-specific begin/end marks. Note this might require
    countext-bound rewriting as we currently do with dynamics and articulation.
  - Simple playing techniques should be standarized, similarly to what we do with instruments.
    The technique/instrument/rage relations should be availible somewhere.
  - Closing this as actions are tracked under $playingTechniques

- Purge lawless instances, do more property testing
  - It is easy to forget to add tests for each instance. Could we automatically discover instances
    and test them?

- If possible, derive everything in Music.Score.Internal.Instances

- Replace Wrapped/Rewrapped with newtype/via where possible

- Generic derivation of HasPitch etc. Case study: Time.Pattern.Lunga.
  - Note: often `Traversable` is enough (for the plural form of each class)

- Make doc generation work in CI (again)
  - For the CI story, see also https://github.com/hanshoglund/music-suite/pull/21#discussion_r429626368

- [ ] IDE allowing "preview on hover"
  - [X] Basic implementation works now (in separate repository), uses the CLI + Cabal/Nix for invocation
  - [ ] More stability and documentation
    - Normal text editor
    - When moving around in a single file (with/without a 'main' function) any hovered expression should
      trigger a preview (visual/audial) in an editor window, as if the expression had been applied to
      defaultMain (see above).

- [X] Deprecate Track/Placed?
  - It's rarely useful to just 'delay'
  - Score/Event/Aligned/Pattern usually more compelling
  - Placed is currently used in the definition of Pattern, but that could be changed to Aligned (which is a
    strict generalization of Placed)
  - Similarly Track can be replaced by a Score where all durations are 1

- [X] Time.Voice API: Do not mention Meta (it's not used and there's no HasMeta instance).

- Import data from various corpuses
  - musescore.com
  - https://web.mit.edu/music21/doc/about/referenceCorpus.html
  - http://kern.ccarh.org/
  - https://abcnotation.com/search
  - Nice viwer: https://verovio.humdrum.org/

- [X] $reactiveSemantics
  - Briefly: Simultaneous events should not be allowed with Reactive. Semantically:
      type Reactive a = ([Time], Time -> a)

- Maybe: Allow passing Inspectables in defaultMain
  - Pros:
    - No arbitrary hardcoding of (Score ...)
    - Looks nicer in documentation (no need for inspectableToMusic)
  - Cons:
    - No defaulting. Simple expressions like `c` will be ambigous.
      Ideally we would accept any type and default to Music/Score StandardNotate iff there is ambiguity.
      In lieue of proper compiler support, we could do something like the following (for expressions in 'transf'):
        - First try the general form (any Inspectable, no custom type hint). This will succeed if the user provided an expression with an unambigious type.
        - Then try adding (Score StandardNote) and possibly other types in sequence. If they all fail, fail with the message from the original failure ("ambigous type").
        - Could this be done with TH or a compiler plugin?

- Allow overriding the default clef

- [X] For each instrument we want to know:
        - Classification:
            - Type: (i.e. woodwind)
            - Family: (i.e. saxophone)
            - Range: (i.e. tenor)
        - Range (i.e. [c_:e'])
        - Transposition:
            sounding = written .+^ transp
        - Suggested clefs

