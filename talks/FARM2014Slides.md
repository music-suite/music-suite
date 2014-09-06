% <a style="color:">Music Suite</a>
% <a style="color:crimson">music-suite.github.io</a>
% <a style="color:blue">Hans Höglund</a> <br/><br/> <img src="farm2014/images/logo3.svg"/>

## Music Suite

- What is music?

- Fundamentally it is <a style="color:blue">*sound*</a>, but we want a more structured representation.

## Music Suite

- What is music?

- Fundamentally it is <a style="color:blue">*sound*</a>, but we want a more structured representation.

- A Haskell library for ... of music

-|-|-|-|-|-
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|*Composition*  |&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| <a style="color:crimson">*a*</a>     | &nbsp;➝&nbsp; |  <a style="color:blue">*Music*</a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|*Manipulation* |&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| <a style="color:blue">*Music*</a> | &nbsp;➝&nbsp; | <a style="color:blue">*Music*</a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|*Analysis*     |&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| <a style="color:blue">*Music*</a> | &nbsp;➝&nbsp; | <a style="color:crimson">*b*</a>


## type Music = <a style="color:crimson">?</a>

No closed definition of music(s)

- Music making software <a style="color:blue">*necessarily*</a> impose restrictions on the music.

- Usually those of Western classical/popular music.
  <br/><a style="font-size:80%">Diatonic/chromatic scale, 3/4 and 4/4 time signatures, etc.</a>

- Difficult to work with other kinds of music.

## type Music = <a style="color:crimson">?</a>

No closed definition of music(s)

- Music making software <a style="color:blue">*necessarily*</a> impose restrictions on the music.

- Usually those of Western classical/popular music.
  <br/><a style="font-size:80%">Diatonic/chromatic scale, 3/4 and 4/4 time signatures, etc.</a>

- Difficult to work with other kinds of music.

But there <a style="color:crimson">*are*</a> many common elements in music!

- How can we formalize these common qualities?

## type Music = <a style="color:crimson">?</a>

Musical <a style="color:blue">*theory*</a>

- Scales, chords, rhythm, harmony, counterpoint

## type Music = <a style="color:crimson">?</a>

Musical <a style="color:blue">*theory*</a>

- Scales, chords, rhythm, harmony, counterpoint

Musical <a style="color:blue">*aspects*</a>

- Pitch, time, dynamics, timbre (instrument/articulation), (space..)

Musical cultures treat (and rank) aspects differently!

<!--
- Western classical places emphasis on pitch/harmony.
- Jazz places more emphasis on time/rhythm.
-->

## type Music = <a style="color:crimson">?</a>

Time is special

- Music take place in time!
- All other aspects are <a style="color:blue">*organized in time*</a>

## type Music = <a style="color:crimson">?</a>

Time is special

- Music take place in time!
- All other aspects are <a style="color:blue">*organized in time*</a>

In other words, aspects may depend on time, but not the other way around.

## type Music = <a style="color:crimson">?</a>

Time is special

- Music take place in time!
- All other aspects are <a style="color:blue">*organized in time*</a>

In other words, aspects may depend on time, but not the other way around.

<!--
Aspects tend to be largely orthogonal (for example pitch and dynamics).
-->
We provide multiple versions of all aspects <a style="color:crimson">*except*</a> time.

<!--
- Reduces complexity as time is so fundamental.
-->


<!--
## type Music = <a style="color:crimson">?</a>

The suite provides two types of types

- <a style="color:blue">*Time types*</a> of kind `* ➝ *`
- <a style="color:crimson">*Aspects types*</a> of kind `*`

Examples:

- <a style="color:blue">*∀a . (Time ⨯ a)*</a>
- <a style="color:blue">*∀a. Time ➝ a* </a>
- <a style="color:crimson">*(Pitch ⨯ Dynamic)*</a>
- <a style="color:black">*Time ➝ (Pitch ⨯ Dynamic)*</a>
- <a style="color:black">*(Pitch ⨯ Time ➝ Dynamic)*</a>
-->

## Library layout

### Aspects

- music-pitch
- music-dynamics
- music-articulation
- etc.

### Special

- music-score (really <a style="color:crimson">*time*</a>)
- music-preludes
- music-suite

## Structure vs. Freedom

- Why not simply use the largest type?

    - type *Pitch* = *Frequency* (*Hz*)
    - type *Dynamics* = *Amplitude* (*dB*)
    etc.

- Restricting the space in which we are working empowers the type system

- A more high-level concept can be <a style="color:blue">*performance-dependent*</a>, for example pitch vs. frequency

- For each aspect: Start out with a free affine space and <a style="color:crimson">*add*</a> structure

<!--
## Time types

(Work pioneered by FRP, Reactive, Euterpea...)
  
-->

## Time types

Compare graphics (2D or 3D)!

- View time as an 1D <a style="color:blue">affine space</a>.
    - Separate *Time* (points) and *Duration* (vectors)

- Time and duration form an <a style="color:crimson">affine/vector</a> space pair.

## Time types

An affine transformation is simply *(Product~Duration~ ⨯ Sum~Time~)*

- This type is called *Span*
- Isomorphic to *Time^2^* or *(Time ⨯ Duration)*

Generalises time transformations `delay` and `stretch`

    class Transformable a where
        transform :: Span -> a -> a

## Time types

-|-|-
*type Note    a* | = | *(Duration, a)*
*type Delayed a* | = | *(Time, a)*
*type Placed  a* | = | *(Span, a)*
&nbsp;||
*type Voice   a* | = | *[Note a]*
*type Score   a* | = | *[Voice a]*
*type Track   a* | = | *[Placed a]*
&nbsp;||
*type Behavior a* | = | *Time -> a*


<!--
  Old        | New        | Semantics
  -----------|------------|--------------------
  Time | Time | Point in time space
  Duration | Duration | Vector in time space
  Span | Span |  `Time^2`
             | Rest       | Container with just duration (like Duration but `* -> *`)
  Stretched  | Note       | Container with duration and value
  Delayed    | Delayed    | Container with offset and value
  Note       | Placed     | Containe with offset, duration and value
             | Future     | Value starting at a point in time
             | Past       | Value ending at a point in  time
             | Nominal    | Value whose duration is ignored
             | Graces     | Value with optional nominal pre- and post events
  Voice      | Voice      | Sequential composition of notes
  Chord      | Chord      | Parallel composition of notes
  Score      | Track      | Set of values with arbitrary span (possibly overlapping)
  Track      |            | 
        | Score     | Parallel composition of voices of chords.
-->
## Aspects types: Pitch...

Defined in `music-pitch`

Main hierarchies are `Music.Pitch.Absolute` `Music.Pitch.Common`
    
- Former defines `Frequency`

- Latter defines `Pitch` and `Interval`
  <br /><a style="font-size:80%">Defined as *(Diatonic ⨯ Chromatic)*</a>

Pitches and intervals form an affine/vector space pair


## Aspects types: ...etc

- Dynamics
- Parts
- Articulation
- Space

### More exotic

- Specific playing techniques (string tremolo, harmonics etc)
- Tremolo and trills
- Percussion

## Accessing aspects

<!--
- Edward Kmetts's `lens` gives us the vocabulary:

Name |&nbsp;| Meaning
-|-|-
Lens' s a      |&nbsp;| (s ➝ a) ➝ (s ➝ a ➝ s) 
Prism' s a     |&nbsp;| (a ➝ s) ➝ (s ➝ Maybe a)
Iso' s a       |&nbsp;| (s ➝ a) (a ➝ s)
Traversal' s a |&nbsp;| Applicative f ⟹ (a ➝ f a) ➝ s ➝ (f s)

Name |&nbsp;| Meaning
-|-|-
Lens s t a b      |&nbsp;| (s ➝ a) ➝ (s ➝ b ➝ t) 
Prism s t a b     |&nbsp;| (b ➝ t) ➝ (s ➝ Either t a)
Iso s t a b       |&nbsp;| (s ➝ a) (b ➝ t)
Traversal s t a b |&nbsp;| Applicative f ⟹ (a ➝ f b) ➝ s ➝ (f t)
-->

Based on `lens` vocabulary:

- `HasPitches` provides a traversal `pitches`
  <br/><a style="font-size:80%">Scores, voices, chords etc</a>

- `HasPitch` provides a lens `pitch` 
  <br/><a style="font-size:80%">Actual pitch types, notes etc</a>

- Whenever `pitch` exists it is the same as `pitches`

Similar for all other aspects.

Allows polymorphic updating of aspects.

## Literals

Pitches

- c, cs, d, ds...

Dynamic values

- m, mf, sfz...

In the style of `OverloadedStrings`.

## Meta-information

A system to annotate all time types with arbitrary values.

- Uses `Typeable` wrappers, same as Diagrams' styles.

- Backends can ignore meta-data they don't understand.

Used for most things that does not affect how the music sounds:

- Key and time signatures, barlines, repeats, rehearsal marks etc.


<!--
## Design principles

- Extensible
    - Only make some *core* assumptions (time is special, other aspect *fetch onto* time)
    - User can change both events and time containers

- Comprehensive, not complete
    - Impossible to include *all* music theories!
    - CMT included
    - Easily extendible

- Use the "latest"
    - MPTCs, TypeFamilies, ContraintKinds
  - lens, vector-space
-->

## Backends

### Import

- MIDI
- <a style="color:crimson">Sibelius</a>

### Export

- MIDI
- Lilypond
- MusicXML
- <a style="color:crimson">ABC Notation</a>
- <a style="color:crimson">SuperCollider</a>

## Challenges

- Develop high-level theory while staying general

- Difficult error messages

    - Simplified version needed

- Consolidate with other libraries

    - Common time/space/linear algebra core?

<!--
- Junk in the backends (as the internal representation is more general), should be moved "higher" up in the libraries
-->


<!--
### Wanted

    OSC
    ABCNotation
    csound-expression
-->

