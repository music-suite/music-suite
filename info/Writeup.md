
## Good ideas (original and stolen)

- Putting the representations ADTs/parsing/printing in separate libraries
- The overloaded terminology (pitches, intervals, dynamics)
- The dynamic meta-attributes
- Including both continuos and discrete components (but not interactivity)
- Using the vector-space library
- Using the lens library
- Using standard type-classes 
    - MonadPlus, Monad, Applicative, Monoid, Semigroup, Comonad
    - Possibly interesting: Bind, Zip, Indexed..., 
- Going so far in the separation of logical vs. presentational
    - I.e. (aspect/TFs vs meta/Typeable)

## Possibly good

- Enforcing the separation of aspects vs. structure
    - Great for modularity
    - Forces us to be explicit in the constraints put on things like pitch.
    - Nice 3-way separation: structure (fixed/classes), aspects (polymorphic) and meta (dynamic)

- Using flat structures (i.e no hierarchy part -> voice -> subvoice -> phrase etc)
    - We will need these representations along with flat ones
    - `Isos` and `Prisms` could help here
    - Still not known: how to pick good representations for the semantic types
    - Any work by Conal et. al we could reuse here?

## Bad


(All now adressed!)

- Maybe we went slightly *too far* in the logical/presentational separation
- It is absolutely right for barlines, fermatas, tempo etc but maybe not quite
  right for part/voice/subvoice separation. See above.
- The time types need an overhaul (ongoing).
    - Time, Duration, Span
    - We should probably replace the many TCs by Transformable + HasPosition
    - Better implementation of Behavior

## Misc

- Rule of thumb for package separation

    - Details on part/pitch/dyn and etc goes into the relevant supporting package
        - Ditto for specific representations (12-tone theory, ragas, scales etc)
    - Timed structure, meta information etc goes into `score`
    - Consolidating instances, examples etc goes into `preludes`
    - Operations that can apply directly to *music* (score, voice, note etc) goes into
      `score`, while operations that apply to a single *aspect value* (reflect pitch,
      divide part) etc goes into the supporting package (and can be applied inside
      containers using a lens/tranversal composition).

## New stuff

- "High-level" stuff
    - Quantization, form/structure, rhythms, instrumentation, counterpoint, harmony
    - Which package?
   
