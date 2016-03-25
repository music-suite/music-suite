
type Time
type Duration
type Span = Time^2

type Note  a = (Duration, a)
type Event a = (Span, a)

type Voice a = [Note a] -- sum monoid
type Score a = [Event a] -- hull/composition monoid?

type Aligned a = (Time, LocalDuration, a)


type Behavior a = Time -> a
type Reactive a = ([Time], Time -> a)

-- Remove segment
