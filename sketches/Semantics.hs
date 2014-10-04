
----------
data Chromatic
data Diatonic
type Interval = (Diatonic, Chromatic)
-- Vector with basis {d2,A1} {(1,0),(0,1)}

----------
data Time
data Duration
type Span = (Time, Duration) = (Time, Time) = (Duration, Time)
-- Vector with basis (assuming Time/Duration repr.) {delay 1,stretch 1} {(1,0),(0,1)}

type Future a = (Min Time, a)
type Past a = (Max Time, a)
type Reactive a = (a, [Future a]) = ([Past a], a)
type Behavior a = Time -> a
type Spline a = Time -> a, where {t : Time, 0 <= t <= 1}
type Segment a = Voice (Spline a) = Note (Spline a)

type Rest a  = Duration
type Note a  = (Duration, a)
type Chord a = ?
-- These are not placed, i.e in parallel composition there is no pickup/putdown

type Delayed a = (Time, a) -- call offset/pickup etc.?
type Event a = (Span, a)

type Voice a = [Note a]
type Track a = [Event a]
