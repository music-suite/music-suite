
module Music.Time.Event
  ( -- * Event type
    Event,

    -- * Construction
    event,
    eventee,
    spanEvent,
  )
where

import Control.Applicative
import Control.Comonad
import Control.Lens hiding
  ( (<|),
    Indexable,
    Level,
    index,
    inside,
    parts,
    reversed,
    transform,
    (|>),
  )
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.Functor.Couple
import Data.String
import Data.Typeable
import Music.Dynamics.Literal
import Music.Pitch.Literal
import Music.Time.Internal.Util (dependingOn)
import Music.Time.Juxtapose

-- |
-- A 'Event' is a value transformed to appear in some 'Span'. Like 'Span', it is an instance of 'Transformable'.
newtype Event a = Event {getEvent :: Span `Couple` a}
  deriving
    ( Eq,
      Ord,
      Typeable,
      Foldable,
      Applicative,
      Monad,
      Comonad,
      Traversable,
      Functor,
      Num,
      Fractional,
      Floating,
      Real,
      RealFrac
    )

instance Transformable (Event a) where
  transform t = over eventSpan (transform t)

instance HasDuration (Event a) where
  _duration = _duration . view eventSpan

instance HasPosition (Event a) where
  _era = Just . view eventSpan

instance HasPosition1 (Event a) where
  _era1 = view eventSpan

instance IsString a => IsString (Event a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Event a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Event a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Event a) where
  fromDynamics = pure . fromDynamics

instance (Show a) => Show (Event a) where
  show x = show (x ^. from event) ++ "^.event"

instance ToJSON a => ToJSON (Event a) where
  -- TODO meta
  toJSON a = JSON.object [("span", toJSON s), ("value", toJSON x)]
    where
      (s, x) = a ^. from event

instance FromJSON a => FromJSON (Event a) where
  parseJSON (JSON.Object x) = liftA2 (\x y -> (x, y) ^. event) era value
    where
      era = x JSON..: "span" -- TODO should change this name
      value = x JSON..: "value"
  parseJSON _ = empty

-- | View a event as a pair of the original value and the transformation (and vice versa).
event :: Iso (Span, a) (Span, b) (Event a) (Event b)
event = iso (Event . Couple) (getCouple . getEvent)

eventSpan :: Lens' (Event a) Span
eventSpan = from event . _1

-- | View the value in the event.
eventee :: (Transformable a, Transformable b) => Lens (Event a) (Event b) a b
eventee = from event `dependingOn` transformed

-- | Event as a span with a trivial value.
spanEvent :: Iso' Span (Event ())
spanEvent = iso (\s -> (s, ()) ^. event) (^. era)
