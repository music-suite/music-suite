
import Music.Prelude
import qualified Music.Time.Note

renderVoice :: Transformable a => Voice a -> [a]
renderVoice = fmap (view stretchee) . view stretcheds

renderVoice2 :: Voice a -> [Music.Time.Note.Note a]
renderVoice2 = renderVoice . fmap (return :: a -> Music.Time.Note.Note a)

-- TODO "pseq"
renderVoice3 :: Voice a -> Score a
renderVoice3 = view score . renderVoice2




renderChord :: Transformable a => Chord a -> [a]
renderChord = fmap (view delayee) . view unchord
  where
    delayee = delayedValue -- TODO rename

renderChord2 :: Chord a -> [Music.Time.Note.Note a]
renderChord2 = renderChord . fmap (return :: a -> Music.Time.Note.Note a)

-- TODO "ppar"
renderChord3 :: Chord a -> Score a
renderChord3 = view score . renderChord2


-- A "directed" pitch type, to check if we have double reverse
data DP = DP (Bool, Pitch)
  deriving (Eq, Ord, Show)
instance IsPitch DP where
  fromPitch a = DP (False, fromPitch a)
instance Transformable DP where
  transform _ = id
instance Reversible DP where
  rev (DP (b,p)) = DP (not b,p)



-- instance Transformable Pitch where
  -- transform _ = id
instance Transformable Interval where
  transform _ = id

instance Reversible Pitch where
  rev = id
instance Reversible Interval where
  rev = id
-- instance Reversible Int where
  -- rev = id
-- instance Reversible Integer where
  -- rev = id
-- instance Reversible Double where
  -- rev = id
-- instance Reversible Rational where
  -- rev = id
instance Reversible Float where
  rev = id

