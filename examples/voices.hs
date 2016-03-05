
import Music.Prelude
import qualified Music.Score

subj :: Voice (Maybe Pitch)
subj = mconcat [c',ab,db',e,f,g,ab,bb,c']

type Chorale = [Voice (Maybe Pitch)]

renderVoice :: Voice (Maybe Pitch) -> Score StandardNote
renderVoice = fmap fromPitch'' . removeRests . renderAlignedVoice . aligned 0 0

renderChorale :: Chorale -> Score StandardNote
renderChorale = catSep . fmap renderVoice

catSep = pcat . zipWith (set parts') (divide 100 mempty)

music = renderChorale [subj]