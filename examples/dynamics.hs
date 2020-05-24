
{-# LANGUAGE OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

import Music.Prelude
import Control.Lens (over)


music = id
  $ title "Dynamics"
  $ composer "Anonymous"
  $ (over eras (stretchRelativeOnset 0.5) $ pseq $ zipWith level [fff,ff,_f,mf,mp,_p,pp,ppp] (fmap fromPitch [c..]))
  |> (rest |* 1.5)
  |> (over phrases' (cresc pp mf) $ pseq [c,d,e,f,g,a,b,c'] |/8)
  |> (rest |* 1)
  |> (over phrases' (cresc ff mp) $ pseq [c,d,e,f,g,a,b,c'] |/8)
main = defaultMain music
