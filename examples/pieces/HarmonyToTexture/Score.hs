
module Main where

import Music.Prelude
import qualified Data.List
import Util

{-
Infinately many ways to orchestrate a chord

How to categorize:

  - Long pitches
    - 1 pitch per instruments
    - 2 or more pitches per instruments (keyboard, double-stops)
    - Repeating patterns (various levels of symmetry)
    - Phased or random patterns
    - Tremolo between 2 chords (in one instrument or several)

    
    ...
-}
arp, arpUD
  :: (Monoid s, Transformable s, HasPosition s, Semigroup s, Reversible s) =>
     [s] -> s
arp ps = stretchTo 1 $ pseq ps
arpUD x = arp x |> rev (arp x) -- TODO palindrome without repeat

elab, elabUD
  :: (Monoid s, HasPosition s, Reversible s, Semigroup s) => [s] -> s
elab ps = stretchTo 3 $ pseq [arp ps, arp (invertChord 2 ps), arp (invertChord 1 ps)]
elabUD ps = stretchTo 3 $ pseq [arpUD ps, arpUD (invertChord 2 ps), arpUD (invertChord 1 ps)]

music = asScore $ sppar chords
arpChords = compress 4 $ pseq $ fmap arp chords




















{-
chords <- fmap getHomophonic $ captureSibelius
-}
chords = [[c,e,g,b,d'],[db,fb,ab,c',eb'],[g_,b_,d,fs,a],[ab_,as_,eb,g,bb],[c,e,g,b,d'],[db,fb,ab,c',eb'],[g_,b_,d,fs,a],[ab_,as_,eb,g,bb],[a_,cb,e,gs,b],[cs,es,gs,bs,ds'],[d,f,a,cs',e'],[gb_,bs_,ds,g,as],[a_,cb,e,gs,b],[c,e,g,b,d'],[ab_,cb,eb,g,bb],[g_,b_,d,fs,a]]

-- chords =
--   [ [f_, a_ ,d  ]^*2
--   , [fs_,a_, cs ]
--   , [g_ ,a_, b_ ]^*1.5
--   , [fs_,a_, cs ]
-- 
--   , [fs_,as_,d ]
--   , [g_,bb_,eb  ]
--   , [gs_,b_,e   ]
-- 
--   ]

getHomophonic :: Transformable a => Score a -> [Chord a]
getHomophonic s = toListOf traverse $ simultaneous $ fmap (\x -> [x])s


instance Reversible a => Reversible (Score a) where
  rev = fmap rev . stretch (-1)
instance Reversible Pitch where
  rev = id

  