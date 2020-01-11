
{- Interpolations! -}

>>> 
>>> interpPitches [c,e,g] [gs',ds'',a''']

<interactive>:1076:33:
    Not in scope: ‘a'''’
    Perhaps you meant one of these:
      ‘a''’ (imported from Music.Prelude),
      ‘ab''’ (imported from Music.Prelude),
      ‘as''’ (imported from Music.Prelude)
>>> interpPitches [c,e,g] [gs',ds'',_8va a'']

<interactive>:1077:1:
    No instance for (Show (Double -> [Pitch]))
      arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
>>> fmap (interpPitches [c,e,g] [gs',ds'',_8va a'']) [x/12|x<-[0..25]]
[[c,e,g],[d,fs,as],[ds,gs,cs'],[f,as,e'],[g,c',gs'],[gs,d',b'],[as,e',d''],[c',f',f''],[cs',g',gs''],[ds',a',c'''],[f',b',ds'''],[fs',cs'',fs'''],[gs',ds'',a'''],[as',f'',c''''],[b',g'',ds''''],[cs'',a'',fs''''],[ds'',b'',as''''],[e'',cs''',cs'''''],[fs'',d''',e'''''],[gs'',e''',g'''''],[a'',fs''',as'''''],[b'',gs''',d''''''],[cs''',as''',f''''''],[d''',c'''',gs''''''],[e''',d'''',b''''''],[fs''',e'''',d''''''']]
>>> _15vb $ fmap (interpPitches [c,e,g] [gs',ds'',_8va a'']) [x/12|x<-[0..25]]
[[c__,e__,g__],[d__,fs__,as__],[ds__,gs__,cs_],[f__,as__,e_],[g__,c_,gs_],[gs__,d_,b_],[as__,e_,d],[c_,f_,f],[cs_,g_,gs],[ds_,a_,c'],[f_,b_,ds'],[fs_,cs,fs'],[gs_,ds,a'],[as_,f,c''],[b_,g,ds''],[cs,a,fs''],[ds,b,as''],[e,cs',cs'''],[fs,d',e'''],[gs,e',g'''],[a,fs',as'''],[b,gs',d''''],[cs',as',f''''],[d',c'',gs''''],[e',d'',b''''],[fs',e'',d''''']]
>>> :o pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,e,g] [gs',ds'',_8va a'']) [x/12|x<-[0..25]]
>>> :au pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,e,g] [gs',ds'',_8va a'']) [x/12|x<-[0..25]]

<interactive>:1081:1:
    No instance for (Display s0) arising from a use of ‘audify’
    The type variable ‘s0’ is ambiguous
    Note: there are several potential instances:
      instance Display (Ambitus Pitch)
        -- Defined at private/studies/Util.hs:753:10
      instance Display Shape -- Defined at private/studies/Util.hs:712:10
      instance Display (Floater StandardNote)
        -- Defined at private/studies/Util.hs:688:10
      ...plus 10 others
    In the expression: audify
    In the expression:
      audify
      $ pseq
        $ map (ppar . map fromPitch'')
          $ _15vb
            $ fmap
                (interpPitches [c, e, g] [gs', ds'', _8va a''])
                [x / 12 | x <- [0 .. 25]]
    In an equation for ‘it’:
        it
          = audify
            $ pseq
              $ map (ppar . map fromPitch'')
                $ _15vb
                  $ fmap
                      (interpPitches [c, e, g] [gs', ds'', _8va a''])
                      [x / 12 | x <- [0 .. 25]]

<interactive>:1081:10:
    No instance for (Monoid s0) arising from a use of ‘pseq’
    The type variable ‘s0’ is ambiguous
    Note: there are several potential instances:
      instance Monoid (Floater a)
        -- Defined at private/studies/Util.hs:663:48
      instance Monoid (Pattern a)
        -- Defined at private/studies/Util.hs:1552:24
      instance Monoid (aeson-0.7.0.6:Data.Aeson.Types.Internal.Parser a)
        -- Defined in ‘aeson-0.7.0.6:Data.Aeson.Types.Internal’
      ...plus 149 others
    In the expression: pseq
    In the second argument of ‘($)’, namely
      ‘pseq
       $ map (ppar . map fromPitch'')
         $ _15vb
           $ fmap
               (interpPitches [c, e, g] [gs', ds'', _8va a''])
               [x / 12 | x <- [0 .. 25]]’
    In the expression:
      audify
      $ pseq
        $ map (ppar . map fromPitch'')
          $ _15vb
            $ fmap
                (interpPitches [c, e, g] [gs', ds'', _8va a''])
                [x / 12 | x <- [0 .. 25]]

<interactive>:1081:31:
    No instance for (IsPitch s0) arising from a use of ‘fromPitch''’
    The type variable ‘s0’ is ambiguous
    Note: there are several potential instances:
      instance (IsPitch a, Reversible a) => IsPitch (Pattern a)
        -- Defined at private/studies/Util.hs:1750:10
      instance IsPitch a => IsPitch (Floater a)
        -- Defined at private/studies/Util.hs:665:10
      instance IsPitch Pitch -- Defined in ‘Music.Pitch.Common.Pitch’
      ...plus 40 others
    In the first argument of ‘map’, namely ‘fromPitch''’
    In the second argument of ‘(.)’, namely ‘map fromPitch''’
    In the first argument of ‘map’, namely ‘(ppar . map fromPitch'')’
>>> :pl pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,e,g] [gs',ds'',_8va a'']) [x/12|x<-[0..25]]
^CExitFailure 1
>>> 
>>> :pl set parts' violins $ pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,e,g] [gs',ds'',_8va a'']) [x/12|x<-[0..25]]
ExitSuccess
>>> 
>>> :pl set parts' violins $ pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,g,e'] [fs,cds']) [x/12|x<-[0..25]]

<interactive>:1086:157:
    Not in scope: ‘cds'’
    Perhaps you meant one of these:
      ‘cs'’ (imported from Music.Prelude),
      ‘ds'’ (imported from Music.Prelude)
>>> :pl set parts' violins $ pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,g,e'] [fs,c',as']) [x/12|x<-[0..25]]
^CExitFailure 1
>>> 
>>> :o set parts' violins $ pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,g,e'] [fs,c',as']) [x/12|x<-[0..25]]
>>> :pl set parts' violins $ pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,g,e'] [fs,c',as']) [x/12|x<-[0..25]]
^CExitFailure 1
>>> :pl set parts' violins $ pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,g,e'] (_15va[fs,c',as'])) [x/12|x<-[0..25]]
^CExitFailure 1
>>> 
>>> :o set parts' violins $ pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,g,e'] (_15va[fs,c',as'])) [x/12|x<-[0..25]]
>>> :pl set parts' violins $ pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,g,e'] (_15va[fs,c',as'])) [x/12|x<-[0..25]]
^CExitFailure 1
>>> 
>>> :pl set parts' violins $ pseq $ map (ppar.map fromPitch'') $ _15vb $ fmap (interpPitches [c,g,e'] (_15va[fs,c',as'])) [x/12|x<-[0..25]]
ExitSuccess
>>> 
