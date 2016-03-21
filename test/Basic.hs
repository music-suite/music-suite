
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

import Music.Prelude

-- music-suite/test/legacy-music-files/articulation_all_accents.music
articulation_all_accents :: Music
articulation_all_accents =
  accent (scat [c..g]|/8)
      </>
  marcato (scat [c..g]|/8)


-- music-suite/test/legacy-music-files/articulation_all_separations.music
articulation_all_separations :: Music
articulation_all_separations =
  legato (scat [c..g]|/8)
      </>
  staccato (scat [c..g]|/8)
      </>
  portato (scat [c..g]|/8)
      </>
  tenuto (scat [c..g]|/8)
      </>
  separated (scat [c..g]|/8)
      </>
  spiccato (scat [c..g]|/8)


-- music-suite/test/legacy-music-files/articulation_legato.music
articulation_legato :: Music
articulation_legato =
  legato (scat [c..g]|/8)


-- music-suite/test/legacy-music-files/articulation_portato.music
articulation_portato :: Music
articulation_portato =
  portato (scat [c..g]|/8)


-- music-suite/test/legacy-music-files/articulation_staccato.music
articulation_staccato :: Music
articulation_staccato =
  staccato (scat [c..g]|/8)


-- music-suite/test/legacy-music-files/decl_style1.music
-- decl_style1 =
--
--   data Foo = Foo | Bar
--
--   scale Foo = scat [c,d,e,f,g,a,g,f]|/8
--   scale Bar = scale Foo
--
--   triad a = a <> up _M3 a <> up _P5 a
--
--   example = up _P8 (scale Foo) </> (triad c)|/2 |> (triad g_)|/2



-- music-suite/test/legacy-music-files/dynamics_constant.music
dynamics_constant :: Music
dynamics_constant =
  scat $ zipWith level [fff,ff,_f,mf,mp,_p,pp,ppp] [c..]


-- music-suite/test/legacy-music-files/melody_chords.music
melody_chords :: Music
melody_chords =
  let
      scale = scat [c,d,e,f,g,a,g,f] |/ 8
      triad a = a <> up _M3 a <> up _P5 a
  in up _P8 scale </> (triad c)|/2 |> (triad g_)|/2


-- music-suite/test/legacy-music-files/meta_annotations.music
meta_annotations :: Music
meta_annotations =
  showAnnotations $ annotate "First note" c |> d |> annotate "Last note" d


-- music-suite/test/legacy-music-files/meta_clef1.music
meta_clef1 :: Music
meta_clef1 =
  let
      part1 = clef f $ staccato $ scat [c_,g_,c,g_]
      part2 = clef c $ staccato $ scat [ab_,eb,d,a]
      part3 = clef g $ staccato $ accentLast $ scat [g,fs,e,d]
  in compress 8 $ part1 |> part2 |> part3


-- music-suite/test/legacy-music-files/meta_composer.music
meta_composer :: Music
meta_composer =
  composer "Anonymous" $ scat [c,d,e,c]


-- music-suite/test/legacy-music-files/meta_time_signature.music
meta_time_signature :: Music
meta_time_signature =
  compress 4 $ timeSignature (4/4) (scat [c,d,e,c,d,e,f,d,g,d]) |> timeSignature (3/4) (scat [a,g,f,g,f,e])


-- music-suite/test/legacy-music-files/meta_title.music
meta_title :: Music
meta_title =
  title "Piece" $ scat [c,d,e,c]


-- music-suite/test/legacy-music-files/misc_counterpoint.music
misc_counterpoint :: Music
misc_counterpoint =
  let
      subj = scat $ scat [ [c],       [d],        [f],          [e]           ]
      cs1  = scat $ scat [ [g,f,e,g], [f,a,g,d'], [c',b,c',d'], [e',g',f',e'] ]
  in compress 4 cs1 </> subj


-- music-suite/test/legacy-music-files/octaves.music
octaves :: Music
octaves =
  c__ |> c_ |> c |> c' |> c''


-- music-suite/test/legacy-music-files/overlay_chords.music
overlay_chords :: Music
overlay_chords =

  pcat [c,e,g] |> pcat [d,f,a] |> pcat [e,g,b] |> pcat [c,e,g]


-- music-suite/test/legacy-music-files/overlay_voices.music
overlay_voices :: Music
overlay_voices =
  scat [c,d,e,c] <> scat [e,f,g,e] <> scat [g,a,b,g]


-- music-suite/test/legacy-music-files/pitch_inv.music
pitch_inv :: Music
pitch_inv =

  (scat [c..g]|*(2/5))
      </>
  (invertPitches c $ scat [c..g]|*(2/5))
      </>
  (invertPitches e $ scat [c..g]|*(2/5))


-- music-suite/test/legacy-music-files/sharpen.music
sharpen' :: Music
sharpen' =
  sharpen c
      </>
  (sharpen . sharpen) c


-- music-suite/test/legacy-music-files/simple_figure.music
simple_figure :: Music
simple_figure =
  (c |> d |> e |> c |> d|*2 |> d|*2)|/16


-- music-suite/test/legacy-music-files/simple_start_later.music
simple_start_later :: Music
simple_start_later =
  up _P8 . compress 2 . delay 3 $ c


-- music-suite/test/legacy-music-files/single_note.music
single_note :: Music
single_note =
  c


-- music-suite/test/legacy-music-files/special_gliss.music
special_gliss :: Music
special_gliss =
  glissando $ scat [c,d]|/2


-- music-suite/test/legacy-music-files/special_harmonics.music
special_harmonics :: Music
special_harmonics =
  (harmonic 1 $ c|/2)
      </>
  (harmonic 2 $ c|/2)
      </>
  (harmonic 3 $ c|/2)


-- music-suite/test/legacy-music-files/special_text.music
special_text :: Music
special_text =
  text "pizz." $ c|/2


-- music-suite/test/legacy-music-files/special_tremolo.music
special_tremolo :: Music
special_tremolo =
  tremolo 2 $ times 2 $ (c |> d)|/2


-- music-suite/test/legacy-music-files/stretch_single_note1.music
stretch_single_note1 :: Music
stretch_single_note1 =
  stretch (1/2) c


-- music-suite/test/legacy-music-files/stretch_single_note2.music
stretch_single_note2 :: Music
stretch_single_note2 =
  stretch (1/2) c


-- music-suite/test/legacy-music-files/stretch_single_note3.music
stretch_single_note3 :: Music
stretch_single_note3 =
  stretch (4+1/2) c


-- music-suite/test/legacy-music-files/times.music
times' :: Music
times' =
  let
      melody = legato $ scat [c,d,e,cs,ds,es]|/16
  in times 4 $ melody


-- music-suite/test/legacy-music-files/track_single.music
track_single :: Music
track_single =
  let
      x = [ (0, c)^.placed, (1, d)^.placed, (2, e)^.placed ]^.track
      y = join $ [ (0, x)^.placed,
                  (1.5,  up _P5 x)^.placed,
                  (3.25, up _P8 x)^.placed ]^.track

      trackToScore d = view score . map (view event . (\(t,x) -> (t >-> d,x)) . (view $ from placed)) . view placeds

  in trackToScore (1/8) y


-- music-suite/test/legacy-music-files/voice_single.music
-- voice_single =
--   let
--       x = [ (1, c)^.note,
--             (1, d)^.note,
--             (1, f)^.note,
--             (1, e)^.note ]^.voice
--
--       y = join $ [ (1, x)^.note,
--                    (0.5, up _P5 x)^.note,
--                    (4, up _P8 x)^.note ]^.voice
--
--   in stretch (1/8) $ view (re singleMVoice) . fmap Just $ y


main = return ()
