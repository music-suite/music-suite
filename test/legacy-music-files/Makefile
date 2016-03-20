
run: golden instances

golden:
	GHC_PACKAGE_PATH=`music-util package-path` ghc -threaded -o golden-tests tests/tests.hs
	time ./golden-tests -j 4 +RTS -N4 -RTS
	rm golden-tests

instances:
	GHC_PACKAGE_PATH=`music-util package-path` ghc -threaded -o instance-tests tests/instances.hs
	time ./instance-tests -j 4 +RTS -N4 -RTS
	rm instance-tests

clean:	
	rm -f tests/golden/*.mid
	rm -f tests/golden/*.xml
	rm -f tests/golden/*.ly
	rm tests/originals_ref.sha
	rm tests/generated_ref.sha

generate: generate-checksums

generate-checksums: generate-originals
	shasum tests/*.music  | shasum > tests/originals_ref.sha
	shasum tests/golden/* | shasum > tests/generated_ref.sha

generate-originals:
	music2midi	-o tests/golden/articulation_all_accents.mid		tests/articulation_all_accents.music
	music2ly	-o tests/golden/articulation_all_accents.ly		tests/articulation_all_accents.music
	music2musicxml	-o tests/golden/articulation_all_accents.xml		tests/articulation_all_accents.music
	music2midi	-o tests/golden/articulation_all_separations.mid    	tests/articulation_all_separations.music
	music2ly	-o tests/golden/articulation_all_separations.ly		tests/articulation_all_separations.music
	music2musicxml	-o tests/golden/articulation_all_separations.xml	tests/articulation_all_separations.music
	music2midi	-o tests/golden/articulation_legato.mid     		tests/articulation_legato.music
	music2ly	-o tests/golden/articulation_legato.ly			tests/articulation_legato.music
	music2musicxml	-o tests/golden/articulation_legato.xml			tests/articulation_legato.music
	music2midi	-o tests/golden/articulation_portato.mid    		tests/articulation_portato.music
	music2ly	-o tests/golden/articulation_portato.ly			tests/articulation_portato.music
	music2musicxml	-o tests/golden/articulation_portato.xml		tests/articulation_portato.music
	music2midi	-o tests/golden/articulation_staccato.mid   		tests/articulation_staccato.music
	music2ly	-o tests/golden/articulation_staccato.ly		tests/articulation_staccato.music
	music2musicxml	-o tests/golden/articulation_staccato.xml		tests/articulation_staccato.music

	music2midi	-o tests/golden/decl_style1.mid		tests/decl_style1.music
	music2ly	-o tests/golden/decl_style1.ly		tests/decl_style1.music
	music2musicxml	-o tests/golden/decl_style1.xml		tests/decl_style1.music

	music2midi	-o tests/golden/dynamics_constant.mid       		tests/dynamics_constant.music
	music2ly	-o tests/golden/dynamics_constant.ly			tests/dynamics_constant.music
	music2musicxml	-o tests/golden/dynamics_constant.xml			tests/dynamics_constant.music
	music2midi	-o tests/golden/melody_chords.mid   			tests/melody_chords.music
	music2ly	-o tests/golden/melody_chords.ly			tests/melody_chords.music
	music2musicxml	-o tests/golden/melody_chords.xml			tests/melody_chords.music
	music2midi	-o tests/golden/meta_annotations.mid			tests/meta_annotations.music
	music2ly	-o tests/golden/meta_annotations.ly			tests/meta_annotations.music
	music2musicxml	-o tests/golden/meta_annotations.xml			tests/meta_annotations.music
	music2midi	-o tests/golden/meta_title.mid				tests/meta_title.music
	music2ly	-o tests/golden/meta_title.ly				tests/meta_title.music
	music2musicxml	-o tests/golden/meta_title.xml				tests/meta_title.music
	music2midi	-o tests/golden/meta_clef1.mid				tests/meta_clef1.music
	music2ly	-o tests/golden/meta_clef1.ly				tests/meta_clef1.music
	music2musicxml	-o tests/golden/meta_clef1.xml				tests/meta_clef1.music
	music2midi	-o tests/golden/meta_composer.mid			tests/meta_composer.music
	music2ly	-o tests/golden/meta_composer.ly			tests/meta_composer.music
	music2musicxml	-o tests/golden/meta_composer.xml			tests/meta_composer.music
	music2midi	-o tests/golden/meta_time_signature.mid			tests/meta_time_signature.music
	music2ly	-o tests/golden/meta_time_signature.ly			tests/meta_time_signature.music
	music2musicxml	-o tests/golden/meta_time_signature.xml			tests/meta_time_signature.music
	music2midi	-o tests/golden/misc_counterpoint.mid			tests/misc_counterpoint.music
	music2ly	-o tests/golden/misc_counterpoint.ly			tests/misc_counterpoint.music
	music2musicxml	-o tests/golden/misc_counterpoint.xml			tests/misc_counterpoint.music
	music2midi	-o tests/golden/octaves.mid				tests/octaves.music
	music2ly	-o tests/golden/octaves.ly				tests/octaves.music
	music2musicxml	-o tests/golden/octaves.xml				tests/octaves.music
	music2midi	-o tests/golden/overlay_chords.mid			tests/overlay_chords.music
	music2ly	-o tests/golden/overlay_chords.ly			tests/overlay_chords.music
	music2musicxml	-o tests/golden/overlay_chords.xml			tests/overlay_chords.music
	music2midi	-o tests/golden/overlay_voices.mid			tests/overlay_voices.music
	music2ly	-o tests/golden/overlay_voices.ly			tests/overlay_voices.music
	music2musicxml	-o tests/golden/overlay_voices.xml			tests/overlay_voices.music
	music2midi	-o tests/golden/pitch_inv.mid				tests/pitch_inv.music
	music2ly	-o tests/golden/pitch_inv.ly				tests/pitch_inv.music
	music2musicxml	-o tests/golden/pitch_inv.xml				tests/pitch_inv.music
	music2midi	-o tests/golden/sharpen.mid				tests/sharpen.music
	music2ly	-o tests/golden/sharpen.ly				tests/sharpen.music
	music2musicxml	-o tests/golden/sharpen.xml				tests/sharpen.music
	music2midi	-o tests/golden/simple_figure.mid			tests/simple_figure.music
	music2ly	-o tests/golden/simple_figure.ly			tests/simple_figure.music
	music2musicxml	-o tests/golden/simple_figure.xml			tests/simple_figure.music
	music2midi	-o tests/golden/simple_start_later.mid			tests/simple_start_later.music
	music2ly	-o tests/golden/simple_start_later.ly			tests/simple_start_later.music
	music2musicxml	-o tests/golden/simple_start_later.xml			tests/simple_start_later.music
	music2midi	-o tests/golden/single_note.mid				tests/single_note.music
	music2ly	-o tests/golden/single_note.ly				tests/single_note.music
	music2musicxml	-o tests/golden/single_note.xml				tests/single_note.music
	music2midi	-o tests/golden/special_gliss.mid			tests/special_gliss.music
	music2ly	-o tests/golden/special_gliss.ly			tests/special_gliss.music
	music2musicxml	-o tests/golden/special_gliss.xml			tests/special_gliss.music
	music2midi	-o tests/golden/special_harmonics.mid			tests/special_harmonics.music
	music2ly	-o tests/golden/special_harmonics.ly			tests/special_harmonics.music
	music2musicxml	-o tests/golden/special_harmonics.xml			tests/special_harmonics.music
	music2midi	-o tests/golden/special_text.mid			tests/special_text.music
	music2ly	-o tests/golden/special_text.ly				tests/special_text.music
	music2musicxml	-o tests/golden/special_text.xml			tests/special_text.music
	music2midi	-o tests/golden/special_tremolo.mid			tests/special_tremolo.music
	music2ly	-o tests/golden/special_tremolo.ly			tests/special_tremolo.music
	music2musicxml	-o tests/golden/special_tremolo.xml			tests/special_tremolo.music
	music2midi	-o tests/golden/stretch_single_note1.mid		tests/stretch_single_note1.music
	music2ly	-o tests/golden/stretch_single_note1.ly			tests/stretch_single_note1.music
	music2musicxml	-o tests/golden/stretch_single_note1.xml		tests/stretch_single_note1.music
	music2midi	-o tests/golden/stretch_single_note2.mid		tests/stretch_single_note2.music
	music2ly	-o tests/golden/stretch_single_note2.ly			tests/stretch_single_note2.music
	music2musicxml	-o tests/golden/stretch_single_note2.xml		tests/stretch_single_note2.music
	music2midi	-o tests/golden/stretch_single_note3.mid		tests/stretch_single_note3.music
	music2ly	-o tests/golden/stretch_single_note3.ly			tests/stretch_single_note3.music
	music2musicxml	-o tests/golden/stretch_single_note3.xml		tests/stretch_single_note3.music
	music2midi	-o tests/golden/times.mid				tests/times.music
	music2ly	-o tests/golden/times.ly				tests/times.music
	music2musicxml	-o tests/golden/times.xml				tests/times.music
	music2midi	-o tests/golden/track_single.mid			tests/track_single.music
	music2ly	-o tests/golden/track_single.ly				tests/track_single.music
	music2musicxml	-o tests/golden/track_single.xml			tests/track_single.music
	music2midi	-o tests/golden/voice_single.mid			tests/voice_single.music
	music2ly	-o tests/golden/voice_single.ly				tests/voice_single.music
	music2musicxml	-o tests/golden/voice_single.xml			tests/voice_single.music

open:
	rm -f *.pdf
	./tests/open-lilypond tests/current/articulation_all_accents.ly
	# open -a "Sibelius 7" tests/current/articulation_all_accents.xml
	./tests/open-lilypond tests/current/decl_style1.ly
	# open -a "Sibelius 7" tests/current/decl_style1.xml
	./tests/open-lilypond tests/current/articulation_all_separations.ly
	# open -a "Sibelius 7" tests/current/articulation_all_separations.xml
	./tests/open-lilypond tests/current/articulation_legato.ly
	# open -a "Sibelius 7" tests/current/articulation_legato.xml
	./tests/open-lilypond tests/current/articulation_portato.ly
	# open -a "Sibelius 7" tests/current/articulation_portato.xml
	./tests/open-lilypond tests/current/articulation_staccato.ly
	# open -a "Sibelius 7" tests/current/articulation_staccato.xml
	./tests/open-lilypond tests/current/dynamics_constant.ly
	# open -a "Sibelius 7" tests/current/dynamics_constant.xml
	./tests/open-lilypond tests/current/melody_chords.ly
	# open -a "Sibelius 7" tests/current/melody_chords.xml
	./tests/open-lilypond tests/current/meta_annotations.ly
	# open -a "Sibelius 7" tests/current/meta_annotations.xml
	./tests/open-lilypond tests/current/meta_title.ly
	# open -a "Sibelius 7" tests/current/meta_title.xml
	./tests/open-lilypond tests/current/meta_clef1.ly
	# open -a "Sibelius 7" tests/current/meta_clef1.xml
	./tests/open-lilypond tests/current/meta_composer.ly
	# open -a "Sibelius 7" tests/current/meta_composer.xml
	./tests/open-lilypond tests/current/meta_time_signature.ly
	# open -a "Sibelius 7" tests/current/meta_time_signature.xml
	./tests/open-lilypond tests/current/misc_counterpoint.ly
	# open -a "Sibelius 7" tests/current/misc_counterpoint.xml
	./tests/open-lilypond tests/current/octaves.ly
	# open -a "Sibelius 7" tests/current/octaves.xml
	./tests/open-lilypond tests/current/overlay_chords.ly
	# open -a "Sibelius 7" tests/current/overlay_chords.xml
	./tests/open-lilypond tests/current/overlay_voices.ly
	# open -a "Sibelius 7" tests/current/overlay_voices.xml
	./tests/open-lilypond tests/current/pitch_inv.ly
	# open -a "Sibelius 7" tests/current/pitch_inv.xml
	./tests/open-lilypond tests/current/sharpen.ly
	# open -a "Sibelius 7" tests/current/sharpen.xml
	./tests/open-lilypond tests/current/simple_figure.ly
	# open -a "Sibelius 7" tests/current/simple_figure.xml
	./tests/open-lilypond tests/current/simple_start_later.ly
	# open -a "Sibelius 7" tests/current/simple_start_later.xml
	./tests/open-lilypond tests/current/single_note.ly
	# open -a "Sibelius 7" tests/current/single_note.xml
	./tests/open-lilypond tests/current/special_gliss.ly
	# open -a "Sibelius 7" tests/current/special_gliss.xml
	./tests/open-lilypond tests/current/special_harmonics.ly
	# open -a "Sibelius 7" tests/current/special_harmonics.xml
	./tests/open-lilypond tests/current/special_text.ly
	# open -a "Sibelius 7" tests/current/special_text.xml
	./tests/open-lilypond tests/current/special_tremolo.ly
	# open -a "Sibelius 7" tests/current/special_tremolo.xml
	./tests/open-lilypond tests/current/stretch_single_note1.ly
	# open -a "Sibelius 7" tests/current/stretch_single_note1.xml
	./tests/open-lilypond tests/current/stretch_single_note2.ly
	# open -a "Sibelius 7" tests/current/stretch_single_note2.xml
	./tests/open-lilypond tests/current/stretch_single_note3.ly
	# open -a "Sibelius 7" tests/current/stretch_single_note3.xml
	./tests/open-lilypond tests/current/times.ly
	# open -a "Sibelius 7" tests/current/times.xml
	./tests/open-lilypond tests/current/track_single.ly
	# open -a "Sibelius 7" tests/current/track_single.xml
	./tests/open-lilypond tests/current/voice_single.ly
	# open -a "Sibelius 7" tests/current/voice_single.xml
	open *.pdf



