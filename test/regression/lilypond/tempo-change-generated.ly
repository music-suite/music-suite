%%% WARNING: GENERATED FILE. DO NOT EDIT. %%%

\version "2.20.0"

\paper {
    #(define fonts
       (make-pango-font-tree "Times New Roman"
                             "Arial"
                             "Andale Mono"
                             (/ staff-height pt 20)))


    % First page vs. subsequent pages
    indent          = 0.3\in
    short-indent    = 0\in
    % horizontal-shift

    % paper-width
    % left-margin
    % right-margin

    % Horizontal space taken up by music
    line-width      = 210\mm - 2.0 * 0.4\in

    page-breaking = #ly:one-page-breaking
}

\header {
  title         = ""
  composer      = ""
  tagline       = ""  % removed
}

\layout {
}

#(set-global-staff-size 10)

<<
    \new StaffGroup <<
                    >>
    \new StaffGroup <<
                    >>
    \new StaffGroup <<
                    >>
    \new GrandStaff <<
                        \new Staff {   \set Staff.instrumentName = "Piano I" \set Staff.shortInstrumentName = "Piano I" \clef treble {   \time 4/4 c'1-\mf
                                                                                                                                     } {   \time 3/4 d'2.~
                                                                                                                                       } {   d'4 e'2~
                                                                                                                                         } e'2
                                   }
                    >>
    \new StaffGroup <<
                    >>
    \new StaffGroup <<
                    >>
>>