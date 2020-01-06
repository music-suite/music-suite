%%% WARNING: GENERATED FILE. DO NOT EDIT. %%%

% \version "2.18.2"

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

    % TODO top marigin

}

\header {
  title         = "$(title)"
  composer      = "$(composer)"
  tagline       = ""  % removed
}

\layout {
}

#(set-global-staff-size 10)

