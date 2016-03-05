
; Usage:
;   runhaskell examples/scheme.hs examples/test1.scm
(let ((*base-note-value* 4) 
      (identity-function (lambda (x) x)))

  ;(rcat
  ;  (move 3 (times 4 (scat (stretch first-note-duration c) (stretch second-note-duration d) eb)))
  ;  (move 3 (times 4 (scat (stretch first-note-duration c) (stretch second-note-duration d) eb))))
  
  (stretch *base-note-value*
  (scat
    (compress 8 (apply scat (list c d e)))
    (compress 16 (scat c d e))
    (compress 8 (scat c d e f))
    (up (identity-function m3)
      (scat
        (compress 16 (scat c d e))
        (compress 8 (scat c d e))
        (compress 16 (scat c d e (pcat f bb))))))))


