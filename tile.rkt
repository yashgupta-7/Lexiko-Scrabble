#lang racket

;TILE STRUCT
;isBlank (if pts=0 and letter=#\- #t otherwise #f)
;locked (#t #f)
;MuLTIPLIER ('dws 'dls 'tws 'tls)

(provide tile
         tile-letter
         set-tile-letter!
         tile-pts
         set-tile-pts!
         tile-isBlank
         set-tile-isBlank!
         tile-locked
         set-tile-locked!
         tile-multiplier
         set-tile-multiplier!
         )

(struct tile (letter pts isBlank locked multiplier) #:transparent #:mutable)