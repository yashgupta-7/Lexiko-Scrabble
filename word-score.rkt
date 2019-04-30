#lang racket
(require 2htdp/batch-io)
;Given a word (string), compute the scrabble score of that word
;Point value is as follows:
;;(1 point)-A, E, I, O, U, L, N, S, T, R
;;(2 points)-D, G
;;(3 points)-B, C, M, P
;;(4 points)-F, H, V, W, Y
;;(5 points)-K
;;(8 points)- J, X
;;(10 points)-Q, Z

;Double Letter Score -light blue cells, they will double the value of the tile placed on that square.
;Triple Letter Score  -dark blue cell, they will triple the value of the tile placed on that square.
;Double Word Score    -light red,the entire value of the word will be doubled.
;Triple Word Score    -dark red square,will triple the word score.
;One Single Use       -When using the extra point squares on the board, they can only be used one time.
;                      If a player places a word here, it cannot be used as a multiplier by placing another word on the same square.

(define (read-hist-word-list file-path #:pick? [choice 'word])
  (call-with-input-file file-path
    (lambda (fin)
      (for/vector ([word-count (in-lines fin)])
        (let ([wc-split (string-split word-count #:trim? #t)])
          (match choice
            ['word   (car wc-split)]
            ['counts (string->number (cadr wc-split))]
            ['both   (cons (car wc-split)
                           (string->number (cadr wc-split)))]))))))


(provide Dictionary)
(define Dictionary (read-hist-word-list "media/scrabblewords_usage.txt"))

(provide is-valid?)
(define (is-valid? word)
  (if (vector-member (string-upcase word) Dictionary) #t #f))

(provide score)
(define (score word)
  (for/sum ([c (string->list (string-downcase word))])
    (match c
      [(or #\a #\e #\i #\o #\u #\l #\n #\r #\s #\t)  1]
      [(or #\g #\d)                                  2]
      [(or #\b #\c #\m #\p)                          3]
      [(or #\f #\h #\v #\w #\y)                      4]
      [(or #\k)                                      5]
      [(or #\j #\x)                                  8]
      [(or #\q #\z)                                 10]
      [_ 0])))

(provide Dictionary2)
(define Dictionary2 (vector-take
                     (vector-filter
                      (λ (x) (> (string-length x) 1) )
                        (read-hist-word-list "media/google-books-common-words.txt"))
                     15000))

(provide Dictionary3)
(define Dictionary3 
                     (vector-filter
                      (λ (x) (< (string-length x) 7) )
                        (read-hist-word-list "media/scrabblewords_usage.txt"))
                     )
