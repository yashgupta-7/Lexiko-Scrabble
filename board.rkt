#lang racket
(require "tile.rkt"
         "word-score.rkt"
         "utility-functions-and-macros.rkt"
         "bag.rkt")

(require racket/vector
         ;srfi/43
         )

(provide board
         board-tiles
         board-columnLock
         board-rowLock
         get-square
         set-board-columnLock!
         set-board-rowLock!)

(struct board (tiles columnLock rowLock) #:transparent #:mutable)

(define squares
  #( #(#\t #\n #\n #\b #\n #\n #\n #\t #\n #\n #\n #\b #\n #\n #\t)
     #(#\n #\d #\n #\n #\n #\c #\n #\n #\n #\c #\n #\n #\n #\d #\n)
     #(#\n #\n #\d #\n #\n #\n #\b #\n #\b #\n #\n #\n #\d #\n #\n)
     #(#\b #\n #\n #\d #\n #\n #\n #\b #\n #\n #\n #\d #\n #\n #\b)
     #(#\n #\n #\n #\n #\d #\n #\n #\n #\n #\n #\d #\n #\n #\n #\n)
     #(#\n #\c #\n #\n #\n #\c #\n #\n #\n #\c #\n #\n #\n #\c #\n)
     #(#\n #\n #\b #\n #\n #\n #\b #\n #\b #\n #\n #\n #\b #\n #\n)
     #(#\t #\n #\n #\n #\n #\n #\n #\d #\n #\n #\n #\n #\n #\n #\t)
     #(#\n #\n #\b #\n #\n #\n #\b #\n #\b #\n #\n #\n #\b #\n #\n)
     #(#\n #\c #\n #\n #\n #\c #\n #\n #\n #\c #\n #\n #\n #\c #\n)
     #(#\n #\n #\n #\n #\d #\n #\n #\n #\n #\n #\d #\n #\n #\n #\n)
     #(#\b #\n #\n #\d #\n #\n #\n #\b #\n #\n #\n #\d #\n #\n #\b)
     #(#\n #\n #\d #\n #\n #\n #\b #\n #\b #\n #\n #\n #\d #\n #\n)
     #(#\n #\d #\n #\n #\n #\c #\n #\n #\n #\c #\n #\n #\n #\d #\n)
     #(#\t #\n #\n #\b #\n #\n #\n #\t #\n #\n #\n #\b #\n #\n #\t) ))

;;;;
(provide get-square)
(define (get-square grid x y)   ;; grid size is 15*15
  (cond [(or (> x 14) (< x 0)) (error "out of bounds")]
        [(or (> y 14) (< y 0)) (error "out of bounds")]
        [else (vector-ref (vector-ref grid (- 14 y)) x)]))
  

;;;;;;;;;
(define init-tiles
  (for/vector ([row squares])
    (for/vector ([square row])
      (match square
        [#\n (tile #\- 0 #t #f 'norm)];normal
        [#\d (tile #\- 0 #t #f 'dws)];double word score
        [#\t (tile #\- 0 #t #f 'tws)];triple word score
        [#\b (tile #\- 0 #t #f 'dls)];double letter score
        [#\c (tile #\- 0 #t #f 'tls)]))));triple letter score(define newboard

(provide newboard)
(define newboard (board init-tiles -1 -1))

;;;;BOARD VALIDATION ROUTINE;;;;;;;;;;;;

(provide ValidateBoard)
(define (ValidateBoard CurrentBoard isFirstTurn tray)
  (define inACol #t)
  (define inARow #t)
  
  ;;;;;;;;;;;collect all tentative tiles
  
  (define inPlay
    (vector-append*
     (for/vector ([row (board-tiles CurrentBoard)]
                  [y '(14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)])
       (for/vector ([Tile row]
                    [x '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)]
                    #:when (and
                            (not (tile-locked Tile))
                            (not (equal? #\- (tile-letter Tile)))))
         (list Tile (list x y))))
     #()))

  

  ;#VALIDATION STEP ONE: There must be at least one tile played, if not return -1...............
  
  (cond [(equal? #() inPlay) (begin
                               (displayln "Play requires atleast one Tile")
                               -1)]

        ;#VALIDATION STEP TWO: Tiles must be played on a straight line, if not remove them and return -1.

        [(let ((col (first (second (vector-ref inPlay 0))))
               (row (second (second (vector-ref inPlay 0)))))
           (begin
             (for ([Tile-pos inPlay])
               (cond [(not (equal? col (first (second Tile-pos)))) (set! inACol #f)]
                     [(not (equal? row (second (second Tile-pos)))) (set! inARow #f)]))
             (and (not inARow) (not inACol))))
         (begin
           (displayln "All tiles should be in a line.")
           (removeTempTiles CurrentBoard tray)
           -1)]

        ;;#VALIDATION STEP THREE: If isFirstTurn, then one tile must be on START_POSITION,
        ;;if not remove them and return -1.
               
        [(and
          isFirstTurn
          (equal?
           (vector-filter
            (lambda (x) (equal? (second x) (list 7 7)))
            inPlay)
           #()))
         (begin
           (displayln "Game should start from the centre Tile.")
           (removeTempTiles CurrentBoard tray)
           -1)]
                        
        ; #VALIDATION STEP FOUR: Word created is unbroken,if not remove them and return -1.
         
        [(let* ((left (first
                       (second
                        (vector-argmin (lambda (x) (first (second x))) inPlay))))
                (right (first
                        (second
                         (vector-argmax (lambda (x) (first (second x))) inPlay))))
                (top (second
                      (second
                       (vector-argmax (lambda (x) (second (second x))) inPlay))))
                (bottom (second
                         (second
                          (vector-argmax (lambda (x) (second (second x))) inPlay))))
                (unbroken #t))
           (begin
             (cond
               [inACol
                (for ([y (build-list (+ bottom 1 (- top)) values)])
                  (cond [(tile-isBlank
                          (get-square (board-tiles CurrentBoard) left (+ top y)))
                         (set! unbroken #f)]))]
               [inARow
                (for ([x (build-list (+ right 1 (- left)) values)])
                  (cond [(tile-isBlank
                          (get-square (board-tiles CurrentBoard) (+ left x) top))
                         (set! unbroken #f)])
                  )])
             (not unbroken)))
         (begin
           (displayln "Word is broken.")
           (removeTempTiles CurrentBoard tray)
           -1)]

        ;VALIDATION STEP FIVE: Words on board are all valid,  if not remove them and return -1.
        ;VALIDATION COMPLETE : Lock Tiles and evaluate score
                             
        [else
         (let ((AreWordsValid (validateWords CurrentBoard isFirstTurn)))
           (cond
             [(negative? AreWordsValid) (begin
                                          (displayln "Invalid Words Present.")
                                          (removeTempTiles CurrentBoard tray)
                                          -1)]
             [else
              (begin
                (LockTempTiles CurrentBoard tray)
                (set-board-columnLock! CurrentBoard -1)
                (set-board-rowLock! CurrentBoard -1)
                AreWordsValid)]))]

        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide ai-tray)
(define ai-tray
    (begin
      (shuffle-bag b-bag)
      (for/vector ([i (list 0 1 2 3 4 5 6)])
        (string-downcase
         (grab b-bag)))))      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide removeTempTiles)
(define (removeTempTiles CurrentBoard tray)
  (begin
    (set-board-columnLock! CurrentBoard -1)
    (set-board-rowLock! CurrentBoard -1)
    (vector-append*
     (for/vector ([row (board-tiles CurrentBoard)]
                  [y '(14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)])
       (for/vector ([Tile row]
                    [x '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)]
                    #:when (and (not (tile-locked Tile)) (not (equal? #\- (tile-letter Tile)))))
         (let ((lett (tile-letter Tile)))
           (begin
             (set-tile-letter! Tile #\-)
             (set-tile-pts! Tile 0)
             (set-tile-isBlank! Tile #t)
             
             lett))))  #())))
  

;;;;
  
(provide LockTempTiles)
(define (LockTempTiles CurrentBoard tray)
  (vector-append*
   (for/vector ([row (board-tiles CurrentBoard)]
                [y '(14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)])
     (for/vector ([Tile row]
                  [x '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)]
                  #:when (and (not (tile-locked Tile)) (not (equal? #\- (tile-letter Tile)))))
       (begin
         (set-tile-locked! Tile #t)
         (vector-set! tray
                      (vector-index tray (tile-letter Tile))
                      #\@)
         (tile-letter Tile))))
   #()))

;;;;
(provide words-in)
(define (words-in vec)
  (define current
    (vector ))
  (define vec-acc
    (vector ))
  (for [(Tile vec)]
    (cond
      [(not (equal? (tile-letter Tile) #\-))
       (set! current (vector-append current (vector Tile)))]
      [(equal? (tile-letter Tile) #\-)
       (begin
         (set!
          vec-acc
          (vector-append vec-acc (vector current)))
         (set! current (vector )))]))
  (begin
    (set!
     vec-acc
     (vector-append vec-acc (vector current)))
    (vector-filter-not (λ (x) (equal? x (vector ))) vec-acc)
    ))
  
(provide make-word)
(define (make-word vec);;;takes a vector of tiles and returns a word
  (apply
   string-append
   (vector->list
    (vector-map (λ (x) (tile-letter x)) vec))))



(provide validateWords)
(define (validateWords CurrentBoard isFirstTurn) 

  (define score-r 0)
  (define score-c 0)
  (define all-unlocked-r #f)
  (define all-unlocked-c #f)
  (define stop-c? #f)
  (define stop-r? #f)
  (begin ;;;;;;;;;;; or ............

    ;;VALIDATION ONE : CHECK ALONG ROWS AND FOR CROSSWORDS

    (for [(row (board-tiles CurrentBoard))
          #:break stop-r?]
      (for ([word (words-in row)]
            #:final (not (is-valid? (make-word word))))
        (begin
          (displayln (make-word word))
          (cond [(not (is-valid? (make-word word)))
                 ;(string-append (make-word word) "is not a valid word in Scrabble Dictionary")
                 (begin
                   (set! score-r -1)
                   (set! stop-r? #t)
                   (displayln (make-word word)))]
                [(equal? (vector ) (vector-filter (λ (x) (tile-locked x)) word))
                 (if isFirstTurn
                       (set! score-r (+ score-r (get-score word)))
                       ;(string-append (make-word word)  "placement not correct."))
                   (cond [(> (vector-length word) 1)
                          (set! all-unlocked-r #t)
;                          (begin
;                            (set! all-unlocked (+ all-unlocked 1))
;                            (displayln 2))
                          ]))]
                [#t
                 (set! score-r (+ score-r (get-score word)))]))))

    ;;VALIDATION TWO : CHECK ALONG COLUMNS AND FOR CROSSWORDS

    (for [(col-num #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
          #:break stop-c?]
      (let ((column (vector-map
                     (λ (x) (vector-ref x col-num))
                     (board-tiles CurrentBoard))))
        (for ([word (words-in column)]
              #:final (not (is-valid? (make-word word))))
          (begin
            (displayln (make-word word))
            (cond [(not (is-valid? (make-word word)))
                   ;(string-append (make-word word) "is not a valid word in Scrabble Dictionary")
                   (begin
                   (set! score-c -1)
                   (set! stop-c? #t)
                   (displayln (make-word word)))]
                  [(equal? (vector ) (vector-filter (λ (x) (tile-locked x)) word))
                   (if isFirstTurn
                       (set! score-c (+ score-c (get-score word)))
                       ;(string-append (make-word word)  "placement not correct."))
                   (cond [(> (vector-length word) 1)
                          (set! all-unlocked-c #t)
;                          (begin
;                            (set! all-unlocked (+ all-unlocked 1))
;                            (displayln 2))
                          ]))]
                  [#t (set! score-c (+ score-c (get-score word)))])))))

    (cond [(= (- 1) score-c) -1]
          [(= (- 1) score-r) -1]
          [all-unlocked-c -1]
          [all-unlocked-r -1]
          [else (+ score-c score-r)])
  ))



;;;;
(provide get-score) ;;;;provides score from a vector of tiles
(define (get-score word-vec)
  
      (define score-multiplier
    (cond [(vector-findf
            (λ (x) (equal? 'dws (tile-multiplier x)))
            word-vec)
           2]
          [(vector-findf
            (λ (x) (equal? 'tws (tile-multiplier x)))
            word-vec)
           3]
          [else 1]))

  (define s (for/sum ([c word-vec])
              (match (cons (tile-letter c) (cond [(equal? 'dls (tile-multiplier c)) 2]
                                                 [(equal? 'tls (tile-multiplier c)) 3]
                                                 [else 1]))

                [(or (cons "a" m) (cons "e" m) (cons "i" m)
                     (cons "o" m) (cons "u" m) (cons "l" m)
                     (cons "n" m) (cons "r" m) (cons "s" m) (cons "t" m))       (* m 1)]
                [(or (cons "g" m) (cons "d" m))                                 (* m 2)]
                [(or (cons "b" m) (cons "c" m) (cons "m" m)
                     (cons "p" m))                                              (* m 3)]
                [(or (cons "f" m) (cons "h" m) (cons "v" m)
                     (cons "w" m) (cons "y" m))                                 (* m 4)]
                [(or (cons "k" m))                                              (* m 5)]
                [(or (cons "j" m) (cons "x" m))                                 (* m 8)]
                [(or (cons "q" m) (cons "z" m))                                (* m 10)]
                [_                                                                    0])))

  (if (= 1 (vector-length word-vec)) 0
      (* score-multiplier s)))

;;;;
(provide validateWords-2)
(define (validateWords-2 CurrentBoard isFirstTurn) 

  (define score-r 0)
  (define score-c 0)
  (define all-unlocked-r #f)
  (define all-unlocked-c #f)
  (define stop-c? #f)
  (define stop-r? #f)
  (begin ;;;;;;;;;;; or ............

    ;;VALIDATION ONE : CHECK ALONG ROWS AND FOR CROSSWORDS

    (for [(row (board-tiles CurrentBoard))
          #:break stop-r?]
      (for ([word (words-in row)]
            #:final (not (is-valid? (make-word word))))
        (begin
          (displayln (make-word word))
          (cond [(not (is-valid? (make-word word)))
                 ;(string-append (make-word word) "is not a valid word in Scrabble Dictionary")
                 (begin
                   (set! score-r -1)
                   (set! stop-r? #t)
                   (displayln (make-word word)))]
                [(equal? (vector ) (vector-filter (λ (x) (tile-locked x)) word))
                 (if isFirstTurn
                       (begin
                         ;(displayln score-r)
                       (set! score-r (+ score-r (get-score word))))
                       ;(string-append (make-word word)  "placement not correct."))
                   (cond [(> (vector-length word) 1)
                          (set! all-unlocked-r #t)
;                          (begin
;                            (set! all-unlocked (+ all-unlocked 1))
;                            (displayln 2))
                          ]))]
                [#t
                 (begin
                   (set! score-r (+ score-r (get-score word)))
                   ;(displayln score-r)
                   ;(displayln word)
                   )]))))

    ;;VALIDATION TWO : CHECK ALONG COLUMNS AND FOR CROSSWORDS

    (for [(col-num #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
          #:break stop-c?]
      (let ((column (vector-map
                     (λ (x) (vector-ref x col-num))
                     (board-tiles CurrentBoard))))
        (for ([word (words-in column)]
              #:final (not (is-valid? (make-word word))))
          (begin
            (displayln (make-word word))
            (cond [(not (is-valid? (make-word word)))
                   ;(string-append (make-word word) "is not a valid word in Scrabble Dictionary")
                   (begin
                   (set! score-c -1)
                   (set! stop-c? #t)
                   (displayln (make-word word)))]
                  [(equal? (vector ) (vector-filter (λ (x) (tile-locked x)) word))
                   (if isFirstTurn
                       (begin
                         ;(displayln score-c)
                       (set! score-c (+ score-c (get-score word))))
                       ;(string-append (make-word word)  "placement not correct."))
                   (cond [(> (vector-length word) 1)
                          (set! all-unlocked-c #t)
;                          (begin
;                            (set! all-unlocked (+ all-unlocked 1))
;                            (displayln 2))
                          ]))]
                  [#t
                   (begin
                   (set! score-c (+ score-c (get-score word)))
                   ;(displayln score-c)
                   ;(displayln word)
                   )])))))

    (cond [(= (- 1) score-c) -1]
          [(= (- 1) score-r) -1]
          [all-unlocked-c -1]
          [all-unlocked-r -1]
          [else (+ score-c score-r)])
  ))


  
          












 
  
