#lang racket
(require "board.rkt"
         "bag.rkt"
         "tile.rkt"
         "utility-functions-and-macros.rkt"
         "word-score.rkt")

(require racket/vector)
(require 2htdp/abstraction)




(provide ai)
(define (ai CurrentBoard isFirstTurn)
  
  (define seeds 
    (vector-append*
     (for/vector ([row (board-tiles CurrentBoard)]
                 [y '(14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)])
      (for/vector ([Tile row]
                   [x '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)]
                   #:when (tile-isBlank Tile))
        (cond [(and (< y 14)
                    (not (tile-isBlank (get-square (board-tiles CurrentBoard) x (+ y 1))))) (cons x y)]
              [(and (> y 0)
                    (not (tile-isBlank (get-square (board-tiles CurrentBoard) x (- y 1))))) (cons x y)]
              [(and (< x 14)
                    (not (tile-isBlank (get-square (board-tiles CurrentBoard) (+ x 1) y)))) (cons x y)]
              [(and (> x 0)
                    (not (tile-isBlank (get-square (board-tiles CurrentBoard) (- x 1) y)))) (cons x y)])))
     #()))
  (set! seeds
        (vector-filter
         (λ (x) (not (void? x)))
         seeds))
  (displayln seeds)

  (define tileSlots
    (vector-append
     (vector-append*
      (for/vector ([pos-pair seeds])
        (vector-append*
         (for/vector ([lo (build-list 7 values)])
           (for/vector ([hi (build-list (- 7 lo) values)])
             ;;Build Horizontal Tile slot
             (vector-append
              (build-left CurrentBoard (- (car pos-pair) 1) (cdr pos-pair) lo #())
              (vector pos-pair)
              (build-right CurrentBoard (+ 1 (car pos-pair)) (cdr pos-pair) hi #()))))
         #()))
      #())

     (vector-append*
      (for/vector ([pos-pair seeds])
        (vector-append*
         (for/vector ([lo (build-list 7 values)])
           (for/vector ([hi (build-list (- 7 lo) values)])
             ;;Build Vertical Tile slot
             (vector-append
              (build-up CurrentBoard (car pos-pair) (+ (cdr pos-pair) 1) hi #())
              (vector pos-pair)
              (build-down CurrentBoard (car pos-pair) (- 1 (cdr pos-pair)) lo #()))))
         #()))
      #())))

  (set! tileSlots (vector-remove-duplicates tileSlots))

;  (define tilestoplace
;    (vector-argmax
;     (λ (x) (car (faster-best-word-score CurrentBoard x tray)))
;     tileSlots));;;best-word-score returns , given a vector of tile positions , a pair of score and the word
  (define totalscore 0)
  (define stop? #f)
  (define vb -1)
  (define tilestoplace 1)
  (for ([sl tileSlots]
        #:break (and stop? (> vb 0)))
    (let ([fbws (faster-best-word-score CurrentBoard sl ai-tray)])
      (cond [(>= (string-length (cdr fbws)) 2) (begin
                                          (set! stop? #t)
                                          (set! tilestoplace sl)
                                          ;(display-board (box CurrentBoard))
                                          (placetiles CurrentBoard tilestoplace (cdr fbws))
                                          (display-board (box CurrentBoard))
                                          (set! vb
                                                (let ((AreWordsValid (validateWords CurrentBoard isFirstTurn)))
                                                  (cond
                                                    [(negative? AreWordsValid) (begin
                                                                                 (displayln "Invalid Words Present.")
                                                                                 (removeTempTiles CurrentBoard ai-tray)
                                                                                 -1)]
                                                    [else
                                                     (begin
                                                       (LockTempTiles CurrentBoard ai-tray)
                                                       (set-board-columnLock! CurrentBoard -1)
                                                       (set-board-rowLock! CurrentBoard -1)
                                                       AreWordsValid)]))
                                                )
                                          )])))
  vb)
  
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (build-left CurrentBoard x y lo acc)
  (cond [(and
          (zero? lo)
          (not (negative? x))
          (tile-isBlank (get-square (board-tiles CurrentBoard) x y)))
         acc]
        [(and
          (zero? lo)
          (not (negative? x))
          (not (tile-isBlank (get-square (board-tiles CurrentBoard) x y))))
         (vector-append (vector (cons x y)) acc)]
        [(and
          (not (negative? x))
          (tile-isBlank (get-square (board-tiles CurrentBoard) x y)))
         (build-left CurrentBoard (- x 1) y (- lo 1) (vector-append (vector (cons x y)) acc))]
        [(and
          (not (negative? x))
          (not (tile-isBlank (get-square (board-tiles CurrentBoard) x y))))
         (build-left CurrentBoard (- x 1) y lo (vector-append (vector (cons x y)) acc))]
        [else acc]))

(define (build-right CurrentBoard x y hi acc)
  (cond [(and
          (zero? hi)
          (< x 15)
          (tile-isBlank (get-square (board-tiles CurrentBoard) x y)))
         acc]
        [(and
          (zero? hi)
          (< x 15)
          (not (tile-isBlank (get-square (board-tiles CurrentBoard) x y))))
         (vector-append acc (vector (cons x y)))]
        [(and
          (< x 15)
          (tile-isBlank (get-square (board-tiles CurrentBoard) x y)))
         (build-right CurrentBoard (+ x 1) y (- hi 1) (vector-append acc (vector (cons x y))))]
        [(and
          (< x 15)
          (not (tile-isBlank (get-square (board-tiles CurrentBoard) x y))))
         (build-right CurrentBoard (+ x 1) y hi (vector-append acc (vector (cons x y))))]
        [else acc]))

(define (build-down CurrentBoard x y lo acc)
  (cond [(and
          (zero? lo)
          (not (negative? y))
          (tile-isBlank (get-square (board-tiles CurrentBoard) x y)))
         acc]
        [(and
          (zero? lo)
          (not (negative? y))
          (not (tile-isBlank (get-square (board-tiles CurrentBoard) x y))))
         (vector-append acc (vector (cons x y)))]
        [(and (not (negative? y))
              (tile-isBlank (get-square (board-tiles CurrentBoard) x y)))
         (build-down CurrentBoard x (- y 1) (- lo 1) (vector-append acc (vector (cons x y))))]
        [(and
          (not (negative? y))
          (not (tile-isBlank (get-square (board-tiles CurrentBoard) x y))))
         (build-down CurrentBoard x (- y 1) lo (vector-append acc (vector (cons x y))))]
        [else acc]))

(define (build-up CurrentBoard x y hi acc)
  (cond [(and
          (zero? hi)
          (< y 15)
          (tile-isBlank (get-square (board-tiles CurrentBoard) x y)))
         acc]
        [(and
          (zero? hi)
          (< y 15)
          (not (tile-isBlank (get-square (board-tiles CurrentBoard) x y))))
         (vector-append (vector (cons x y)) acc)]
        [(and
          (< y 15)
          (tile-isBlank (get-square (board-tiles CurrentBoard) x y)))
         (build-up CurrentBoard x (+ 1 y) (- hi 1) (vector-append (vector (cons x y)) acc))]
        [(and
          (< y 15)
          (not (tile-isBlank (get-square (board-tiles CurrentBoard) x y))))
         (build-up CurrentBoard x (+ 1 y) hi (vector-append (vector (cons x y)) acc))]
        [else acc]))
        
;;;;;;;;;;;;;;;
(provide placetiles)
(define (placetiles CurrentBoard tilestoplace str)
  (define i 0)
  (for ([pos tilestoplace])
    (let* ([x (car pos)]
           [y (cdr pos)]
           [til (get-square(board-tiles CurrentBoard) x y)])
      (cond [(not (tile-locked til))
              (begin
                (vector-set!
                 (vector-ref (board-tiles CurrentBoard) (- 14 y))
                 x
                 (tile
                  (~a (string-ref str i))
                  (score (~a (string-ref str i)))
                  #f
                  #f ;;;;;;;;no need to validate board....already done in best-word-score
                  (tile-multiplier (get-square (board-tiles CurrentBoard) x y))))
                (set! i (+ i 1)))]
            [else (set! i (+ i 1))]))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(provide best-word-score)
(define (best-word-score CurrentBoard v tray)
  (displayln v);;; v is a vector of coordinates.........also use validate board
  (displayln tray)
  (define (g pos)
    (tile-letter
     (get-square (board-tiles CurrentBoard) (car pos) (cdr pos))))
  (define emt
    (vector-count (λ (x) (equal? (g x) #\-)) v))
  (define to-put
    (choose-and-arrange tray emt))
  
  (define words
    (map
     (λ (x) (let ((j 0))
              (apply
             string-append
               (for/list ([i v])
              (begin
              ;(displayln i)
              ;(displayln j)
              (if (equal? (g i) #\-)
                  (begin
                    (set! j (+ j 1))
                    (list-ref x (- j 1)))
                  (g i)))))))
     to-put))
  (let ((m-word (argmax
                 (λ (x)
                   (if (is-valid? x)
                            (score x)
                            0)
                   ;(score x)
                   ) words)))
    (displayln
     (cons
     (if (is-valid? m-word)
           (score m-word)
              0)
          m-word)) 
    (cons
     (if (is-valid? m-word)
           (score m-word)
              0)
          m-word)))

(define (faster-best-word-score CurrentBoard v tray) 
  (displayln v);;; v is a vector of coordinates.........also use validate board
  (displayln tray)
  
  (define (g pos)
    (tile-letter
     (get-square (board-tiles CurrentBoard) (car pos) (cdr pos))))
 (define word-length (vector-length v))
 (define words
   (vector-filter
     (λ (Word)
   (begin
     (define word (string-downcase Word))
     (define match? #t)
     (define tray2 (vector->list tray))
   (if (= (string-length word) word-length)
    (for ([pos v]
         [letter word])
     (if (equal? (g pos) #\-)
         (if (member (~a letter) tray2)
             (set! tray2 (remove (~a letter) tray2))
             (set! match? #f))
         (cond [(not (equal? (g pos) (~a letter))) (set! match? #f)])))
    (set! match? #f))
   match?))
     Dictionary2))
  (define best-word
    (if (zero? (vector-length words)) "_"
        (vector-argmax (λ (x) (score x)) words)))
  (displayln (cons (score best-word) best-word))
  (cons (score best-word) (string-downcase best-word)))
   
 (define (display-board b-CurrentBoard)
  (define tiles (board-tiles (unbox b-CurrentBoard)))
  (for ([row tiles])
    (begin (for ([square row])
      (begin (display (tile-letter square))
             ;(display (tile-isBlank square))
             (display " ")
             )) (newline)))) 
   
   
  

;;;;;;;;;;;
(provide faster-best-word-score-2)
(define (faster-best-word-score-2 CurrentBoard v tray) 
  (displayln v);;; v is a vector of coordinates.........also use validate board
  (displayln tray)
  
  (define (g pos)
    (tile-letter
     (get-square (board-tiles CurrentBoard) (car pos) (cdr pos))))
 (define word-length (vector-length v))
 (define match? #f)
 (define w "_")
   (for ([Word Dictionary]
         #:break match?)
     (begin
     (define word (string-downcase Word))
     (set! match? #t)
     (define tray2 (vector->list tray))
   (if (= (string-length word) word-length)
    (for ([pos v]
         [letter word])
     (if (equal? (g pos) #\-)
         (if (member (~a letter) tray2)
             (set! tray2 (remove (~a letter) tray2))
             (set! match? #f))
         (cond [(not (equal? (g pos) (~a letter))) (set! match? #f)])))
    (set! match? #f))
   (cond [match? (set! w Word)])
   ))
  (displayln (cons (score w) w))
  (cons (score w) (string-downcase w)))
