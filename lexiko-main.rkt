#lang racket
(require racket/gui)
(require racket/draw)
(require "board.rkt"
         "bag.rkt"
         "tile.rkt"
         "utility-functions-and-macros.rkt"
         "player.rkt"
         "word-score.rkt"
         ;"draw-board-2.rkt"
         )

(provide b-board-in-play)
(define b-board-in-play (box newboard))

(provide PlayTile)
(define (PlayTile letter x y b-CurrentBoard)
  (vector-set!
   (vector-ref (board-tiles (unbox b-CurrentBoard)) (- 14 y))
    x
   (tile
     letter
     (score (~a letter))
     #f
     #f
     (tile-multiplier (get-square (board-tiles (unbox b-CurrentBoard)) x y)))))

(define (remove-unlocked b-current-board)
  (for ([x 15])
    (for ([y 15])
      (let ((tile-1 (vector-ref (vector-ref (board-tiles (unbox b-current-board)) (- 14 y))
                 x)))
       
        (cond [(and (equal? #f (tile-locked tile-1)) (string? (tile-letter tile-1)))
               (vector-set!
                (vector-ref (board-tiles (unbox b-current-board)) (- 14 y))
                x
                (tile
                  #\-
                  0
                  #f
                  #f
                  (tile-multiplier tile-1)))])))))


                               
  

(define is-first-turn #t)
(shuffle-bag b-bag)
(define x 10)
(define global-char (~a #\$))

(provide player-tray)
(define player-tray
    (begin
      (shuffle-bag b-bag)
      (for/vector ([i (list 0 1 2 3 4 5 6)])
        (string-downcase
         (grab b-bag)))))
(displayln player-tray) 

(define (bitmap-hop colour)
  (define type-bitmap (make-bitmap (* 2 x) (* 2 x)))
(define type-dc (new bitmap-dc% [bitmap type-bitmap]))

(send type-dc set-brush colour 'solid)
(send type-dc set-pen colour 1 'solid)
(send type-dc draw-rectangle 0 0 (* 2 x) (* 2 x))
  type-bitmap)


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

(define (tile-label x y)
  (define square (get-square squares x y))
  (match square
        [#\n (list (bitmap-hop "Silver") "" 'top) ];normal
        [#\d (list (bitmap-hop "Pink") "." 'top)];double word score
        [#\t (list (bitmap-hop "Aqua") "." 'top)];triple word score
        [#\b (list (bitmap-hop "Blue") "." 'top)];double letter score
        [#\c (list (bitmap-hop "OrangeRed") "." 'top)]))

;(define (set-tile-label board)
;(for ([x 14])
;  (for ([y 14])
;    (let ((letter-val (tile-letter (get-square board x y)))
;          (button (get-square gameboard-column x y)))
;      (cond
;        [(string? letter-val)
;         (send button set-label (string-append letter-val (~a #\() (number->string (score letter-val)) (~a #\))))]
;        [(equal? #\n) (send button set-label "")]
;        [(equal? #\d) (send button set-label "DW")]
;        [(equal? #\t) (send button set-label "TW")]
;        [(equal? #\b) (send button set-label "DL")]
;        [(equal? #\c) (send button set-label "TL")])))))
;  
   
    

;
;(define (tile-label-1 x y board)
;  (define struct (get-square board x y)
;    (con
      
  

(define welcomeframe (new frame%
                          [label "WELCOME"]
                          [height 400]
                          [width 800]
                           [stretchable-width #t]
                                            [stretchable-height #t]))


(define (menu-click frame)
  (class canvas%
    (define/override (on-event mouse-event)
      (cond [(eq? (send mouse-event get-event-type) 'left-down) 
               (begin (send welcomeframe show #f) (send base show #t))]))
                  (super-new [parent frame])))

(define menu-canvas (new (menu-click welcomeframe)
                         [paint-callback ;;like a callback function this keeps executing, but it kind of "paints" on
                          ; the canvas with some bitmap
                          (lambda (canvas1 dc)    
                            (send dc draw-bitmap
                                  (make-object bitmap% "play2.png") ; all png images need to be stored as a bitmap (sort of an array of 0s and 1s)
                                  0 0))]))


                            

                          

(define base (new frame%
                   [label "lexico"]
                   [height (* 27 x)]
                   [width (* 17 x)]
                    [stretchable-width #t]
                                            [stretchable-height #t]))
(define frame (new vertical-panel%
                   [parent base]
                   [min-height (* 27 x)]
                   [min-width (* 17 x)]
                   [alignment (list 'center 'center)]
                   [stretchable-width #t]
                   [stretchable-height #t]
                  [style (list 'vscroll)]
                   ))


(define scores (new horizontal-panel%
                    [parent frame]
                    [min-height x]
                    [min-width (* 15 x)]
                    [vert-margin x]
                    [horiz-margin x]
                    [alignment (list 'center 'center)]
                     [stretchable-width #t]
                                            [stretchable-height #t]
                    ))

(define ai-scores (new vertical-panel%
                               [parent scores]
                               [min-height x]
                               [horiz-margin (* 3 x)]
                               [min-width (* 4 x)]
                               [alignment (list 'center 'center)]
                                [stretchable-width #t]
                                            [stretchable-height #t]
                               ))
(define ai-score-header (new message%
                             [label "CPU::"]
                             [font (make-object font% 15.0 'script 'normal 'bold)]

                          ;   [font (make-object font% 
                           ;           (* 2 2.0))]
                             [parent ai-scores]))
(define ai-score-value (new message%
                            [label "0"]
                            [font (make-object font% 15.0 'script 'normal 'bold #t)]
                            [stretchable-width #t]
                            [parent ai-scores]))

(define player-score (new vertical-panel%
                       [parent scores]
                       [min-height x]
                       [horiz-margin x]
                       [min-width (* 4 x)]
                        [stretchable-width #t]
                                            [stretchable-height #t]

                       ))
(define player-score-header (new message%
                                 [label "YOU::"]
                                  [font (make-object font% 15.0 'script 'normal 'bold)]
                                 [parent player-score]))
(define player-score-value (new message%
                                 [label "0"]
                                 [font (make-object font% 15.0 'script 'normal 'bold #t)]
                                 [stretchable-width #t]
                                 [parent player-score]))


(define gameboard (new vertical-panel%
                       [parent frame ]
                       [style (list 'border)]
                       [min-width (* 15 x)]
                       [min-height (* 15 x)]
                       [vert-margin x]
                       [horiz-margin x]
                        [stretchable-width #t]
                                            [stretchable-height #t]
                       ))


(define gameboard-rows (build-vector 15
                                     (lambda (y)
                                       (new horizontal-panel%
                                            [alignment (build-list 2 (lambda (x) 'center))]
                                            [parent gameboard]
                                            [style (list 'border)]
                                            [horiz-margin x]
                                            [min-width (* 15 x)]
                                            [min-height x]
                                            [stretchable-width #t]
                                            [stretchable-height #t]
                                            ))))


(provide gameboard-columns)
(define gameboard-columns
  (for/vector ([i gameboard-rows] [j (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)])
    (for/vector ([k (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)])
                  
                    (new button%
                         [label (tile-label k (- 14 j))]
                         [parent i]
                         [font 
                              (make-object font% 15.0 'swiss 'normal 'normal #f 'default #f 'aligned)
                            ]
                                ; ['slant 'normal #f 'default #f 'aligned])]
                        ; [style (cond
                         ;         [(not (equal? (tile-label k (- 14 j)) ""))
                          ;         (list 'border)]
                           ;       [else (list)])]
                         [vert-margin 0]	 
                         [horiz-margin 0]
                         [min-width x]	 
                         [min-height x]
                         [stretchable-width #t]
                         [stretchable-height #t]
                         [callback (lambda (button event)
                                       (let* ((label (send button get-label))
                                             (string-label (second label)))
                                             
                                         (cond [(and (or (equal? string-label "")
                                                    (equal? string-label "DW")
                                                    (equal? string-label "DL")
                                                    (equal? string-label "TW")
                                                    (equal? string-label "TL")

                                                    (equal? string-label "."))
                                                     (not (equal? global-char "$")))
                                                (begin (send button set-label (substring global-char 0 1))
                                                       (PlayTile (substring (string-downcase global-char) 0 1)
                                                                 k (- 14 j) b-board-in-play)
                                                       (set! global-char (~a #\$)))])))]
                                                
                                         
                                          
                         ))))
;                         [callback (lambda (button event)
;                                     (send button set-label letter-clicked))]))))

(define (set-tile-label board)
(for ([x 15])
  (for ([y 15])
    (let* ((tile (get-square (board-tiles board) x y))
          (letter-val (tile-letter tile))
          (type (tile-multiplier tile))
          (button (get-square gameboard-columns x y)))
      (cond
        [(string? letter-val)
         ;(equal? #t (tile-locked tile))
         
         (send button set-label (string-upcase letter-val)
                                               )]
        [else (cond 
        [(equal? 'norm type) (send button set-label "")]
        [(equal? 'dws type) (send button set-label ".")]
        [(equal? 'tws type) (send button set-label ".")]
        [(equal? 'dls type) (send button set-label ".")]
        [(equal? 'tls type) (send button set-label ".")])])))))

;(define (remove-unlocked board)
;  (for ([x 15])
;    (for ([y 15])
;      (let ((tile (get-square board x y)))
;            (cond
;              [(string? (tile-letter tile))
;               (cond [(equal? #f (tile-locked tile))
;                      (vector-set!
;   (vector-ref board (- 14 y))
;    x
;   (tile
;     #\-
;     0
;     #f
;     #f
;     (tile-multiplier (get-square board x y))))])])))))
        

;(define (set-tile-label-fixed board)
;(for ([x 15])
;  (for ([y 15])
;    (let* ((tile (get-square (board-tiles board) x y))
;          (letter-val (tile-letter tile))
;          (type (tile-multiplier tile))
;          (button (get-square gameboard-columns x y)))
;      (cond
;        [;(string? letter-val)
;         (equal? #t (tile-locked tile))
;         
;         (send button set-label (string-append (string-upcase letter-val)
;                                               (~a #\() (number->string (score letter-val)) (~a #\))))]
;        [else (cond 
;        [(equal? 'norm type) (send button set-label "")]
;        [(equal? 'dws type) (send button set-label "DW")]
;        [(equal? 'tws type) (send button set-label "TW")]
;        [(equal? 'dls type) (send button set-label "DL")]
;        [(equal? 'tls type) (send button set-label "TL")])])))))
  
(define self-tray (new horizontal-panel%
                       [parent frame]
                       [horiz-margin x]
                       [vert-margin (* 2 x)]
                       [min-width (* 15 x)]
                       [min-height (* 2 x)]
                       [alignment (list 'center 'center)]
                        [stretchable-width #t]
                                            [stretchable-height #t]
                       ))

;(define (place-on-tray tray)  ;;;tray is a vector of buttons.
;  (for ([button tray])
;    (cond [(equal? (send button get-label) "TT")
;           (let ((letter (~a (grab b-bag))))
;        (send button set-label (string-append letter (~a #\() (number->string (score letter)) (~a #\)) )))])))
;




(define tray-buttons (build-vector 7
                                   (lambda (y)
                                     (new button%
                                          [label (let ((strng (vector-ref player-tray y)))
                                                   (string-append (string-upcase strng) (~a #\() (number->string (score strng)) (~a #\))))]
                                          [font (make-object font% 15.0 'default)] 
                                          [parent self-tray]
                                         ; [style (list 'border)]
                                          [vert-margin 0]	 
                                          [horiz-margin 0]	 
                                          [min-width (* 2 x)]	 
                                          [min-height (* 2 x)]
                                          
                                           [stretchable-width #t]
                    [stretchable-height #t]
                    [callback (lambda (button event)
                               (cond
                                 [(not (equal? "TT" (send button get-label)))
                                  (begin (set! global-char (send button get-label))
                                       (send button set-label "TT")
                                       )]))] 
                    
                   ))))

(define (set-tray-label)
  (for/vector ([i (list 0 1 2 3 4 5 6)])
    (let ((strng (vector-ref player-tray i)))
    (send (vector-ref tray-buttons i) set-label (string-append (string-upcase strng) (~a #\()
                                                               (number->string (score strng)) (~a #\)))))))

(define (place-on-tray tray)  ;;;tray is a vector of buttons.
  (for  ([i 7])
    (let ((button (vector-ref tray-buttons i))
          (char (vector-ref player-tray i)))
    (cond [(equal? (send button get-label) "TT")
           (let ((new-val (grab b-bag)))
             (begin (vector-set! player-tray i (string-downcase new-val)) (send button set-label
                                                              (string-append new-val (~a #\() (number->string (score new-val))
                                                                             (~a #\))))))]))))


(define (complete-tray tray)
  (for ([i 7])
    (cond [(equal? #\@ (vector-ref tray i)) (vector-set! tray i (string-downcase (grab b-bag)))])))

(define buttons-tray (new horizontal-panel%
                       [parent frame]
                       [horiz-margin (* 3 x)]
                       [vert-margin (* 2 x)]
                       [min-width (* 8 x)]

                       [min-height (* 2 x)]
                       [alignment (list 'center 'center)]
                        [stretchable-width #t]
                                            [stretchable-height #t]
                       ))

(define (putBack letter b-coll)
  (set-box! b-coll
            (vector-append (vector letter) (unbox b-coll))))

 (define (exchange)
    (for ([i 7])
     (let ((button (vector-ref tray-buttons i))
           (tile (vector-ref player-tray i)))
       (begin (putBack (substring (send button get-label) 0 1) b-bag)
              (shuffle-bag b-bag)
              (let ((new (grab b-bag)))
                (vector-set! player-tray i (string-downcase new))
                (send button set-label
                      (string-append new (~a #\() (number->string (score new)) (~a #\)))))))))
  

(define bottom-buttons (map 
                                   (lambda (y)
                                     (new button%
                                          [label y]
                                          [font (make-object font% 15.0 'script 'normal 'bold)]
                                          [parent buttons-tray]
                                          ;[style (list 'border)]
                                          [vert-margin 0]	 
                                          [horiz-margin 0]	 
                                          [min-width (* 2 x)]	 
                                          [min-height (* 2 x)]
                                           [stretchable-width #t]
                                           [callback (lambda (button event) (callback-f y))]
                   [stretchable-height #t]
                   )) (list "SHUFFLE" "LEAVE" "CHANGE" "PLAY" "UNDO")))

(define (callback-f word)
  (cond [(equal? word "LEAVE") (leave-button)]
        [(equal? word "SHUFFLE") (shuffle-button)]
        [(equal? word "PLAY") (begin (play-button) (set! is-first-turn #f))]
        [(equal? word "CHANGE") (if (equal? is-first-turn #t)
                                      (message-box "SORRY" "cannot exchange tiles on first turn" #f (list 'ok))
                                      (exchange-button))]
        [(equal? word "UNDO") (begin (remove-unlocked b-board-in-play)
                                     (set-tile-label (unbox b-board-in-play))
                                                       
                                     (set-tray-label))]))

(define key-tray (new horizontal-panel%
                       [parent frame]
                       [horiz-margin (* 3 x)]
                       [vert-margin (* 2 x)]
                       [min-width (* 8 x)]

                       [min-height (* 2 x)]
                       [alignment (list 'center 'center)]
                        [stretchable-width #t]
                                            [stretchable-height #t]
                       ))

(define key-buttons (map 
                                   (lambda (y)
                                     (new button%
                                          [label y]
                                          [font (make-object font% 10.0 'roman 'normal 'bold)]
                                          [parent key-tray]
                                          ;[style (list 'border)]
                                          [vert-margin 0]	 
                                          [horiz-margin 0]	 
                                          [min-width (* 2 x)]	 
                                          [min-height (* 2 x)]
                                           [stretchable-width #t]
                                           
                   [stretchable-height #t]
                   )) (list
                       (list (bitmap-hop "Pink") "Double Word Score" 'left)
                       (list (bitmap-hop "Aqua") "Triple Word Score" 'left)
                       (list (bitmap-hop "Blue") "Double Letter Score" 'left)
                       (list (bitmap-hop "OrangeRed") "Triple Letter Score" 'left))))

        


(define (shuffle v)
  (do ((n (vector-length v) (- n 1))) ((zero? n) v)
    (let* ((r (random n))
           (t (vector-ref v r)))
      (vector-set! v r (vector-ref v (- n 1)))
      (vector-set! v (- n 1) t))))

(define (shuffle-labels v)
  (do ((n (vector-length v) (- n 1))) ((zero? n) v)
    (let* ((r (random n))
           (label (send (vector-ref v r) get-label)))
      (send (vector-ref v r) set-label (send (vector-ref v (- n 1)) get-label))
    (send (vector-ref v (- n 1)) set-label label))))



(define (shuffle-button)
  (shuffle-labels tray-buttons))

(define (play-button)
  (let ((total-score (ValidateBoard (unbox b-board-in-play) is-first-turn player-tray)))
    (cond
      [(= -1 total-score)
       (let ((val (message-box "INVALID MOVE" "Oops, sorry, please try again :(" #f (list 'ok))))
         (cond [(equal? val 'ok) (begin (set-tile-label (unbox b-board-in-play)) (set-tray-label))]))]
      [else (begin
              (send player-score-value set-label (number->string (- total-score (string->number (send ai-score-value get-label)))))
              (let ((score (ai (unbox b-board-in-play) #f)))
                (cond [(= score -1) (send ai-score-value set-label
                                              (number->string
                                               (- (string->number (send ai-score-value get-label)) 1)))]
                      [else 
                
                (begin (set-tile-label (unbox b-board-in-play))
                (send ai-score-value set-label
                      (number->string (- score (string->number (send player-score-value get-label))))))])
                
              (complete-tray ai-tray)
              (let ((val1 (message-box "YOUR TURN" "It is your turn now :)" #f (list 'ok))))
                (cond [(equal? 'ok val1) (place-on-tray tray-buttons)]))))])))

(define (exchange-button)
  (let ((val (message-box "EXCHANGE" "Are you sure you want to exchange your tile?
 All your tiles will be exchanged and it will count as your turn."
                          #f (list 'yes-no))))
    (cond [(equal? val 'yes)
           (begin (exchange)
                  (let ((score (ai (unbox b-board-in-play) #f)))
                    (cond [(= score -1) (send ai-score-value set-label
                                              (number->string (- (string->number
                                                                  (send ai-score-value get-label)) 1)))]
                    [else (begin (set-tile-label (unbox b-board-in-play))
                           (send ai-score-value set-label
                                 (number->string (- score (string->number
                                                           (send player-score-value get-label))))))])
                           (complete-tray ai-tray)
                           (message-box "YOUR TURN" "It is your turn now :)" #f (list 'ok))))])))



(define (leave-button)
  (let ((val (message-box "LEAVE" "Are you sure you want to leave the game?" #f (list 'yes-no))))
    (cond [(equal? val 'yes)
           (let ((cpu-score (string->number (send ai-score-value get-label)))
                 (player-score (string->number (send player-score-value get-label))))
             (cond [(> player-score cpu-score) (begin (send base show #f)
                                   ;(new message%
                                    ;    [parent base]
                                               
                                   ;[label (make-object bitmap% "youwin1.png")]) 
                                  (message-box "RESULT!" "YOU WIN!!" #f (list 'ok)))]
                 [(> cpu-score player-score) (begin (send base show #f) 
;                                                    (new message%
;                                                         [parent base]
;                                                         [vert-margin 0]
;                                                         [horiz-margin 0]
;                                                         [alignment (list 'top)]
                                                    (message-box "RESULT!" "You lose :/" #f (list 'ok)))]
                                                         
                                                   
                                  
                 [(= cpu-score player-score) (begin (send base show #f)
                                                    (message-box "RESULT!" "It's a draw!" #f (list 'ok)))]))]


          
  ;               (lambda (canvas1 dc)    
;                            (send dc draw-bitmap
;                                  (make-object bitmap% "draw.jpg") 
;                                  0 0))]))]
          [(equal? 'no val) (void)])))

(send welcomeframe show #t)