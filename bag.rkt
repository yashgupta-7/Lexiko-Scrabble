#lang racket
(require racket/vector)
(provide bag
         shuffle
         b-bag
         shuffle-bag
         grab
         putBack)


(define bag
  (vector-append
   (make-vector 9 "A")
   (make-vector 2 "B")
   (make-vector 2 "C")
   (make-vector 4 "D")
   (make-vector 12 "E")
   (make-vector 2 "F")
   (make-vector 3 "G")
   (make-vector 2 "H")
   (make-vector 9 "I")
   (make-vector 1 "J")
   (make-vector 1 "K")
   (make-vector 4 "L")
   (make-vector 2 "M")
   (make-vector 6 "N")
   (make-vector 8 "O")
   (make-vector 2 "P")
   (make-vector 1 "Q")
   (make-vector 6 "R")
   (make-vector 4 "S")
   (make-vector 6 "T")
   (make-vector 4 "U")
   (make-vector 2 "V")
   (make-vector 2 "W")
   (make-vector 1 "X")
   (make-vector 4 "Y")
   (make-vector 1 "Z")
    ))

;(provide shuffle)
;(define (shuffle v)
;  (do ((n (vector-length v) (- n 1))) ((zero? n) v)
;    (let* ((r (random n))
;           (t (vector-ref v r)))
;      (vector-set! v r (vector-ref v (- n 1)))
;      (vector-set! v (- n 1) t))))


(define (shuffle- a)
  (for ([x a] [i (in-naturals)])
    (define j (random (add1 i)))
    (unless (= j i) (vector-set! a i (vector-ref a j)))
    (vector-set! a j x))
  a)



(define b-bag (box bag))


(define (shuffle-bag b-Bag)
  (set-box! b-Bag (shuffle- (unbox b-Bag))))


(define (grab b-coll)
  (if (equal? (vector ) (unbox b-coll))
      (begin
        (displayln "No more Tiles left.")
        -1)
      (let ((pick (vector-ref (unbox b-coll) 0)))
        (begin
          (set-box! b-coll (vector-drop (unbox b-coll) 1))
          pick))))


(define (putBack letter b-coll)
  (set-box! b-coll
            (vector-append (vector letter) (unbox b-coll))))

 
  
  
