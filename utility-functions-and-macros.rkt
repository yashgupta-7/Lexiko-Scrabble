#lang racket
(require racket/vector)
(provide vector-append*
         vector-findf
         while
         vector-flatten
         vector-remove-duplicates
         perms
         choose
         choose-and-arrange
         vector-index)


(define (vector-index vec i)
  (define ans 0)
  (for ([t vec] [j 100])
    (cond [(equal? t i) (set! ans j)]))
  ans)
 ;;;;;;;;;;
(define (vector-append* vec vec-acc)
  (if (equal? #() vec) vec-acc
      (vector-append*
       (vector-drop vec 1)
       (vector-append (vector-ref vec 0) vec-acc))))
  


(define (vector-findf f vector)
    
    (let loop ([v vector])
      (cond
       [(equal? v #()) #f]
       [(not (vector? v))
        (raise-mismatch-error 'findf
                              "not a proper list: "
                              vector)]
       [else (let ([a (vector-ref v 0)])
               (if (f a)
                   a
                   (loop (vector-drop v 1))))])))


(define-syntax while
  (syntax-rules ()
    [(while condition expr) (begin (define (iter) (cond [condition (begin expr (iter))])) (iter))]))


(define (vector-flatten orig-sexp)
  (let loop ([sexp orig-sexp] [acc #()])
    (cond [(equal? #() sexp) acc]
          [(vector? sexp) (loop (vector-ref sexp 0) (loop (vector-drop sexp 1) acc))]
          [else (vector-append (vector sexp) acc)])))


(define (vector-remove-duplicates l [=? equal?] #:key [key #f])
  
  (define-syntax-rule (no-key x) x)
  
  (let* ([len (vector-length l)]
         [h (cond [(<= len 1) #t]
                  
                  [(eq? =? eq?) (make-hasheq)]
                  [(eq? =? equal?) (make-hash)]
                  [else #f])])
    (case h
      [(#t) l]
      [else
      
       (let-syntax ([loop
                     (syntax-rules ()
                       [(_ getkey)
                        (let loop ([l l])
                          (if (equal? #() l)
                            l
                            (let* ([x (vector-ref l 0)] [k (getkey x)] [l (vector-drop l 1)])
                              (if (hash-ref h k #f)
                                (loop l)
                                (begin (hash-set! h k #t)
                                       (vector-append (vector x) (loop l)))))))])])
         (if key (loop key) (loop no-key)))])))



(define (perms l)
  (define (g x) (map (lambda (y) (cons x y)) (perms (remove x l))))
  (match l
    ['() '(())]
    [(cons x xs) (append* (map g l))]))



(define (choose l n)
  (cond [(= n (length l)) (list l)]
        [(= n 0) '(())]
        [else  (append  (choose (cdr l) n)
                        (map (lambda (y) (cons (car l) y))
                             (choose (cdr l) (- n 1))))]))



(define (choose-and-arrange l n)
  (append* (map (lambda (x) (perms x)) (choose l n))))


