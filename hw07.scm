(define (filter-lst fn lst)
  (cond
    ((null? lst)'())
    ((fn (car lst))(cons (car lst) (filter-lst fn (cdr lst))))
    (else (filter-lst fn (cdr lst))))
)

;;; Tests
(define (even? x)
  (= (modulo x 2) 0))
(filter-lst even? '(0 1 1 2 3 5 8))
; expect (0 2 8)


(define (interleave first second)
  (cond
    ((and (null? first) (null? second))'())
    ((null? first)(cons (car second) (cdr second)))
    ((null? second)(cons (car first) (cdr first)))
    (else (cons (car first) (cons (car second) (interleave (cdr first) (cdr second)))))))

(interleave (list 1 3 5) (list 2 4 6))
; expect (1 2 3 4 5 6)

(interleave (list 1 3 5) nil)
; expect (1 3 5)

(interleave (list 1 3 5) (list 2 4))
; expect (1 2 3 4 5)

(define (accumulate combiner start n term)
(define a 1)
(define (help_accu combiner n term a)
  (if (= n 1)
      (term a)
      (combiner (term a) (help_accu combiner (- n 1) term (+ a 1)))))
(combiner start (help_accu combiner n term a)))



(define (no-repeats lst)
(define (in? item lst)
  (cond 
    ((null? lst) #f)
    ((equal? (car lst) item) #t)
    (else (in? item (cdr lst)))))

(if (null? lst)
    '()
    (let ((memory (list (car lst))))
      (define (helper lst memory)
        (cond 
          ((null? lst) '())
          ((in? (car lst) memory)
           (helper (cdr lst) memory))
          (else (cons (car lst) (helper (cdr lst) (cons (car lst) memory))))))

      (cons (car lst) (helper (cdr lst) memory)))))



