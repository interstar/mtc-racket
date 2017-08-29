#lang racket
(require rackunit)

;;; Simple Can Fail / Maybe type monad (I'm not sure how useful this is without types, but it feels the principled thing to do

(struct Faily (val err status))
(define (just x) (Faily x "" #f))
(define (error x) (Faily "" x #t))

(define (error? m) (eq? (Faily-status m) #t ))

(define (bind f m)
  (cond [(error? m) m]
        [else (f (Faily-val m))]))

(define (open m)
  (cond [(error? m) (Faily-err m)]
        [else (Faily-val m)]))
              

(provide Faily just error bind open)

(let* ([f (λ (x) (just (* x x)))]
       [j3 (just 3)]
       [fail (error "This failed")])
  (check-equal? (open j3) 3)
  (check-equal? (open fail) "This failed")
  (check-equal? (open (bind f j3)) 9)
  (check-equal? (open (bind f fail)) "This failed")
  (check-equal? (open (bind f (bind f j3))) 81)
  (check-equal? (open (bind f (bind f fail))) "This failed")
)