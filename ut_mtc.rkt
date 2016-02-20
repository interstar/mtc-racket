#lang racket

(require rackunit "tools.rkt")

; Testing Frame%
(define f1 (new Frame% [filter (lambda (x) x)] [items '()]))
(check-equal? (send (send f1 add "hello world") get-items) '("hello world"))

; Testing FrameStack%

(define s1 (new FrameStack% [xs '()]))
(define s2 (send s1 push* 1 2 3))

(check-equal? (send s2 all) '(3 2 1))
(check-equal? (send s2 peek) 3)
(check-equal? (send (send s2 pop) all) '(2 1))

(check-equal? (send (send s2 swap-top 4) all) '(4 2 1))

; Testing MTC%
