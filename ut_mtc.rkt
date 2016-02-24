#lang racket

(require rackunit "tools.rkt")

; Testing Frame%
(let ([f0 (new-Frame)])
  (check-equal? ((send f0 get-filter) 123) 123)
  (check-equal? (send f0 is-empty?) true)
  )

(let* ([f1 (new Frame% [filter (lambda (x) x)] [items '()])]
       [f2 (send (send f1 add 1) add 2)] 
       [f3 (foldl (lambda (x f) (send f add x)) f1 '(1 2 3 4 5))]
       )
  (check-equal? (send (send f1 add "hello world") get-items) '("hello world"))
  (check-equal? (send f2 get-items) '(1 2))
  (check-equal? (send (send f2 delay) get-items) '(2 1))
  (check-equal? (send (send f2 done) get-items) '(2))
  (check-equal? (send f3 get-items) '(1 2 3 4 5))
  (check-equal? (send (send f3 delay-by 1) get-items) '(2 1 3 4 5))
  (check-equal? (send (send f3 delay-by 3) get-items) '(2 3 4 1 5)) 
)

; Testing FrameStack%
(let* ([s0 (new-FrameStack)]
       [f0 (send s0 peek)])  
  (check-equal? (send (send s0 pop) is-empty?) true)
  (check-equal? (send f0 is-empty?) true)
  )

; Stackness of it
(let* ([s1 (new FrameStack% [xs '()])]
       [s2 (send s1 push* 1 2 3)]
       )
  (check-equal? (send s2 all) '(3 2 1))
  (check-equal? (send s2 peek) 3)
  (check-equal? (send (send s2 pop) all) '(2 1))
  (check-equal? (send (send s2 swap-top 4) all) '(4 2 1)))



; Testing MTC%


(let* ([m0 (new-MTC)]
       [item1 "this is the first item"]
       [m1 (send m0 add item1 )]
       [item2 "a second item"]
       [m2 (send m1 add item2)]
       [m3 (send m2 delay)]
       [m4 (send m2 add* "hello teenage america" "another green world")]
       [m5 (send m4 delay-by 2)]
      )
  
  (check-equal? (send m0 is-empty?) true)
  (check-equal? (send m1 next) item1)
  (check-equal? (send m2 next) item1)
  (check-equal? (send m3 next) item2)
  (check-equal? (send m4 get-items) '("this is the first item" "a second item" "hello teenage america" "another green world"))
  (check-equal? (send m5 get-items) '("a second item" "hello teenage america" "this is the first item" "another green world"))
  )