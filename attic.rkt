#lang racket

; Testing Frame%
(let ([f0 (new-Frame)])
  (check-equal? ((send f0 get-filter) 123) 123)
  (check-equal? (send f0 is-empty?) true)
  )

(let* ([f1 (new Frame% [a-filter (lambda (x) x)] [items '()])]
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

(define (new-Frame) (new Frame% [a-filter (lambda (x) x)] [items '()]))

(define FrameStack%
  (class object%
    (init xs)
    (define _xs xs)
    (super-new)
    (define/public (push x) (new FrameStack% [xs (cons x _xs)]))
    (define/public (push* . xs) (foldl (lambda (x s) (send s push x)) this xs))
    (define/public (pop) (new FrameStack% [xs (cdr _xs)]))
    (define/public (peek) (car _xs))
    (define/public (pop*) (list (peek) (pop)))
    
    (define/public (is-empty?) (empty? _xs))
    
    (define/public (swap-top f) (send (pop) push f))
    (define/public (all) _xs)))

(define (new-FrameStack) (new FrameStack% [xs (list (new-Frame))]))


(define MTC%
  (class object%
    (init-field input)
    (init-field frame-stack)
    (init-field report)
    (define _input input)
    (define _frame-stack frame-stack)
    (define _report report)
    (super-new)
    (define/public (over-input in) (new MTC% [input in] [frame-stack _frame-stack] [report _report]))
    (define/public (over-fs fs) (new MTC% [input _input] [frame-stack fs] [report _report]))
    (define/public (over-report rep) (new MTC% [input _input] [frame-stack _frame-stack] [report rep] ))

    (define/public (get-report) _report)
    (define/public (get-items) (send (current-frame) get-items))
    
    (define/public (current-frame) (send _frame-stack peek))
    (define/public (is-empty?) (send (current-frame) is-empty?))
    (define/public (next) (send (current-frame) next))
        
    (define/public (operate inp f rep)
      (let* ([top (send _frame-stack peek )]
             [newtop (f top)])
             (new MTC% [input inp] [frame-stack (send _frame-stack swap-top newtop)] [report rep])
             ))

    (define/public (add item) 
      (let* ([f (λ (fm) (send fm add item))]
             [r (string-append "Added : " item)] )
        (operate item f r)))
            
    (define/public (add-front item) 
      (let* ([f (λ (fm) (send fm add-front item))]
             [r (string-append "Added (front) : " item)] )
        (operate item f r)))

    
    (define/public (load-items items) 
      (foldl (λ (item mtc) (send mtc add item)) this items))

    (define/public (add* . items) (load-items items))
                      
    (define/public (delay)
      (let* ([f (λ (fm) (send fm delay))]
             [r (string-append "Delayed : " (next))] )
        (operate "/" f r)))
    
    (define/public (delay-by n) 
      (let* ([f (λ (fm) (send fm delay-by n))]
             [r (string-append "Delayed by " (number->string n) " : " (next))] )
        (operate (string-append "delay-by " (number->string n)) f r)))
    
    (define/public (done)
      (let* ([f (λ (fm) (send fm done))]
             [r (string-append "Done : " (next))])
        (operate "*" f r)))
    
    (define/public (count) (length (get-items)))
    
    (define/public (pull-to-front p rep) 
        (operate "" (λ (fm) (send fm pull-to-front p)) rep))
    
    (define/public (throw-to-back p rep)
        (operate "" (λ (fm) (send fm throw-to-back p)) rep))
    
    (define/public (edit extra) 
      (let* ([f (λ (fm) (send fm edit extra))]
             [r (string-append "Appended " extra " to " (next))])
        (operate "" f  r)))

    (define/public (kill pattern)
      (let* ([f (λ (fm) (send fm kill pattern))]
             [r (string-append "Killed all : " pattern)])
        (operate "" f r)))
    
    ;(define (keep pattern lines) (filter (lambda (s) (regexp-match (pregexp pattern) s)) lines))
    ;(define (kill pattern lines) (filter (lambda (s) (not (regexp-match (pregexp pattern) s))) lines))

    
    
    ))
        
        
    
(define (new-Frame) (new Frame% [a-filter (lambda (x) x)] [items '()]))

(define FrameStack%
  (class object%
    (init xs)
    (define _xs xs)
    (super-new)
    (define/public (push x) (new FrameStack% [xs (cons x _xs)]))
    (define/public (push* . xs) (foldl (lambda (x s) (send s push x)) this xs))
    (define/public (pop) (new FrameStack% [xs (cdr _xs)]))
    (define/public (peek) (car _xs))
    (define/public (pop*) (list (peek) (pop)))
    
    (define/public (is-empty?) (empty? _xs))
    
    (define/public (swap-top f) (send (pop) push f))
    (define/public (all) _xs)))

(define (new-FrameStack) (new FrameStack% [xs (list (new-Frame))]))


(define MTC%
  (class object%
    (init-field input)
    (init-field frame-stack)
    (init-field report)
    (define _input input)
    (define _frame-stack frame-stack)
    (define _report report)
    (super-new)
    (define/public (over-input in) (new MTC% [input in] [frame-stack _frame-stack] [report _report]))
    (define/public (over-fs fs) (new MTC% [input _input] [frame-stack fs] [report _report]))
    (define/public (over-report rep) (new MTC% [input _input] [frame-stack _frame-stack] [report rep] ))

    (define/public (get-report) _report)
    (define/public (get-items) (send (current-frame) get-items))
    
    (define/public (current-frame) (send _frame-stack peek))
    (define/public (is-empty?) (send (current-frame) is-empty?))
    (define/public (next) (send (current-frame) next))
        
    (define/public (operate inp f rep)
      (let* ([top (send _frame-stack peek )]
             [newtop (f top)])
             (new MTC% [input inp] [frame-stack (send _frame-stack swap-top newtop)] [report rep])
             ))

    (define/public (add item) 
      (let* ([f (λ (fm) (send fm add item))]
             [r (string-append "Added : " item)] )
        (operate item f r)))
            
    (define/public (add-front item) 
      (let* ([f (λ (fm) (send fm add-front item))]
             [r (string-append "Added (front) : " item)] )
        (operate item f r)))

    
    (define/public (load-items items) 
      (foldl (λ (item mtc) (send mtc add item)) this items))

    (define/public (add* . items) (load-items items))
                      
    (define/public (delay)
      (let* ([f (λ (fm) (send fm delay))]
             [r (string-append "Delayed : " (next))] )
        (operate "/" f r)))
    
    (define/public (delay-by n) 
      (let* ([f (λ (fm) (send fm delay-by n))]
             [r (string-append "Delayed by " (number->string n) " : " (next))] )
        (operate (string-append "delay-by " (number->string n)) f r)))
    
    (define/public (done)
      (let* ([f (λ (fm) (send fm done))]
             [r (string-append "Done : " (next))])
        (operate "*" f r)))
    
    (define/public (count) (length (get-items)))
    
    (define/public (pull-to-front p rep) 
        (operate "" (λ (fm) (send fm pull-to-front p)) rep))
    
    (define/public (throw-to-back p rep)
        (operate "" (λ (fm) (send fm throw-to-back p)) rep))
    
    (define/public (edit extra) 
      (let* ([f (λ (fm) (send fm edit extra))]
             [r (string-append "Appended " extra " to " (next))])
        (operate "" f  r)))

    (define/public (kill pattern)
      (let* ([f (λ (fm) (send fm kill pattern))]
             [r (string-append "Killed all : " pattern)])
        (operate "" f r)))
    
    ;(define (keep pattern lines) (filter (lambda (s) (regexp-match (pregexp pattern) s)) lines))
    ;(define (kill pattern lines) (filter (lambda (s) (not (regexp-match (pregexp pattern) s))) lines))

    
    
    ))
        
        
    
