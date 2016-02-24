#lang racket

(require racket/format)

(define Frame% 
  (class object%
    (init filter)
    (init items)
    (define _filter filter)
    (define _items items)
    (super-new)
    
    (define/public (get-filter) _filter)
    (define/public (get-items) _items) 
    (define/public (over-items new-items) (new Frame% [filter (get-filter)] [items new-items]))

    (define/public (is-empty?) (empty? _items))
    
    ; managing an MTC item list 
    (define/public (next) (car _items))
    (define/public (add item) (over-items (append _items (list item))))
    (define/public (delay) (over-items (append (cdr _items) (list (car _items)))))
    (define/public (delay-by n) 
      (over-items 
       (let ([before (take (cdr _items) n)]
             [after (drop (cdr _items) n)])
         (append before (list (car _items)) after)
         )))
    (define/public (done) (over-items (cdr _items)))
    
    ))

(define (new-Frame) (new Frame% [filter (lambda (x) x)] [items '()]))

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

    (define/public (get-report) (_report))
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
    
    (define/public (add* . items)
      (foldl (λ (item mtc) (send mtc add item)) this items))
                      
                      
    (define/public (delay)
      (let* ([f (λ (fm) (send fm delay))]
             [r (string-append "Delayed : " (send (current-frame) next))] )
        (operate "/" f r)))
    
    (define/public (delay-by n) 
      (let* ([f (λ (fm) (send fm delay-by n))]
             [r (string-append "Delayed by " (number->string n) " : " (send (current-frame) next))] )
        (operate (string-append "delay-by " (number->string n)) f r)))
    
    ))
        
        
    
    

(define (new-MTC) (new MTC% [input ""] [frame-stack (new-FrameStack)] [report ""]))

(provide Frame% new-Frame FrameStack% new-FrameStack MTC% new-MTC)