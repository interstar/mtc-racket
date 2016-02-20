#lang racket


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

    ; managing an MTC item list 
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
    
    (define/public (swap-top f) (send (pop) push f))
    (define/public (all) _xs)))



(define MTC%
  (class object%
    (init input)
    (init frame-stack)
    (init report)
    (define _input input)
    (define _frame-stack frame-stack)
    (define _report report)
    (super-new)
    (define/public (over-input in) (new MTC% [input in] [frame-stack _frame-stack] [report _report]))
    (define/public (over-fs fs) (new MTC% [input _input] [frame-stack fs] [report _report]))
    (define/public (over-report rep) (new MTC% [input _input] [frame-stack _frame-stack] [report rep] ))

    (define/public (add item) (send (send this over-fs ) over-report (string-append "Added : " item)))
    
    ))


(provide Frame% FrameStack% MTC%)