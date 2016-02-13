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
    
    (define/public (add item) (new Frame% [filter _filter] [items (append _items (list item))]))

    ))

(define f1 (new Frame% [filter (lambda (x) x)] [items '()]))
((send f1 get-filter) 12)
(send (send f1 add "hello world") get-items)

(define Stack%
  (class object%
    (init xs)
    (define _xs xs)
    (super-new)
    (define/public (push x) (new Stack% [xs (cons x _xs)]))
    (define/public (pop) (new Stack% [xs (cdr _xs)]))
    (define/public (peek) (car _xs))
    (define/public (all) _xs)))
                             
(define s1 (new Stack% [xs '()]))
(send (send (send (send (send s1 push 1) push 2) push 3) pop) all)


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
    
    ))


