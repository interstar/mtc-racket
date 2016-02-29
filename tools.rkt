#lang racket

(require racket/format)

(define MTC% 
  (class object%
    (init-field items)
    (init-field input)
    (init-field report)
    
    (define _input input)
    (define _items items)
    (define _report report)
    
    (super-new)
    
    (define/public (get-items) _items) 
    (define/public (get-input) _input)
    (define/public (get-report) _report)
    
    (define/public (over-items new-items) (new MTC% [input (get-input)] [items new-items] [report (get-report)] ))
    (define/public (over-report rep) (new MTC% [input (get-input)] [items (get-items)] [report rep]))
    (define/public (over-input inp) (new MTC% [input inp] [items (get-items)] [report (get-report)]))

    (define/public (is-empty?) (empty? _items))
    (define/public (count) (length (get-items)))
    
    ; managing an MTC item list 
    (define/public (next) (car _items))
    
    (define/public (add item) 
      (send+ (over-items (append _items (list item))) 
             (over-report (string-append "Added : " item))))
    
    (define/public (add-front item) 
      (send+ (over-items (append (list item) _items)) 
             (over-report (string-append "Added (first) : " item))))
    
    (define/public (load-items items) (foldl (λ (item mtc) (send mtc add item)) this items))

    (define/public (add* . items) (load-items items))    
    
    (define/public (delay) 
      (send+ (over-items (append (cdr _items) (list (car _items)))) 
             (over-report (string-append "Delayed : " (car _items)) )))
    
    (define/public (delay-by n) 
      (let ([tail (cdr _items)])
        (if (< (length tail) n) (send this delay)
            (send+
             (over-items 
             (let ([before (take tail n)]
                   [after (drop tail n)])
               (append before (list (car _items)) after)
             ))
             (over-report (string-append "Delayed by " (number->string n) " : " (car _items)))))))
    
    (define/public (done) (over-items (cdr _items)))

    (define/public (pull-to-front p rep) 
      (let* ([hits (filter p (send this get-items))]
             [misses (filter (λ(x)(not (p x))) _items)])
        (send+ (over-items (append hits misses)) (over-report rep) )))
    
    (define/public (throw-to-back p rep)
      (pull-to-front (λ (x) (not (p x))) rep))

    (define/public (edit extra)
      (let* ([item (next)]
             [edited (string-append item " " extra)])
        (send+ (done) (add-front edited) (over-report (string-append "Appended " extra " to " item)))
        ))

    (define/public (kill pattern)
      (let* ([misses (filter (λ(x)(not (regexp-match (pregexp pattern) x))) _items)])
        (send+ (over-items misses) (over-report (string-append "Kill*ed ''" pattern "''")))))
    
    ))


    
(define (new-MTC) (new MTC% [input ""] [items '()] [report ""]))

(provide MTC% new-MTC)