#lang racket

(require racket/format )

(struct MTC (items input report path file-name))

    
(define (over-items mtc new-items) (struct-copy MTC mtc [items new-items]))
(define (over-report mtc rep) (struct-copy MTC mtc [report rep]))
(define (over-input mtc inp) (struct-copy MTC mtc [input inp]))
(define (over-path mtc new-path) (struct-copy MTC mtc [path new-path]))
(define (over-file-name mtc f-name) (struct-copy MTC mtc [file-name f-name]))

(define (over-file-path mtc path name) (over-file-name (over-path mtc path) name))
(define (make-file-path mtc) (string-append (MTC-path mtc) (MTC-file-name mtc)))

(define (is-empty? mtc) (empty? (MTC-items mtc)))
(define (count mtc) (length (MTC-items mtc)))

; managing an MTC item list 
(define (next mtc) (car (MTC-items mtc)))
(define (tail mtc) (cdr (MTC-items mtc)))
    
(define (add mtc item) 
  (over-report (over-items mtc (append (MTC-items mtc) (list item)))
               (string-append "Added : " item)))

(define (add-front mtc item)
  (over-report (over-items mtc (append (list item) (MTC-items mtc)))
               (string-append "Added (first) : " item)))
    
(define (load-items mtc items) (foldl (λ (item mtc) (add mtc item)) mtc items))

(define (add* mtc . items) (load-items mtc items))

(define (delay mtc) 
  (let ([items (MTC-items mtc)])
      (over-report (over-items mtc (append (cdr items) (list (car items)))) 
                   (string-append "Delayed : " (car items) ))))
    
(define (delay-by mtc n) 
  (if (< (length (tail mtc)) n)
      (over-report (delay mtc) (string-append "Delayed to end : " (next mtc)))
      (over-report (over-items mtc 
                      (let ([before (take (tail mtc) n)]
                            [after (drop (tail mtc) n)])
                        (append before (list (next mtc)) after)))
                   (string-append "Delayed by " (number->string n) " : " (next mtc)))))
    
(define (done mtc) 
      (over-report (over-items mtc (tail mtc)) (string-append "Done : " (next mtc))))

(define (pull-last mtc) 
  (let* ([size (count mtc)]
         [sting (last (MTC-items mtc))]
         [snake (take (MTC-items mtc) (- size 1))]
         [new-items (append (list sting) snake)])
    (over-report (over-items mtc new-items) (string-append "Pulled from end : " sting))))

(define (pull-to-front mtc p rep) 
      (let* ([hits (filter p (MTC-items mtc))]
             [misses (filter (λ(x)(not (p x))) (MTC-items mtc))])
        (over-report (over-items mtc (append hits misses)) rep )))

(define (throw-to-back mtc p rep) (pull-to-front mtc (λ (x) (not (p x))) rep))

(define (edit mtc extra)
  (let* ([item (next mtc)]
         [edited (string-append item " " extra)])
    (over-report (add-front (done mtc) edited) (string-append "Appended " extra " to " item)))
    )

(define (kill mtc pattern)
  (let* ([misses (filter (λ(x)(not (regexp-match (pregexp pattern) x))) (MTC-items mtc))])
        (over-report (over-items mtc misses) (string-append "Kill*ed ''" pattern "''"))))
    
   
;; Order of MTC struct  (items input report path file-name)    
(define (new-MTC) (MTC '() "" "" "" ""))

(provide new-MTC over-items over-report over-input 
         over-path over-file-name make-file-path over-file-path
         is-empty? count next tail add add-front load-items add* delay delay-by done
         pull-last pull-to-front throw-to-back edit kill
         MTC-items MTC-report MTC-path MTC-file-name)