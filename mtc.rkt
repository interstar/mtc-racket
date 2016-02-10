#lang racket

(define f-path "/home/phil/bin/todo.txt")

(struct MTC (items report))

(define (new-report mtc rep) (MTC (MTC-items mtc) rep))

(define (add mtc item) (MTC (append (MTC-items mtc) (list item)) (string-append "Added : " item)))

(define (delay mtc) 
  (let ([items (MTC-items mtc)])
    (MTC (append (cdr items) (list (car items))) (string-append "Delayed " (car items)))))

(define (done mtc) 
  (let ([items (MTC-items mtc)])
    (MTC (cdr items) (string-append "Done :" (car items)))))

(define (display-state input mtc)
  (displayln (MTC-report mtc))
  (displayln (match input 
    ["l" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (MTC-items mtc))]
    [_ (if (not (empty? (MTC-items mtc))) 
           (string-append "Next item : "(car (MTC-items mtc))) 
           "No items")] )))
    

(define (process input mtc)
  (if (> (string-length input) 5)
      ;; it's a new item
      (add mtc input) 
  (match input
    ["" (new-report mtc "")]
    ["/" (delay mtc)]
    ["*" (done mtc)]
    ["s" (begin
           (display-lines-to-file (MTC-items mtc) f-path 
                                  #:mode 'text 
                                  #:exists 'replace)
           (MTC (MTC-items mtc) "Saved") )]
    ["l" (new-report mtc "Your full list")]
    [_ (MTC (MTC-items mtc) (string-append "Don't understand : " input)) ] )))

(define (main input mtc)  
    (display-state input mtc)
    (let ([new-input (read-line (current-input-port) 'any)])
      (main new-input (process new-input mtc))))

(define args (current-command-line-arguments))

(main "" (MTC (file->lines f-path) "Welcome to Mind Traffic Control"))



