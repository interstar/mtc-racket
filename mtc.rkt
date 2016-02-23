#lang racket

(define f-path "/home/phil/Documents/development/writing/todos/todo.txt")


(define (display-state input mtc)
  (displayln (send mtc get-report))
  (displayln (match input 
    ["l" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (send mtc get-items))]
    [_ (if (not (send mtc empty?)) 
           (string-append "Next item : " (send mtc next))
           "No items")] )))
    

(define (process input mtc)
  (if (> (string-length input) 5)
      ;; it's a new item
      (send mtc add input) 
  (match input
    ["" (send mtc over-report "")]
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



