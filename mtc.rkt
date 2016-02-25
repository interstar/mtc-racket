#lang racket

(require "tools.rkt")


(define f-path "/home/phil/Documents/development/writing/todos/")
(define f-name (string-append f-path "todo.txt"))

(define (display-state input mtc)
  (displayln (send mtc get-report))
  (displayln (match input
    ["l" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (send mtc get-items))]
    [_ (if (not (send mtc is-empty?)) 
           (string-append "Next item : " (send mtc next))
           "No items")] )))
    

(define (process input mtc)
  (if (> (string-length input) 5)
      ;; it's a new item
      (send mtc add input) 
  (match input
    ["" (send mtc over-report "")]
    ["///" (send mtc delay-by 50)]
    ["//" (send mtc delay-by 10)]
    ["/" (send mtc delay)]
    ["*" (send mtc done)]
    ["s" (begin
           (display-lines-to-file (send mtc get-items) f-name 
                                  #:mode 'text 
                                  #:exists 'replace)
           (send mtc over-report "Saved") )]
    ["l" (send mtc over-report "Your full list")]
    [_ (send mtc over-report (string-append "Don't understand : " input)) ] )))

(define (main input mtc)  
    (display-state input mtc)
    (let ([new-input (read-line (current-input-port) 'any)])
      (main new-input (process new-input mtc))))

(define args (current-command-line-arguments))

(main "" (send+ (new-MTC) 
                (load-items (file->lines f-name))
                (over-report "Welcome to Mind Traffic Control")))



