#lang racket

(require "tools.rkt")


(define f-path "/home/phil/Documents/development/writing/todos/")
(define f-name (string-append f-path "todo.txt"))

(define (display-state input mtc)
  (displayln (send mtc get-report))
  (displayln (match input
    ["ll" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (take (send mtc get-items) 10))]
    ["l" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (send mtc get-items))]               
    [_ (if (not (send mtc is-empty?)) 
           (string-append "Next item : " (send mtc next))
           "No items")] )))
    

(define (process input mtc)
  (if (regexp-match (pregexp " ") input)
      (process-command input mtc)
      (process-short input mtc)))

(define (process-command input mtc)
  (let* ([command (string-split input)]
         [op (car command)]
         [arg (cadr command)])
    (cond
      [(string=? op "+") (send mtc pull-to-front (λ (s) (regexp-match (pregexp arg) s)) (string-append "Pulled " arg))]
      [(string=? op "-") (send mtc throw-to-back (λ (s) (regexp-match (pregexp arg) s)) (string-append "Thrown " arg))]
      [(string=? op "e") (send mtc edit arg)]
      [(string=? op "k*") (send mtc kill arg)]
      [else (send mtc add input)])))
      
(define (process-short input mtc)
  (if (> (string-length input) 5) (send mtc add input) 
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
        ["ll" (send mtc over-report "First 10")]
        ["l" (send mtc over-report "Your full list")]
        ["c" (send mtc over-report (string-append "No items : " (number->string (send mtc count))))]
        [_ (send mtc over-report (string-append "Don't understand : " input)) ] )))

(define (main input mtc)  
    (display-state input mtc)
    (let ([new-input (read-line (current-input-port) 'any)])
      (main new-input (process new-input mtc))))

(define args (current-command-line-arguments))

(main "" (send+ (new-MTC) 
                (load-items (file->lines f-name))
                (over-report "Welcome to Mind Traffic Control")))



