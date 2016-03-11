#lang racket

(require "tools.rkt" "actions.rkt")


; UI Code
(define (safe-take xs n) 
  (let ([ln (length xs)])
    (if (< n ln) 
      (take xs n) 
      (take xs ln) )))

(define (display-state input mtc)
  (displayln (MTC-report mtc))
  (displayln (match input
    ["lll" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (safe-take (MTC-items mtc) 50))]               
    ["ll" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (safe-take (MTC-items mtc) 10))]               
    ["l" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (MTC-items mtc))]
    ["h" "Commands

 s\tSAVE : If you do not explicitly save, you will lose what you put into MTC.
 h\tHelp : Shows this list of commands
 /\tDelay the current next-item ie. Push it to the end of the queue.
 //\tPush the current next-item 10 back.
 ///\tPush current next-item 50 back. 
 *\tDone / Delete. There's no difference. 
 c\tCount items in queue
 + TEXT\tfinds ALL items that contain TEXT and pulls them to the front of the queue. 
 - TEXT\tfinds ALL items that contain TEXT and pushes them to the end of the queue. 
 l\tShows the entire queue.
 ll\tA quick peek ahead at the first 10 items on queue.
 k* TEXT\tMulti-kill or bulk delete. It removes all items from the list that contain TEXT.
 e EXTRA TEXT\tAppends EXTRA TEXT to the next item."]
              
    [_ (if (not (is-empty? mtc)) 
           (string-append "Next item : " (next mtc))
           "No items")] ))
  )
    

(define (process input mtc)
  (if (regexp-match (pregexp " ") input)
      (process-command input mtc)
      (process-short input mtc)))

(define (process-command input mtc)
  (let* ([command (string-split input)]
         [op (car command)]
         [arg (cadr command)]
         [args (string-join (cdr command) " " )])
    (cond
      [(string=? op "+") (pull-to-front mtc  (λ (s) (regexp-match (pregexp arg) s)) (string-append "Pulled " arg))]
      [(string=? op "-") (throw-to-back mtc  (λ (s) (regexp-match (pregexp arg) s)) (string-append "Thrown " arg))]
      [(string=? op "e") (edit mtc args)]
      [(string=? op "q") 
       (over-report mtc 
             (if (page-exists? mtc arg) 
           (string-append "page " (page-path mtc arg) " exists")
           (string-append "page " (page-path mtc arg) " DOESN'T exist")))]
      [(string=? op "k*") (kill mtc  arg)]
      [else (add mtc input)])))
      
(define (process-short input mtc)
  (let* ([reply (lambda (s) (over-report mtc s))])
    (if (> (string-length input) 5) (add mtc input) 
      (match input
        ["" (reply "")]
        ["///" (delay-by mtc  50)]
        ["//" (delay-by mtc  10)]
        ["/" (delay mtc )]
        ["\\" (pull-last mtc)]
        ["*" (done mtc )]
        ["s" (begin
               (display-lines-to-file (MTC-items mtc ) (make-file-path mtc ) 
                                      #:mode 'text 
                                      #:exists 'replace)
               (reply "Saved") )]
        ["ll" (reply "First 10")]
        ["lll" (reply "First 50")]
        ["l" (reply "Your full list")]
        ["c" (reply (string-append "No items : " (number->string (count mtc ))))]
        ["h" (reply "MTC Help")]
        ["a" (reply (string-append "Analyze Url : " (analyze (next mtc ) )))]
        [_ (reply (string-append "Don't understand : " input)) ] ))) )

(define (main input mtc)  
    (display-state input mtc)
    (let ([new-input (read-line (current-input-port) 'any)])
      (main new-input (process new-input mtc))))

;;; Files

(define (file-name->MTC f-path f-name)  
  (let* ([mtc (over-file-path (new-MTC) f-path f-name)])
    (when (not (file-exists? (make-file-path mtc))) (display-to-file "" (make-file-path mtc)))
    (over-report (load-items mtc (file->lines (make-file-path mtc)))
                 (string-append "Welcome to Mind Traffic Control
Todo file is " (make-file-path mtc)))))
 
; Thanks Asumu Takikawa
; http://stackoverflow.com/questions/35803167/how-can-i-tell-a-racket-program-to-load-optional-configuration-code/35805340#35805340
; 
; Config file is just a definition of a dictionary ... eg.
;
; ((todo-dir . "/home/phil/Documents/development/writing/todos/"))
;
; See README for more info.
;
(define (get-config home-path name default)
  (let* ([config-path (string-append home-path "bin/.mtc/config.rkt")]
         [path (string->path config-path)])
    (with-handlers ([exn:fail? (λ (e) default)])
      (let* ([s (file->value (string->path config-path))])
        (dict-ref s name default )))))



;;;; Imperative bit.

(define args (current-command-line-arguments))

; configurations
(let* ([home (path->string (find-system-path 'home-dir))]
       [path (get-config home 'todo-dir (string-append home "Documents/") )])
   (main "" (file-name->MTC path "todo.txt")))



