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
    ["llll" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (safe-take (MTC-items mtc) 500))]                              
    ["lll" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (safe-take (MTC-items mtc) 50))]               
    ["ll" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (safe-take (MTC-items mtc) 10))]               
    ["l" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (MTC-items mtc))]
    ["h" "Commands

  s\t\tSAVE : If you do not explicitly save, you will lose what you put into MTC.
  h\t\tHelp : Shows this list of commands 
  /\t\tDelay the current next-item ie. Push it to the end of the queue.
  //\t\tPush the current next-item 10 back.
  ///\t\tPush current next-item 50 back.
  ////\t\tPush current next-item 500 back.
  *\t\tDone / Delete. There's no difference. 
  c\t\tCount items in queue
  + TEXT\tfinds ALL items that contain TEXT and pulls them to the front of the queue. 
  - TEXT\tfinds ALL items that contain TEXT and pushes them to the end of the queue.
  -- TEXT\tfinds ALL items that contain TEXT and pushes them 10 back
  --- TEXT\tfinds ALL items that contain TEXT and pushes them 50 back
  ---- TEXT\tfinds ALL items that contain TEXT and pushes them 500 back
  l\t\tShows the entire queue.
  ll\t\tA quick peek ahead at the first 10 items on queue.
  lll\t\tPeek ahead to first 50 items
  llll\t\tPeek ahead to first 500 items
  \\\t\tPull the last item to the front
  \\\\ ITEM TEXT\tAdd the new item at the front of the queue
  k* TEXT\tMulti-kill or bulk delete. It removes all items from the list that contain TEXT.
  e EXTRA TEXT\tAppends EXTRA TEXT to the next item.
  a\t\tAnalyze a URL in an item (pull and show its title)
"]
               
              
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

      [(string=? op "----") (delay-pattern-by
                             mtc (λ (s) (regexp-match (pregexp arg) s)) 500 (string-append "Pushed all " arg " 500 back"))]
      [(string=? op "---") (delay-pattern-by
                            mtc (λ (s) (regexp-match (pregexp arg) s)) 50 (string-append "Pushed all " arg " 50 back"))]
      [(string=? op "--") (delay-pattern-by
                           mtc (λ (s) (regexp-match (pregexp arg) s)) 10 (string-append "Pushed all " arg " 10 back "))]

      [(string=? op "-") (throw-to-back mtc  (λ (s) (regexp-match (pregexp arg) s)) (string-append "Thrown " arg))]
 
      [(string=? op "e") (edit mtc args)]
      [(string=? op "q") 
       (over-report mtc 
             (if (page-exists? mtc arg) 
           (string-append "page " (page-path mtc arg) " exists")
           (string-append "page " (page-path mtc arg) " DOESN'T exist")))]
      [(string=? op "k*") (kill mtc  arg)]
      [(string=? op "\\\\") (over-report (add-front mtc args) (string-append "Added in front : " args))]      
      [else (add mtc input)])))
      
(define (process-short input mtc)
  (let* ([reply (lambda (s) (over-report mtc s))])
    (if (> (string-length input) 5) (add mtc input) 
      (match input
        ["" (reply "")]
        ["////" (delay-by mtc  500)]        
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
        ["llll" (reply "First 500")]
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
  (let* ([config-path (string-append home-path "/bin/.mtc/config.rkt")]
         [path (string->path config-path)])
    (displayln (string-append "CONfiG PATH " config-path))
    (with-handlers ([exn:fail? (λ (e) default)])
      (let* ([s (file->value (string->path config-path))])
        (displayln s)
        (dict-ref s name default )))))



;;;; Imperative bit.

(define args (current-command-line-arguments))

; configurations
(let* ([home (path->string (find-system-path 'home-dir))]
       [path (get-config home 'todo-dir (string-append home "Documents/") )])
   (main "" (file-name->MTC path "todo.txt")))



