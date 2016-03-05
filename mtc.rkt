#lang racket

(require "tools.rkt")

; UI Code
(define (safe-take xs n) 
  (let ([ln (length xs)])
    (if (< n ln) 
      (take xs n) 
      (take xs ln) )))

(define (display-state input mtc)
  (displayln (send mtc get-report))
  (displayln (match input
    ["ll" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (safe-take (send mtc get-items) 10))]
    ["l" (foldl (lambda (s rest) (string-append rest "\n" s)) "" (send mtc get-items))]
    ["h" "Commands

s   : SAVE ... this is the most important command. If you do not explicitly save, you will lose what you put into MTC.
h   : Show the list of commands
/   : delay the current next-item ie. push it to the end of the queue.
//  : push the current next-item 10 back. Once your queue gets long you have things that ought to be done ''now-ish'', but aren't your next task. This is for those. |
/// : push current next-item 50 back. When your queue gets big, this becomes meaningful. Depending on how long your tasks are, this is effectively pushing the current one until the end of the afternoon or until next week. If your queue is really big, the single / can push the task months back.
*   : done / delete. There's no difference. I used to store ''done'' items. But actually, you really don't care. MTC isn't about admiring long lists of crossed-out completed work. It's to manage the things you still have to do.
+ TEXT : finds ALL items that contain TEXT and pulls them to the front of the queue. There is no other search or filter in MTC. This is how you do search / filtering type stuff. NB .. TEXT can not have any spaces in it. At this time, it's only possible to pull a single search term. However, because pulling preserves ordering, if you pull a term like OFFICE and then pull a term like PARTY, you *will* get the items that match both OFFICE and PARTY at the top of the queue.
- TEXT : finds ALL items that contain TEXT and pushes them to the end of the queue. The opposite of + ... this is for getting rid of an entire project if you know it's not something you want to think about at the moment. You can always pull it back to the front (ie. prioritize it) later.
l   : List. You really shouldn't use this command. But occassionally you do need to scan the entire list. l lets you do it
ll  : a quick peek ahead at the first 10 items on queue
k* TEXT : be careful. This is multi-kill or bulk delete. It removes all items from the list that contain TEXT. Useful if you find that you've cancelled a project and want to remove all items from it from the queue.
e EXTRA TEXT : appends EXTRA TEXT to the next item. There is currently no general edit item in MTC. But it's sometimes useful to be able to append some kind of extra tag or keyword to an existing item."]
               
    [_ (if (not (send mtc is-empty?)) 
           (string-append "Next item : " (send mtc next))
           "No items")] ))
  )
    

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
      [(string=? op "e") (send mtc edit (string-join (cdr command) " " ))]
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
               (display-lines-to-file (send mtc get-items) (send mtc get-file-path) 
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

;;; Files

(define (file-name->MTC f-name)   
  (when (not (file-exists? f-name)) (display-to-file "" f-name))
  (send+ (new-MTC)
         (load-items (file->lines f-name))
         (over-file-path f-name)
         (over-report (string-append "Welcome to Mind Traffic Control
File is " f-name))))

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
       [mtc-path (get-config home 'todo-dir (string-append home "Documents/") )]
       [f-name (string-append mtc-path "todo.txt") ])
   (main "" (file-name->MTC f-name)))



