#lang racket

(require rackunit "tools.rkt" "actions.rkt")


; Testing MTC%

(let* ([m0 (new-MTC)]
       [item1 "this is the first item"]
       [m1 (send m0 add item1 )]
       [m1a (send m1 done)]
       [item2 "a second item"]
       [m2 (send m1 add item2)]
       [m2a (send m2 add-front "FIRST!!!")]
       [m3 (send m2 delay)]
       [m4 (send m2 add* "hello teenage america" "another green world")]
       [m4a (send m2 load-items '("hello teenage america" "another green world"))]
       [m5 (send m4 delay-by 2)]
       [m5a (send m4 delay-by 50)]
       [m6 (send m1 over-report "hello world")]
       [m7 (send m5 pull-to-front (λ (s) (regexp-match (pregexp "item") s)) "Pulling item to front")]       
       [m8 (send m5 throw-to-back (λ (s) (regexp-match (pregexp "item") s)) "Throwing item to back")]
       [m9 (send m4 edit "EXTRA WORDS WITH SPACES")]
       [m10 (send m8 kill "item")]
       
       [m11 (send m0 over-path "a/path/to/")]
       [m12 (send m11 over-file "todo.txt")]
       
      )
  
  (check-equal? (send m0 is-empty?) true)
  (check-equal? (send m0 count) 0)
  (check-equal? (send m0 get-file-path) "")
  (check-equal? (send m1 next) item1)
  (check-equal? (send m1 is-empty?) false)
  (check-equal? (send m1 get-report) "Added : this is the first item")
  (check-equal? (send m1a is-empty?) true)
  (check-equal? (send m1a get-report) "Done : this is the first item")
  (check-equal? (send m2 next) item1)
  (check-equal? (send m2a next) "FIRST!!!")
  (check-equal? (send m2a count) 3)
  (check-equal? (send m2a get-report) "Added (first) : FIRST!!!")
  (check-equal? (send m3 next) item2)
  (check-equal? (send m3 get-report) "Delayed : this is the first item")
  (check-equal? (send m4 get-items) '("this is the first item" "a second item" "hello teenage america" "another green world"))
  (check-equal? (send m4a get-items) '("this is the first item" "a second item" "hello teenage america" "another green world"))
  (check-equal? (send m5 get-items) '("a second item" "hello teenage america" "this is the first item" "another green world"))
  (check-equal? (send m5 get-report) "Delayed by 2 : this is the first item")
  (check-equal? (send m5a get-items) '("a second item" "hello teenage america" "another green world" "this is the first item"))
  (check-equal? (send m5a get-report) "Delayed to end : this is the first item")
  (check-equal? (send m5 count) 4)
  (check-equal? (send m6 get-report) "hello world")
  (check-equal? (send m7 get-items) '("a second item" "this is the first item" "hello teenage america" "another green world"))
  (check-equal? (send m7 get-report) "Pulling item to front")
  (check-equal? (send m8 get-items) '( "hello teenage america" "another green world" "a second item" "this is the first item"))
  (check-equal? (send m8 get-report) "Throwing item to back")  
  (check-equal? (send m9 get-report) "Appended EXTRA WORDS WITH SPACES to this is the first item")
  (check-equal? (send m9 get-items) '("this is the first item EXTRA WORDS WITH SPACES" "a second item" "hello teenage america" "another green world"))
  (check-equal? (send m10 get-items) '( "hello teenage america" "another green world"))
  (check-equal? (send m10 get-report) "Kill*ed ''item''")
  (check-equal? (send m11 get-file-path) "a/path/to/")
  (check-equal? (send m12 get-file) "todo.txt")
  (check-equal? (send m12 get-file-path) "a/path/to/todo.txt")
  )

; Testing actions

; URLS
(let* ([x 1])
  (check-equal? (has-url "") #f)
  (check-equal? (has-url "fdfbksdj fkdsjf sdkhto psfksdj +fgsk jf") #f)
  (check-equal? (has-url "this contains https://url.com and other text") #t)
  (check-equal? (has-url "this contains https://url.com and other text another http://another.net") #t)
  
  (check-equal? (extract-url "this contains https://url.com and other text") "https://url.com")
  (check-equal? (extract-url "this contains http://url.com and other text") "http://url.com")
  (check-equal? (extract-url "this contains http://url.com and other text another http://another.net") "http://url.com")  
  
  )

; Pages
(let* ([mtc (send+ (new-MTC) (over-file-path "/home/phil/pages/"))]
       [pp (page-path mtc "hello-world")])
  (check-equal? pp "/home/phil/pages/hello-world.txt")
  )