#lang racket

(require rackunit "tools.rkt" "actions.rkt" "faily.rkt")


; Testing MTC%

(let* ([m0 (new-MTC)]
       [item1 "this is the first item"]
       [m1 (add m0 item1 )]
       [m1a (done m1)]
       [item2 "a second item"]
       [m2 (add m1 item2)]
       [m2a (add-front m2 "FIRST!!!")]
       [m3 (delay m2)]
       [m4 (add* m2 "hello teenage america" "another green world")]
       [m4a (load-items m2 '("hello teenage america" "another green world"))]
       [m5 (delay-by m4 2)]
       [m5a (delay-by m4 50)]
       [m6 (over-report m1 "hello world")]
       [m7 (pull-to-front m5 (λ (s) (regexp-match (pregexp "item") s)) "Pulling item to front")]       
       [m8 (throw-to-back m5 (λ (s) (regexp-match (pregexp "item") s)) "Throwing item to back")]
       [m9 (edit m4 "EXTRA WORDS WITH SPACES")]
       [m10 (kill m8 "item")]
       
       [m11 (over-path m0 "a/path/to/")]
       [m12 (over-file-name m11 "todo.txt")]
       [m13 (over-file-path m0 "another/path/" "file.tmp")]
       [m14 (pull-last m4)]
       
      )
  
  (check-equal? (is-empty? m0) true)
  (check-equal? (count m0) 0)
  (check-equal? (make-file-path m0) "")
  (check-equal? (next m1) item1)
  (check-equal? (is-empty? m1) false)
  (check-equal? (MTC-report m1) "Added : this is the first item")
  (check-equal? (is-empty? m1a) true)
  (check-equal? (MTC-report m1a) "Done : this is the first item")
  (check-equal? (next m2) item1)
  (check-equal? (next m2a) "FIRST!!!")
  (check-equal? (count m2a) 3)
  (check-equal? (MTC-report m2a) "Added (first) : FIRST!!!")
  (check-equal? (next m3) item2)
  (check-equal? (MTC-report m3) "Delayed : this is the first item")
  (check-equal? (MTC-items m4) '("this is the first item" "a second item" "hello teenage america" "another green world"))
  (check-equal? (MTC-items m4a) '("this is the first item" "a second item" "hello teenage america" "another green world"))
  (check-equal? (MTC-items m5) '("a second item" "hello teenage america" "this is the first item" "another green world"))
  (check-equal? (MTC-report m5) "Delayed by 2 : this is the first item")
  (check-equal? (MTC-items m5a) '("a second item" "hello teenage america" "another green world" "this is the first item"))
  (check-equal? (MTC-report m5a) "Delayed to end : this is the first item")
  (check-equal? (count m5) 4)
  (check-equal? (MTC-report m6) "hello world")
  (check-equal? (MTC-items m7) '("a second item" "this is the first item" "hello teenage america" "another green world"))
  (check-equal? (MTC-report m7) "Pulling item to front")
  (check-equal? (MTC-items m8) '( "hello teenage america" "another green world" "a second item" "this is the first item"))
  (check-equal? (MTC-report m8) "Throwing item to back")  
  (check-equal? (MTC-report m9) "Appended EXTRA WORDS WITH SPACES to this is the first item")
  (check-equal? (MTC-items m9) '("this is the first item EXTRA WORDS WITH SPACES" "a second item" "hello teenage america" "another green world"))
  (check-equal? (MTC-items m10) '( "hello teenage america" "another green world"))
  (check-equal? (MTC-report m10) "Kill*ed ''item''")
  (check-equal? (MTC-path m11) "a/path/to/")
  (check-equal? (MTC-file-name m12) "todo.txt")
  (check-equal? (make-file-path m12) "a/path/to/todo.txt")
  (check-equal? (make-file-path m13) "another/path/file.tmp")
  (check-equal? (MTC-items m14) '("another green world" "this is the first item" "a second item" "hello teenage america"))
  )

(let* ([m0 (new-MTC)]
       [m1 (load-items m0 (map number->string (range 100)))])
  (check-equal? (count m1) 100)
  (check-equal? (next m1) "0")
  (check-equal? (take (MTC-items (delay-pattern-by m1 (λ (s) (regexp-match (pregexp "0") s)) 10 "a report")) 13)
                '("1" "2" "3" "4" "5" "6" "7" "8" "9" "11" "0" "10" "20"))
  )

; Testing actions

; URLS
(let* ([x 1])
  (check-equal? (has-url "") #f)
  (check-equal? (has-url "fdfbksdj fkdsjf sdkhto psfksdj +fgsk jf") #f)
  (check-equal? (has-url "this contains https://url.com and other text") #t)
  (check-equal? (has-url "this contains https://url.com and other text another http://another.net") #t)
  
  (check-equal? (open (extract-url "this contains https://url.com and other text")) "https://url.com")
  (check-equal? (open (extract-url "this contains http://url.com and other text")) "http://url.com")
  (check-equal? (open (extract-url "this contains http://url.com and other text another http://another.net")) "http://url.com")  
  
  )

; Pages
(let* ([mtc (over-path (new-MTC) "/home/phil/pages/")]
       [pp (page-path mtc "hello-world")])
  (check-equal? pp "/home/phil/pages/hello-world.txt")
  )
