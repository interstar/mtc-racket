#lang racket

(require "tools.rkt" (planet neil/html-parsing:2:0)
         net/url xml/path xml)

(provide has-url extract-url grab-url url->bytes url->xexp title-from-url analyze
         page-path page-exists?)

;;;; URLs
(define (has-url s) (regexp-match? (pregexp "(https?\\S+)") s))

(define (extract-url s)
  (let* ([r (regexp-match (pregexp "(https?\\S+)") s)])
    (car r)))

;;; Sites
(define (grab-url url) (get-pure-port (string->url url) #:redirections 2))
(define (url->bytes url) (port->bytes (grab-url url)))
(define (url->xexp url) (html->xexp (grab-url url)))


;; temporarily doing my own search here until I can get pkg installation working properly and install sxml package
(define (search-sxml key doc)
  (cond
    [(empty? doc) ""]
    [(list? doc) 
     (if (eq? (car doc) key) (cdr doc)
         (filter (lambda (x) (not (or (equal? x "") (equal? x '())))) (map (curry search-sxml key) doc)))]
    [else ""]))

(define (title-from-url url) 
  (let* ([xexp (url->xexp url)])
    (search-sxml 'title xexp  )))

(define (analyze item) 
  (let* ([url (extract-url item)])
    (~a (car (flatten (title-from-url url))))
    ))

;;; Files

(define (page-path mtc page-name) (string-append (make-file-path mtc) page-name ".txt"))
(define (page-exists? mtc page-name) (file-exists? (page-path mtc page-name)))
                           