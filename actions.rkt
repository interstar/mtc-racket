#lang racket

(require (planet neil/html-parsing:2:0)
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
(define (title-from-url url) 
  (let* ([xexp (url->xexp url)])
    (if #t
      (se-path* '(title) xexp)
      xexp)))

(define (analyze item) 
  (let* ([url (extract-url item)])
    (title-from-url url)
    ))

;;; Files

(define (page-path mtc page-name) (string-append (send mtc get-file-path) page-name ".txt"))
(define (page-exists? mtc page-name) (file-exists? (page-path mtc page-name)))
                           