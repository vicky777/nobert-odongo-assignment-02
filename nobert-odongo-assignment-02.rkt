#lang racket
(require data-science-master)
(require plot)
(require math)
(require json)

;; Procedure call to prompt user for input
(define (input-prompt string)
  (newline)
  (display string))

;; Procedure to capture user input
(define (cin>>)
  (read))

;; Capture Country
(input-prompt "Enter country: ")
(define country (cin>>))

;; Make massmine query call


(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record) (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))

(define (preprocess-text lst)
  (map (λ (x)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x)))))
       lst))

(define tweets (string->jsexpr
                (with-input-from-file "trump.json" (λ () (json-lines->json-array)))))

;(preprocess-text tweets)
;(display tweets)


(define tweet-text
  (let ([tmp (map (λ (x) (list (hash-ref x 'text) (hash-ref x 'created_at))) tweets)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))

(define words (document->tokens t #:sort? #t))