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
;;(input-prompt "Enter country: ")
;;(define country (cin>>))

;; Make massmine query call

;; end of massmine query

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


(define tweet-list
  (let ([tmp (map (λ (x) (list (hash-ref x 'text))) tweets)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))



(define first-item (car tweet-list))
(list? first-item)
(define tweet-t (car first-item))
(string? tweet-t)


(define (agg-texts list)
  (cond
    [(empty? list) list]
    [else (
           (tweet-t (string-append (car (car tweet-list)))))]))

(agg-texts tweet-list)

;;(define words (document->tokens tweet-text #:sort? #t))