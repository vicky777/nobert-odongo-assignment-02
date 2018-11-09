;; Odongo Nobert louis
;; 2018U/HD05/2018
;; End of Sem Assignment 02

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
  (read-line))

;; Procedure converts
(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record) (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))

;; Procedure to Perform preprocessing on the json data read into memory
(define (preprocess-text lst)
  (map (λ (x)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x)))))
       lst))

;; Read list of countries from list to facilitate searching by application user.
;; the list of countries will be processed to retrieve  a country-woeid pair list.
(define countries-raw (string->jsexpr
                (with-input-from-file "countries.json" (λ () (json-lines->json-array)))))

(define countries-list
  (let ([tmp (map (λ (x) (list (hash-ref x 'country) (hash-ref x 'woeid))) (flatten countries-raw))])
    tmp))

;(display countries-list)

(define (country? c)
  (let ([tmp_ (map (lambda (cl)
         (string=? (string-downcase c) (string-downcase (car cl))))
       countries-list)])
    tmp_))

;; Perform preprocessing on read list of countries.
;;(define countries-cleaned (preprocess-text countries-list))


;; Capture Country
(input-prompt "Enter country: ")
(define country (cin>>))
(display (string? country))

;;(display (if (member '#t (country? country))))


;; Make massmine query call with country woeid
(define (run-query)(""))
;; end of massmine query





;; Load tweeter data into memory from file.
(define tweets (string->jsexpr
                (with-input-from-file "trump.json" (λ () (json-lines->json-array)))))


(define tweet-list
  (let ([tmp (map (λ (x) (list (hash-ref x 'text))) tweets)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))


;; Reduce the two level list of lists of tweet texts to a single level of list of tweet texts.
(define flattened-list (flatten tweet-list))

;; Procedure for aggregating the list of tweet texts into a single string
(define (append-tweet-texts t-list) 
   (cond 
     [(null? t-list) ""] 
     [else (string-append (car t-list) 
                          (append-tweet-texts (cdr t-list)))])) 

;; Store sggregated tweet texts into a single string variable
(define tweet-texts (append-tweet-texts flattened-list))

;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document
(define words (document->tokens tweet-texts #:sort? #t))

;;; Using the nrc lexicon, we label each (non stop-word) with an
;;; emotional label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))

;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))

;;; Visualizing the result as a barplot (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))