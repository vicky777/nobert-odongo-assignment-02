#lang racket
(require data-science-master)
(require plot)
(require math)
(require json)

;; Procedure called to prompt user for input
(define (input-prompt string)
  (newline)
  (display string))

;; Procedure to capture user input
(define (cin>>)
  (read))

;; Capture Country
(input-prompt "Enter country: ")
(define country (cin>>))

;; Capture Duration
(input-prompt "Enter months YYYY-MM-DD HH:MM:SS  ")
(define duration (cin>>))
(newline)

;; Make massmine query call


