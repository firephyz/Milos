#lang racket

(provide file->string)

;; Converts a given input port into a
;; string of characters
(define (file->string input)
  ;; Builds up the actual string
  (define (helper input string-builder)
    (let ([char (read-char input)])
      (if (eof-object? char)
          (string-builder '())
          (begin (string-builder char)
                 (helper input string-builder)))))
  (helper input (make-string-builder)))

;; Creates an object used to build
;; strings from characters
(define (make-string-builder)
  (define (expand-string src max-index)
    (define (copy-string index src-max-index dest)
      ;; Copy the src char to the dest
      (string-set! dest
                   index
                   (string-ref src index))
      ;; Either continue copying or
      ;; return the dest string
      (if (= index src-max-index)
          dest
          (copy-string (+ index 1) src-max-index dest)))
    ;; Copy the string over to a new one
    (let ([src-max-index (- (string-length src) 1)])
      (copy-string 0
                   src-max-index
                   (make-string max-index #\space))))
  ;;  Create the actual string-bulder
  (let* ([index 0]
         [max-index 4]
         [string (make-string max-index #\space)])
    (lambda (char)
      ;; Return the string if just given null
      (if (null? char)
          string
          ;; Otherwise, add the character to the string
          (begin (string-set! string
                              index
                              char)
                 (set! index (+ index 1))
                 ;; Expand the string if we need to
                 (if (= index max-index)
                     (begin (set! max-index (* max-index 2))
                            (set! string (expand-string string max-index)))
                     '())
                 string)))))