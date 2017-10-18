;;;
;;; Make parser function
;;;
(define (parse string)
  (define tokenizer
    (make-tokenizer (tokenize string)))
  (define (tokens->list has-dot)
    (let ([tk (tokenizer)])
      (case (car tk)
        ['right '()]
        ['dot (tokens->list #t)]
        [else
         (let ([result
                (if (eq? (car tk) 'left)
                    (tokens->list #f)
                    (string->internal-data (cdr tk)))])
           (if has-dot
               (begin (tokenizer)
                      result)
               (cons result
                     (tokens->list #f))))])))
  (tokens->list #t))

(define (string->internal-data string)
  (let ([number-parse (string->number string)])
    (if number-parse
	number-parse
	(string->symbol string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokenizer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-tokenizer tks)
  (let ([tokens tks])
    (lambda ()
      (if (null? tokens)
	  '()
	  (let ([result (car tokens)])
	    (set! tokens (cdr tokens))
	    result)))))

; Returns a list of tokens in the string
(define (tokenize string)
  (define symbol-terminals '(#\space #\tab #\( #\) #\. #\newline))
  (define white-space '(#\space #\tab #\newline))
  ; Returns if a character is white space
  (define (is-white-space char)
    (define (helper terminals)
      (if (null? terminals)
          #f
          (if (eq? char (car terminals))
              #t
              (helper (cdr terminals)))))
    (helper white-space))
  
  ; Returns token and next index
  (define (determine-type string)
    ; Return type of determine type.
    (define (new-token-meta type data index)
      (cons (cons type
                  data)
            index))
    ; Parses a string for the next symbol
    ; Returns (symbol-string . next-token-index)
    (define (parse-symbol string)
      (let ([index (index-of-chars string symbol-terminals)])
        (cons (substring string 0 index)
              index)))
    (let ([char (string-ref string 0)])
      (if (is-white-space char)
          '()
          (case char
            [(#\() (new-token-meta 'left '() 1)]
            [(#\)) (new-token-meta 'right '() 1)]
            [(#\.) (new-token-meta 'dot '() 1)]
            [else (let ([symbol (parse-symbol string)])
                    (new-token-meta 'symbol
                                    (car symbol)
                                    (cdr symbol)))]))))
  ;; Body of determine-type
  (if (= 0 (string-length string))
      '()
      (let ([result (determine-type string)])
        (if (null? result)
            (tokenize (substring string 1))
            (cons (car result)
                  (tokenize (substring string (cdr result))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Find the lowest index of one of the characters in @chars
; occuring in @string
(define (index-of-chars string chars)
  ; Find the index of a character occuring in a string
  (define (index-of char string)
    (define (helper char string count)
      (if (= 0 (string-length string))
          -1
          (if (eq? char (string-ref string 0))
              count
              (helper char
                      (substring string 1)
                      (+ count 1)))))
    (helper char string 0))
  ; Find the minimum number in a list
  (define (list-min arg)
    (define (helper arg result)
      (if (null? arg)
          result
          (let ([new-min (if (< (car arg) result)
                             (car arg)
                             result)])
            (helper (cdr arg) new-min))))
    (if (null? arg)
        '()
        (helper (cdr arg) (car arg))))
  (define (helper string chars)
    (if (null? chars)
        '()
        (let ([index (index-of (car chars) string)])
          (if (= index -1)
              (helper string (cdr chars))
              (cons index
                    (helper string (cdr chars)))))))
  (let ([indices (helper string chars)])
    (let ([result (list-min indices)])
      (if (null? result)
          (string-length string)
          result))))

