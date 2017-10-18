#lang racket

(require "parse.scm")
(require "string.scm")

(define (eval-file name)
  (define initial-environment '())
;  ;; Opens the file, reads it in and parses
  ;; it into a list
  (let* ([input (open-input-file name)]
	 [input-string (file->string input)]
	 [input-list (parse input-string)])
    (translate input-list)))

;;;
;;; Translator
;;;
(define (translate expr)
  (if (atom? expr)
      (if (number? expr)
	  expr
	  ;; Is a variable. Must lookup in environment
	  (lambda (env)
	    (lookup-var expr env)))
      (let ([temp (car expr)])
	(if (not (and (atom? temp)
		      (eq? temp 'λ)))
	    ;; Is an application. Apply the func to the argument
	    (let ([func (translate temp)]
		  [args (translate-args (cadr expr))])
	      (lambda (env)
		((lambda (func-result)
		   (let ([func-body (car func-result)]
			 [func-args (cdr func-result)])
		     (func-body (build-env func-args
					   args
					   env))))
		 (func env))))
	      ;; Is a function. Create a function unit.	    
	      (let ([body (translate (caddr expr))])
		(lambda () (cons body
				 (cadr expr)))))))))

(define (translate-args args)
  (if (null? args)
      '()
      (cons (translate (car args))
	    (translate-args (cdr args)))))

;;;
;;; Translator Runtime
;;;

(define (build-env args values env)
  (define (helper args values)
    (if (null? args)
	'()
	(cons (cons (car args)
		    (car values))
	      (helper (cdr args)
		      (cdr values)))))
  (cons (helper args values) env))

(define (lookup-var var env)
  (define (lookup-in-frame var frame)
    (if (null? frame)
	'()
	(if (eq? var (caar frame))
	    (cdar frame)
	    (lookup-in-frame var (cdr frame)))))
  (if (null? env)
      '()
      (let ([result (lookup-in-frame var (car env))])
	(if (null? result)
	    (lookup-var var (cdr env))
	    result))))
;;;
;;; Main evaluator
;;;

(define (eval expr env)
  (if (atom? expr)
      (eval-atom expr env)
      (let ([instr (car expr)])
        (if (and (atom? instr)
                 (eq? instr 'λ))
            (create-closure expr env)
            (apply (eval instr env)
                   (evlis (cdr expr) env)
                   env)))))

(define (apply func args env)
  (let ([new-env (extenv args
                         (closure-args func)
                         (closure-env func))])
    (eval (closure-body func) new-env)))

(define (eval-atom atom env)
  (if (literal? atom)
      atom
      (lookup atom env)))

(define (evlis args env)
  (if (null? args)
      '()
      (cons (eval (car args) env)
            (evlis (cdr args) env))))

(define (lookup var env)
  (define (sub-lookup var sub)
    (if (null? sub)
        '()
        (let ([binding (car sub)])
          (if (eq? var (car binding))
              (cdr binding)
              (sub-lookup var (cdr sub))))))
  (if (null? env)
      '()
      (let ([result (sub-lookup var (car env))])
        (if (null? result)
            (lookup var (cdr env))
            result))))

(define (extenv values args env)
  (define (create-bindings args values)
    (if (null? args)
        '()
        (cons (cons (car args)
                    (car values))
              (create-bindings (cdr args)
                               (cdr values)))))
  (let ([frame (create-bindings args values)])
    (cons frame env)))

;;;
;;; Helper functions
;;;

(define (atom? expr)
  (and (not (pair? expr))
       (not (null? expr))))

(define (literal? expr)
  (or (number? expr)
      (boolean? expr)))

(define (create-closure expr env)
  (cons env
        (cons (cadr expr)
              (caddr expr))))

(define (closure-body func)
  (cddr func))
(define (closure-args func)
  (cadr func))
(define (closure-env func)
  (car func))
