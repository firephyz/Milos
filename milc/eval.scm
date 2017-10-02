(load "parse.scm")
(load "string.scm")

(define (eval-file name)
  (define initial-environment '())
  ;; Opens the file, reads it in and parses
  ;; it into a list
  (let* ([input (open-input-file name)]
	 [input-string (file->string input)]
	 [input-list (parse input-string)])
    (eval input-list
	  initial-environment)))
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
