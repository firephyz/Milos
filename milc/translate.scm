(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (translate expr env)
  ;; Creates a closure of the form:
  ;; (body args . env)
  (define (create-closure body args env)
    (list* body args env))
  ;; TRANSLATE
  (if (atom? expr)
      ;; Variable
      (func-lookup expr)
      (let ([temp (car expr)])
	(if (and (not (atom? temp))
		 (not (eq? temp 'λ)))
	    ;; Application
	    (let ([func (translate (car expr))]
		  [args (map translate (cdr expr))])
	      (func-application func args))
	    ;; Function
	    (let* ([body (translate (caddr expr))]
		   [closure (create-closure body
					    (cadr expr)
					    env)])
	      (func-closure closure))))))

(define (func-lookup expr)
  (lambda (env)
    (lookup-var expr env)))

(define (func-application func-translated params)
  (lambda (env)
    (let* ([func (func-translated)]
	   [body (car func)]
	   [args (cadr func)]
	   [env (cddr func)]
	   [new-env (build-env args params env)])
      (body new-env))))

(define (func-closure closure)
  (lambda (env)
    closure))

;;
;; Runtime
;;

(define (build-env args values env)
  (define (pair-args args values)
    (if (null? args)
	'()
	(cons (cons (car args)
		    (car values))
	      (pair-args (cdr args)
			 (cdr values)))))
  (cons (pair-args args values) env))

(define (lookup-var var env)
  (define (look-in-frame var frame)
    (if (null? frame)
	'()
	(if (eq? var (caar frame))
	    (cdar frame)
	    (look-in-frame var (cdr frame)))))
  (if (null? env)
      '()
      (let ([result (look-in-frame var (car env))])
	(if (null? result)
	    (lookup-var var (cdr env))
	    result))))
