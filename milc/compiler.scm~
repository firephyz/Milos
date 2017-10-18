#lang racket

(define (machine-compile expr env)
  (if (pair? expr)
      (let ([token (car expr)])
        (if (eq? token 'lambda)
            (compile-function (caddr expr)
                              (cons (cadr expr)
                                    env))
            (compile-application expr env)))
      (if (symbol? expr)
          (list 'LOOKUP
                (get-symbol-index expr env))
          (list 'NUM expr))))

(define (compile-function body env)
  (list (machine-compile body env) 'CALL))

(define (compile-application expr env)
  (if (null? expr)
      '(RET)
      (cons (machine-compile (car expr) env)
            (compile-application (cdr expr) env))))

(define (get-symbol-index var env)
  (define (search-frame frame dist)
    (if (null? frame)
        '()
        (if (eq? var (car frame))
            dist
            (search-frame (cdr frame) (+ dist 1)))))
  (define (search-env env depth)
    (if (null? env)
        (error "Var not found!")
        (let ([result (search-frame (car env) 0)])
          (if (null? result)
              (search-env (cdr env) (+ depth 1))
              (cons depth result)))))
  (search-env env 0))
          