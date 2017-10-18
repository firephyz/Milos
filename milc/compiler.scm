(define (flatten-code code pc result-start result-end)
  (if (null? code)
      result-start
      (let ((next-segment-offset (+ pc (length code)))
            (instr (car code)))
        (if (eq? (car instr) 'FUNC)
            (let ((func-body (cadr instr)))
              (set-cdr! instr next-segment-offset)
              (flatten-code (append (cdr code)
                                    func-body)
                            next-segment-offset
                            result-start
                            (set-cdr! result-end (list instr))))
            (flatten-code (cdr code)
                          pc
                          result-start
                          (set-cdr! result-end (list (instr))))))))

;; Main compile function
(define (machine-compile expr env)
  (if (pair? expr)
      (let ((token (car expr)))
        (cond ((eq? token 'lambda)
               (compile-function (caddr expr)
                                 (cons (cadr expr)
                                       env)))
              ((eq? token 'if)
               (compile-if-statement (cadr expr)
                                     (caddr expr)
                                     (cadddr expr)
                                     env))
              (else (compile-application expr env))))
      (if (symbol? expr)
          (let ((built-in? (compile-built-in expr)))
            (if (null? built-in?)
                (let ((index (get-symbol-index expr env)))
                  (list (list 'LOOKUP
                              (car index)
                              (cdr index))))
                built-in?))
          (list (list 'NUM expr)))))

;; Function
(define (compile-function body env)
  (list (append '(FUNC)
                (machine-compile body env)
                '((RET)))))

;; Application
(define (compile-application expr env)
  (define (helper result expr)
    (if (null? expr)
        result
        (helper (append (machine-compile (car expr) env)
                        result)
                (cdr expr))))
  (if (null? (compile-built-in (car expr)))
      ;; Not a built-in
      (helper '((CALL)) expr)
      ;; Is a built-in
      (helper '() expr)))

;; If statement
(define (compile-if-statement pred then else env)
  (let ((pred-compiled (machine-compile pred env))
        (then-compiled (machine-compile then env))
        (else-complied (machine-compile else env)))
    (append pred-compiled
            (list 'JIF
                  (+ 1 (length then-compiled)))
            then-compiled
            (list 'JUMP
                  (+ 1 (length else-compiled)))
            else-compiled)))

;; Find the environment index of a given symbol
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
        (let ((result (search-frame (car env) 0)))
          (if (null? result)
              (search-env (cdr env) (+ depth 1))
              (cons depth result)))))
  (search-env env 0))

;; Built-in functions
(define (compile-built-in expr)
  (let ((result (cond ((eq? expr '+) '(ADD))
        ((eq? expr '-) '(SUB))
        ((eq? expr '*) '(MULT))
        ((eq? expr '/) '(DIV))
        (else '()))))
    (if (null? result)
        '()
        (list result))))

(define result (cons 'result '()))
(flatten-code (machine-compile '((lambda (g n) (+ n (g))) (lambda () 5) 3) '()) 0 result result)