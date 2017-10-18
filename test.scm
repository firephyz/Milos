(define Y
  (lambda (f)
    ((lambda (x)
       (f (lambda (a) ((x x) a))))
     (lambda (x)
       (f (lambda (a) ((x x) a)))))))

(define fact*
  (lambda (f)
    (lambda (n)
      (if (= n 1)
	  1
	  (* n (f (- n 1)))))))

;(letr fact* (lambda (n)
;	      (if (= n 1)
;		  1
;		  (* n (fact* (- n 1)))))
;      ())

;(letr name func body)

;((lambda (name)
;   body)
; (Y (lambda (name)
;      func)))

((lambda (fact)
   (fact 5))
 (Y (lambda (fact)
      (lambda (n)
	(if (= n 1)
	    1
	    (* n (fact (- n 1))))))))

(define-syntax letr
  (syntax-rules ()
    ((letr name func body)
     ((lambda (name)
	body)
      (Y (lambda (name)
	   func))))))

(letr test
      (lambda (n)
	(if (= n 1)
	    1
	    (* n (test (- n 1)))))
      (let ((result (test 5)))
	(display result)
	(newline)))

