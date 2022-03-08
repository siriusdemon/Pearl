#lang racket/load
(load "a2.rkt")

;; Part 1
(define lex
  (lambda (e bound)
    (match e
      [`(lambda (,x) ,body) `(lambda ,[lex body (cons x bound)])]
      [`(,rator ,rand) `(,[lex rator bound] ,[lex rand bound])]
      [`,y (if (not (memv y bound)) y `(var ,[index-of bound y]))])))

(display (lex '(lambda (x) x) '()))
(newline); (lambda (var 0))

     
(display (lex '(lambda (y) (lambda (x) y)) '()))
(newline); (lambda (lambda (var 1)))
(display (lex '(lambda (y) (lambda (x) (x y))) '()))
(newline); (lambda (lambda ((var 0) (var 1))))
(display (lex '(lambda (x) (lambda (x) (x x))) '()))
(newline); (lambda (lambda ((var 0) (var 0))))
(display (lex '(lambda (x) (lambda (x) (y x))) '()))
(newline); (lambda (lambda (y (var 0))))
(display (lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()))
(newline); (lambda ((lambda ((var 0) (var 1))) (lambda (lambda ((var 2) (var 1))))))
(display (lex '(lambda (a)
          (lambda (b)
            (lambda (c)
              (lambda (a)
                (lambda (b)
                  (lambda (d)
                    (lambda (a)
                      (lambda (e)
                        (((((a b) c) d) e) a))))))))) 
       '()))
(newline)
; (lambda
;   (lambda
;     (lambda
;       (lambda
;         (lambda
;           (lambda
;             (lambda
;               (lambda
;                 ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (var 1))))))))))

(display (lex '(lambda (a)
          (lambda (b)
      	    (lambda (c)
	            (lambda (w)
	              (lambda (x)
            		  (lambda (y)
		                ((lambda (a)
		                  (lambda (b)
                			 (lambda (c)
                			   (((((a b) c) w) x) y))))
		                 (lambda (w)
            		       (lambda (x)
			                   (lambda (y)
                  			   (((((a b) c) w) x) y))))))))))) 
       '()))
(newline)
; (lambda 
;   (lambda 
;     (lambda 
;       (lambda 
;       	(lambda 
;           (lambda 
;             ((lambda
;               (lambda
;                 (lambda
;                   ((((((var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
;             (lambda
;               (lambda
;                 (lambda
;                   ((((((var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0))))))))))))

(display (lex '(lambda (a)
          (lambda (b)
            (lambda (c)
              (lambda (w)
                (lambda (x)
                  (lambda (y)
                    ((lambda (a)
                      (lambda (b)
                        (lambda (c)
                          (((((a b) c) w) x) y))))
                    (lambda (w)
                      (lambda (x)
                        (lambda (y)
                          (((((a b) c) w) h) y))))))))))) 
       '()))
(newline)
; '(lambda 
;    (lambda
;      (lambda 
;        (lambda
;          (lambda 
;            (lambda
;              ((lambda 
;                 (lambda 
;                   (lambda ((((((var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
;               (lambda 
;                 (lambda 
;                   (lambda ((((((var 8) (var 7)) (var 6)) (var 2)) h) (var 0))))))))))))

;; Part 2

(define empty-env-fn 
  (lambda ()
    (lambda (x) 
      (error 'empty-env-fn "Variable unbound ~s~n" x))))

(define extend-env-fn
  (lambda (sym val old-env)
    (lambda (x)
      (if (eqv? x sym)
          val
          (old-env x)))))

(define apply-env-fn
  (lambda (env x)
    (env x)))

(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`,y #:when (symbol? y) (apply-env-fn env y)]
      [`(if ,test ,conseq ,alt) 
        (if (value-of-fn test env)
            (value-of-fn conseq env)
            (value-of-fn alt env))]
      )))

(println 
  (value-of-fn
   '((lambda (x) (if (zero? x)
                     #t
                     #f))
     0)
   (empty-env-fn)))
; #t                  
(println 
  (value-of-fn 
   '((lambda (x) (if (zero? x) 
                     12 
                     47)) 
     0) 
   (empty-env-fn)))
; 12    
(println 
  (value-of-fn
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (empty-env-fn)))
; 60
(println 
  (value-of-fn
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))
   (empty-env-fn)))
; 30
(println 
  (value-of-fn
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (empty-env-fn)))
; 25
(println 
  (value-of-fn
   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)
   (empty-env-fn)))
; 120