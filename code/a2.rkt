#lang racket/load

(load "a1.rkt")

; 1
(define list-ref
  (lambda (ls n)
    (letrec
        ([nth-cdr (lambda (n)
                      (if (= n 0) 
                          ls 
                          (cdr (nth-cdr (- n 1)))))])
      (car (nth-cdr n)))))

(println (list-ref '(a b c) 2))
(println (list-ref '(a b c) 0))


; 2
(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(null? set2) set1]
      [(memv (car set2) set1) (union set1 (cdr set2))]
      [else (union (cons (car set2) set1) (cdr set2))])))

(display (union '() '()))
(newline)
(display (union '(x) '()))
(newline)
(display (union '(x) '(x)))
(newline)
(display (union '(x y) '(x z)))
(newline)

; 3
(define stretch
  (lambda (pred alternative)
    (lambda (n)
      (or (pred n) (eqv? alternative n)))))

(display ((stretch even? 1) 0))
(newline)
(display ((stretch even? 1) 1))
(newline)
(display ((stretch even? 1) 2))
(newline)
(display ((stretch even? 1) 3))
(newline)
(display (filter (stretch even? 1) '(0 1 2 3 4 5)))
(newline)
(display (filter (stretch (stretch even? 1) 3) '(0 1 2 3 4 5)))
(newline)
(display (filter (stretch (stretch (stretch even? 1) 3) 7) '(0 1 2 3 4 5)))
(newline)

; 4 
(define walk-symbol
  (lambda (s lst)
    (let ([p (assv s lst)])
      (cond
        [(boolean? p) s]
        [(symbol? (cdr p)) (walk-symbol (cdr p) lst)]
        [else (cdr p)]))))

(display (walk-symbol 'a '((a . 5))))
(newline)
(display (walk-symbol 'a '((b . c) (a . b))))
(newline)
(display (walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e))))
(newline)
(display (walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e))))
(newline)


; 5
(define lambda-exp?
  (lambda (E)
    (letrec
      ([p
        (lambda (e)
          (match e
            [`(lambda (,x) ,body) (and (symbol? x) (p body))]
            [`(,rator ,rand . ,more) (and (p rator) (p rand) (null? more))]
            [`,y #t]
            [else #f]))])
      (p E))))

(display (lambda-exp? 'x))
(newline) ; #t
(display (lambda-exp? '(lambda (x) x)))
(newline) ; #t
(display (lambda-exp? '(lambda (f) (lambda (x) (f (x x))))))
(newline) ; #t
(display (lambda-exp? '(lambda (x) (lambda (y) (y x)))))
(newline) ; #t
(display (lambda-exp? '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a)))))))
(newline) ; #t
(display (lambda-exp? '(lambda (lambda) lambda)))
(newline) ; #t
(display (lambda-exp? '((lambda (lambda) lambda) (lambda (y) y))))
(newline); #t
(display (lambda-exp? '((lambda (x) x) (lambda (x) x))))
(newline); #t
(display (lambda-exp? '((lambda (5) x) (lambda (x) x))))
(newline) ; #f
(display (lambda-exp? '((lambda (x) x) (lambda (x) x) (lambda (x) x))))
(newline) ; #f
(display (lambda-exp? '((lambda (lambda (x) x) x)  (lambda (x) x))))
(newline) ; #f
(display (lambda-exp? '(y x z)))
(newline) ; #f


; 6 
(define var-occurs?
  (lambda (v E)
    (define p
      (lambda (e)
        (match e
          [`(lambda (,x) ,body) (p body)]     ; not as parameter
          [`(,rator ,rand . ,more) (or (p rator) (p rand))]
          [`,y (eqv? y v)])))
    (p E)))

(display "var-occurs")
(newline)
(display (var-occurs? 'x 'x))
(newline) ; #t
(display (var-occurs? 'x '(lambda (x) y)))
(newline) ; #f
(display (var-occurs? 'x '(lambda (y) x)))
(newline) ; #t
(display (var-occurs? 'x '((z y) x)))
(newline) ; #t


; 7 
(define vars
  (lambda (E)
    (define p
      (lambda (e col)
        (match e
          [`(lambda (,x) ,body) (p body col)]     ; not as parameter
          [`(,rator ,rand) (p rator (p rand col))]
          [`,y (cons y col)])))
    (p E '())))

(display (vars 'x))
(newline)
(display (vars '(lambda (x) x)))
(newline)
(display (vars '((lambda (y) (x x)) (x y))))
(newline)
(display (vars '(lambda (z) ((lambda (y) (a z))
                      (h (lambda (x) (h a)))))))
(newline)

; 8
(define unique-vars
  (lambda (E)
    (define p
      (lambda (e col)
        (match e
          [`(lambda (,x) ,body) (p body col)]     ; not as parameter
          [`(,rator ,rand) (p rator (p rand col))]
          [`,y (if (memv y col) col (cons y col))])))
    (p E '())))

(display (unique-vars '((lambda (y) (x x)) (x y))))
(newline)
(display (unique-vars '((lambda (z) (lambda (y) (z y))) x)))
(newline)
(display (unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a)))))
(newline)

; 9 
(define var-occurs-free?
  (lambda (v E)
    (define p
      (lambda (e)
        (match e
          [`(lambda (,x) ,body) (and (not (eqv? x v)) (p body))]     ; not as parameter
          [`(,rator ,rand . ,more) (or (p rator) (p rand))]
          [`,y (eqv? y v)])))
    (p E)))

(display (var-occurs-free? 'x 'x))
(newline) ; #t
(display (var-occurs-free? 'x '(lambda (y) y)))
(newline) ; #f
(display (var-occurs-free? 'x '(lambda (x) (x y))))
(newline) ; #f
(display (var-occurs-free? 'x '(lambda (x) (lambda (x) x))) )
(newline) ; #f
(display (var-occurs-free? 'y '(lambda (x) (x y))))
(newline) ; #t
(display (var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y)))))
(newline) ; #t
(display (var-occurs-free? 'x '((lambda (x) (x x)) (x x))))
(newline) ; #t


; 10 
(define var-occurs-bound?
  (lambda (v E)
    (define p
      (lambda (e)
        (match e
          [`(lambda (,x) ,body) (or (and (eqv? x v) (var-occurs? v body)) (p body))]     ; not as parameter
          [`(,rator ,rand . ,more) (or (p rator) (p rand))]
          [`,y #f])))
    (p E)))

(display (var-occurs-bound? 'x 'x))
(newline) ;#f
(display (var-occurs-bound? 'x '(lambda (x) x)))
(newline) ;#t
(display (var-occurs-bound? 'y '(lambda (x) x)))
(newline) ;#f
(display (var-occurs-bound? 'x '((lambda (x) (x x)) (x x))))
(newline) ;#t
(display (var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z)))))
(newline) ;#f
(display (var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z)))))
(newline); #t
(display (var-occurs-bound? 'x '(lambda (x) y)))
(newline); #f
(display (var-occurs-bound? 'x '(lambda (x) (lambda (x) x))))
(newline); #t


; 11
(define unique-free-vars
  (lambda (E)
    (define p
      (lambda (e col bound)
        (match e
          [`(lambda (,x) ,body) (p body col (cons x bound))]   ; declare x is bound, so it is not free
          [`(,rator ,rand) (p rator (p rand col bound) bound)]
          [`,y (if (or (memv y col) (memv y bound)) col (cons y col))])))
    (p E '() '())))

(display (unique-free-vars 'x))
(newline) ; (x)
(display (unique-free-vars '(lambda (x) (x y))))
(newline); (y)
(display (unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c))))))))
(newline); (y e x)


; 12
(define unique-bound-vars
  (lambda (E)
    (define p
      (lambda (e col bound)
        (match e
          [`(lambda (,x) ,body) (p body col (cons x bound))]   ; declare x is bound, so it is not free
          [`(,rator ,rand) (p rator (p rand col bound) bound)]
          [`,y (if (or (memv y col) (not (memv y bound))) col (cons y col))])))
    (p E '() '())))

(display (unique-bound-vars 'x))
(newline); ()
(display (unique-bound-vars '(lambda (x) y)))
(newline); ()
(display (unique-bound-vars '(lambda (x) (x y))))
(newline); (x)
(display (unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c))))))))
(newline); (x c)
(display (unique-bound-vars '(lambda (y) y)))
(newline); (y)
(display (unique-bound-vars '(lambda (x) (y z))))
(newline); ()
(display (unique-bound-vars '(lambda (x) (lambda (x) x))))
(newline); (x)

; 13




; 13
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


; 14