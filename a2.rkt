#lang racket

; 1
(define list-ref
  (lambda (ls n)
    (letrec
        ([nth-cdr (lambda (n)
                      (if (= n 0) 
                          ls 
                          (cdr (nth-cdr (- n 1)))))])
      (car (nth-cdr n)))))

(display (list-ref '(a b c) 2))
(newline)

(display (list-ref '(a b c) 0))
(newline)


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
    ))