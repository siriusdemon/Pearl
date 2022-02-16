#lang racket/load
(load "a0.rkt")

; 1
(define countdown 
  (lambda (n)
    (if (= n 0)
        '(0)
        (cons n (countdown (- n 1))))))

(println (countdown 5))
(println (countdown 0))

; 2
(define insertL 
  (lambda (s1 s2 slist)
    (cond 
      [(null? slist) '()]
      [(eqv? (car slist) s1)
       (cons s2 
          (cons s1
              (insertL s1 s2 (cdr slist))))]
      [else (cons (car slist)
                  (insertL s1 s2 (cdr slist)))])))

(println (insertL 'x 'y '(x z z x y z)))

; 3
(define remv-lst
  (lambda (s lst)
    (cond 
      [(null? lst) '()]
      [(eqv? (car lst) s) (cdr lst)]
      [else (cons (car lst)
                (remv-lst s (cdr lst)))])))

(println (remv-lst 'x '(x y z x)))
(println (remv-lst 'y '(x y z y x)))
(println (remv-lst 'z '(a b c)))

; 4
(define map
  (lambda (f lst)
    (cond 
      [(null? lst) '()]
      [else (cons (f (car lst))
                (map f (cdr lst)))])))

(println (map add1 '(1 2 3 4)))
(println (map sub1 '(1 2 3 4)))


; 5
(define filter
  (lambda (pred lst)
    (cond
      [(null? lst) '()]
      [(pred (car lst)) (cons (car lst)
                            (filter pred (cdr lst)))]
      [else (filter pred (cdr lst))])))

(println (filter even? '( 1 2 3 4 5 6)))

; 6
(define zip
  (lambda (lst1 lst2)
    (cond 
      [(or (null? lst1) (null? lst2)) '()]
      [else 
        (let ([s1 (car lst1)]
              [s2 (car lst2)])
          (cons (cons s1 s2)
            (zip (cdr lst1) (cdr lst2))))])))

(println (zip '(1 2 3) '(a b c)))
(println (zip '(1 2 3 4 5 6) '(a b c)))
(println (zip '(1 2 3) '(a b c d e f)))

; 7
(define list-index-ofv
  (lambda (s lst)
    (define helper
      (lambda (s lst index)
        (cond
          [(null? lst) (error "bad data")]
          [(eqv? (car lst) s) index]
          [else (helper s (cdr lst) (add1 index))])))
    (helper s lst 0)))

(println (list-index-ofv 'x '(x y z x x )))
(println (list-index-ofv 'x '(y z x x)))

; 8
(define append
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) lst2]
      [else (cons (car lst1)
              (append (cdr lst1) lst2))])))

(println (append '(42 120) '(1 2 3)))
(println (append '(a b c) '(cat dog)))


; 9
(define reverse
  (lambda (lst)
    (cond 
      [(null? lst) '()]
      [else (append (reverse (cdr lst))
              (list (car lst)))])))

(println (reverse '(a 3 x)))
(println (reverse '(a b c d e f g)))

; 10
(define repeat
  (lambda (lst n)
    (cond 
      [(= n 0) '()]
      [(= n 1) lst]
      [else (append lst (repeat lst (- n 1)))])))

(println (repeat '(4 8 11) 4))
(println (repeat '(4 8 11) 1))
(println (repeat '(4 8 11) 0))

; 11
(define same-lists*
  (lambda (lst1 lst2)
    (cond 
      [(and (null? lst1) (null? lst2)) #t]
      [(or (null? lst1) (null? lst2)) #f]
      [else (let ([s1 (car lst1)]
                  [s2 (car lst2)])
              (cond
                [(and (pair? s1) (pair? s2)) 
                 (and (same-lists* s1 s2)
                    (same-lists* (cdr lst1) (cdr lst2)))]
                [(or (pair? s1) (pair? s2)) #f]
                [(eqv? s1 s2) (same-lists* (cdr lst1) (cdr lst2))]))])))

(println (same-lists* '() '()))
(println (same-lists* '(1 2 3 4 5) '(1 2 3 4 5)))
(println (same-lists* '(1 2 3 5) '( 1 2 3)))
(println (same-lists* '(1 (2 3) 4) '(1 (2) 3 4)))
(println (same-lists* '((a) b (c d) d) '((a) b (c d ) d)))

; 12
; ((w x) y (z))
;  =>  ((w x) . (y (z)))
;  =>  ((w . (x . ())) . (y (z)))
;  =>  ((w . (x . ())) . (y . ((z . ()) . ())))
(println (equal? '((w x) y (z)) '((w . (x . ())) . (y . ((z . ()) . ())))))

; 13 
(define binary->natural
  (lambda (lst)
    (cond 
      [(null? lst) 0]
      [(= 1 (car lst)) (+ 1 (* 2 (binary->natural (cdr lst))))]
      [(= 0 (car lst)) (* 2 (binary->natural (cdr lst)))])))

;; this function do in normal bit order
; (define binary->natural2
;   (lambda (lst)
;     (define (helper lst acc)
;       (cond 
;         [(null? lst) acc]
;         [(= 1 (car lst)) (helper (cdr lst) (+ 1 (* 2 acc)))]
;         [(= 0 (car lst)) (helper (cdr lst) (* 2 acc))]))
;     (helper lst 0)))

(println (binary->natural '()))
(println (binary->natural '(0 0 1)))
(println (binary->natural '(0 0 1 1)))
(println (binary->natural '(1 1 1 1)))
(println (binary->natural '(1 0 1 0 1)))
(println (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)))


; 14
(define div
  (lambda (divor divee)
    (cond
      [(= 0 divee) (error "bad data")]
      [(= 1 divee) divor]
      [(= 0 divor) 0]
      [else (add1 (div (- divor divee) divee))])))

(println (div 25 5))
(println (div 36 6))


; 15
(define append-map
  (lambda (f lst)
    (cond
      [(null? lst) '()]
      [else (append (f (car lst))
                    (append-map f (cdr lst)))])))

(println (append-map countdown (countdown 5)))

; 16
(define set-difference
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(memv (car set1) set2) (set-difference (cdr set1) set2)]
      [else (cons (car set1) (set-difference (cdr set1) set2))])))

(println (set-difference '(1 2 3 4 5) '(2 4 6 8)))


; 17
;; fold-left
; (define foldl
;   (lambda (f init lst)
;     (cond
;       [(null? lst) init]
;       [else (let ([new-acc (f (car lst) init)])
;               (foldl f new-acc (cdr lst)))])))
; (println (foldl cons '() '(1 2 3 4)))

(define foldr
  (lambda (f init lst)
    (cond
      [(null? lst) init]
      [else (f (car lst) (foldr f init (cdr lst)))])))
  
(println (foldr cons '() '(1 2 3 4)))
(println (foldr + 0 '(1 2 3 4)))
(println (foldr * 1 '(1 2 3 4)))

; brainteasers
; 18
(define powerset
  (lambda (set)
    (cond
      [(null? set) '(())]
      [else (let ([rest (powerset (cdr set))])
              (append rest (map (lambda (x) (cons (car set) x)) rest)))])))

(println (powerset '()))
(println (powerset '(1 2 3)))

; 19
(define cartesian-product
  (lambda (sets)
    (define f
      (lambda (s acc)
        (append-map (lambda (ss)
                      (map (lambda (a) (cons ss a)) acc))
                    s)))
    (foldr f '(()) sets)))

(println (cartesian-product '()))
(println (cartesian-product '((5 4) (3 2 1))))

; 20
(define insertR-fr
  (lambda (s1 s2 lst)
    (define f 
      (lambda (s acc)
        (if (eqv? s s1)
            (cons s (cons s2 acc))
            (cons s acc))))
    (foldr f '() lst)))

(println (insertR-fr 'x 'y '(x z z x y z)))

(define filter-fr
  (lambda (pred lst)
    (define f
      (lambda (s acc)
        (if (pred s) (cons s acc) acc)))
    (foldr f '() lst)))
(println (filter-fr even? '(1 2 3 4 5 6)))

(define map-fr
  (lambda (f lst)
    (define ff
      (lambda (s acc)
        (cons (f s) acc)))
    (foldr ff '() lst)))

(println (map-fr add1 '(1 2 3 4 5 6)))


(define append-fr
  (lambda (lst1 lst2)
    (foldr cons lst2 lst1)))

(println (append-fr '(a b c) '(1 2 3 4 5 6)))

(define reverse-fr
  (lambda (lst)
    (define f
      (lambda (s acc)
        (append acc (cons s '()))))
    (foldr f '() lst)))

(println (reverse-fr '(a b c 1 2 3 4 5 6)))

(define binary->natural-fr
  (lambda (lst)
    (define f
      (lambda (n acc)
        (if (= n 0) 
            (* 2 acc)
            (+ 1 (* 2 acc)))))
    (foldr f 0 lst)))
(println (binary->natural-fr '(0 0 1)))

(define append-map-fr
  (lambda (f lst)
    (define ff
      (lambda (s acc)
        (append (f s) acc)))
    (foldr ff '() lst)))

(println (append-map-fr countdown (countdown 5)))

(define set-difference-fr
  (lambda (set1 set2)
    (define f
      (lambda (s acc)
        (if (memq s set2)
            acc
            (cons s acc))))
    (foldr f '() set1)))

(println (set-difference-fr '(1 2 3 4 5) '( 2 4 6)))

(define powerset-fr
  (lambda (set)
    (define f
      (lambda (s acc)
        (append acc (map (lambda (x) (cons s x)) acc))))
    (foldr f '(()) set)))

(println (powerset-fr '(1 2 3)))


; 21


; 22