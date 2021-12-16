#lang racket
; 1
(define countdown 
  (lambda (n)
    (if (= n 0)
        '(0)
        (cons n (countdown (- n 1))))))

(display (countdown 5))
(newline)

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

(display (insertL 'x 'y '(x z z x y z)))
(newline)

; 3
(define remv-lst
  (lambda (s lst)
    (cond 
      [(null? lst) '()]
      [(eqv? (car lst) s) (cdr lst)]
      [else (cons (car lst)
                (remv-lst s (cdr lst)))])))

(display (remv-lst 'x '(x y z x)))
(newline)
(display (remv-lst 'y '(x y z y x)))
(newline)
(display (remv-lst 'z '(a b c)))
(newline)

; 4

(define map
  (lambda (f lst)
    (cond 
      [(null? lst) '()]
      [else (cons (f (car lst))
                (map f (cdr lst)))])))

(display (map add1 '(1 2 3 4)))
(newline)


; 5
(define filter
  (lambda (pred lst)
    (cond
      [(null? lst) '()]
      [(pred (car lst)) (cons (car lst)
                            (filter pred (cdr lst)))]
      [else (filter pred (cdr lst))])))

(display (filter even? '( 1 2 3 4 5 6)))
(newline)

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

(display (zip '(1 2 3) '(a b c)))
(newline)
(display (zip '(1 2 3 4 5 6) '(a b c)))
(newline)
(display (zip '(1 2 3) '(a b c d e f)))
(newline)

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

(display (list-index-ofv 'x '(x y z x x )))
(newline)
(display (list-index-ofv 'x '(y z x x)))
(newline)

; 8
(define append
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) lst2]
      [else (cons (car lst1)
              (append (cdr lst1) lst2))])))

(display (append '(42 120) '(1 2 3)))
(newline)
(display (append '(a b c) '(cat dog)))
(newline)


; 9
(define reverse
  (lambda (lst)
    (cond 
      [(null? lst) '()]
      [else (append (reverse (cdr lst))
              (cons (car lst) '()))])))

(display (reverse '(a 3 x)))
(newline)
(display (reverse '(a b c d e f g)))
(newline)

; 10
(define repeat
  (lambda (lst n)
    (cond 
      [(= n 0) '()]
      [(= n 1) lst]
      [else (append lst (repeat lst (- n 1)))])))

(display (repeat '(4 8 11) 4))
(newline)
(display (repeat '(4 8 11) 1))
(newline)
(display (repeat '(4 8 11) 0))
(newline)

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

(display (same-lists* '() '()))
(newline)
(display (same-lists* '(1 2 3 4 5) '(1 2 3 4 5)))
(newline)
(display (same-lists* '(1 2 3 5) '( 1 2 3)))
(newline)
(display (same-lists* '(1 (2 3) 4) '(1 (2) 3 4)))
(newline)
(display (same-lists* '((a) b (c d) d) '((a) b (c d ) d)))
(newline)

; 12
; ((w x) y (z)) 

; 13 
(define binary->natural
  (lambda (lst)
    (cond 
      [(null? lst) 0]
      [(= 1 (car lst)) (+ 1 (* 2 (binary->natural (cdr lst))))]
      [(= 0 (car lst)) (* 2 (binary->natural (cdr lst)))])))

; this function do in normal bit order
; (define binary->natural2
;   (lambda (lst)
;     (define (helper lst acc)
;       (cond 
;         [(null? lst) acc]
;         [(= 1 (car lst)) (helper (cdr lst) (+ 1 (* 2 acc)))]
;         [(= 0 (car lst)) (helper (cdr lst) (* 2 acc))]))
;     (helper lst 0)))

(display (binary->natural '()))
(newline)
(display (binary->natural '(0 0 1)))
(newline)
(display (binary->natural '(1 1 1 1)))
(newline)
(display (binary->natural '(0 0 1 1)))
(newline)
(display (binary->natural '(1 0 1 0 1)))
(newline)
(display (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)))
(newline)


; 14
(define minus
  (lambda (subor subee)
    (if (= subee 0) 
        subor
        (minus (sub1 subor) (sub1 subee)))))

(display (minus 5 3))
(newline)
(display (minus 100 50))
(newline)


; 15
(define div
  (lambda (divor divee)
    (cond
      [(= 0 divee) (error "bad data")]
      [(= 1 divee) divor]
      [(= 0 divor) 0]
      [else (add1 (div (- divor divee) divee))])))

(display (div 25 5))
(newline)
(display (div 36 6))
(newline)



; 16
(define append-map
  (lambda (f lst)
    (cond
      [(null? lst) '()]
      [else (append (f (car lst))
                    (append-map f (cdr lst)))])))

(display (append-map countdown (countdown 5)))
(newline)


; 17
(define set-difference
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [else (if (memq (car set1) set2)
                (set-difference (cdr set1) set2)
                (cons (car set1) (set-difference (cdr set1) set2)))])))

(display (set-difference '(1 2 3 4 5) '(2 4 6 8)))
(newline)


; 18
(define foldl
  (lambda (f init lst)
    (cond
      [(null? lst) init]
      [else (let ([new-acc (f (car lst) init)])
              (foldl f new-acc (cdr lst)))])))
(display (foldl cons '() '(1 2 3 4)))
(newline)

(define foldr
  (lambda (f init lst)
    (cond
      [(null? lst) init]
      [else (f (car lst) (foldr f init (cdr lst)))])))
  
(display (foldr cons '() '(1 2 3 4)))
(newline)
(display (foldr + 0 '(1 2 3 4)))
(newline)
(display (foldr * 1 '(1 2 3 4)))
(newline)

; bt 19
; init:  '() => '(())
; n   :  cons n into (n - 1) 
(define powerset
  (lambda (set)
    (cond
      [(null? set) '(())]
      [else (let ([rest (powerset (cdr set))])
              (append rest (map (lambda (x) (cons (car set) x)) rest)))])))

(display (powerset '()))
(newline)
(display (powerset '(1 2 3)))
(newline)


; bt 20
; the anno is not understandable yet


; bt 21
(define cartesian-product
  (lambda (sets)
    (define f
      (lambda (s acc)
        (append-map (lambda (ss)
                (map (lambda (a) (cons ss a)) acc))
             s)))
    (foldr f '(()) sets)))

(display "cartesian-product")
(newline)
(display (cartesian-product '()))
(newline)     ;  (())
(display (cartesian-product '((1 2 3))))
(newline)     ;  ((1) (2) (3))
(display (cartesian-product '((1 2 3) (a b))))
(newline)     ;  ((1 a) (1 b) (2 a) (2 b) (3 a) (3 b))
(display (cartesian-product '((1 2 3) (a b) (H))))
(newline)     ;  


; bt 22
; the light is that, foldr is high abstraction....
; using foldr rewrite these
(define insertR-fr
  (lambda (s1 s2 lst)
    (define f 
      (lambda (s acc)
        (if (eqv? s s1)
            (cons s (cons s2 acc))
            (cons s acc))))
    (foldr f '() lst)))

(display (insertR-fr 'x 'y '(x z z x y z)))
(newline)
;; in essence, fold function is likely a for-loop


(define filter-fr
  (lambda (pred lst)
    (define f
      (lambda (s acc)
        (if (pred s) (cons s acc) acc)))
    (foldr f '() lst)))
(display (filter-fr even? '(1 2 3 4 5 6)))
(newline)


(define map-fr
  (lambda (f lst)
    (define ff
      (lambda (s acc)
        (cons (f s) acc)))
    (foldr ff '() lst)))
(display (map-fr add1 '(1 2 3 4 5 6)))
(newline)


(define append-fr
  (lambda (lst1 lst2)
    (foldr cons lst2 lst1)))
(display (append-fr '(a b c) '(1 2 3 4 5 6)))
(newline)


(define reverse-fr
  (lambda (lst)
    (define f
      (lambda (s acc)
        (append acc (cons s '()))))
    (foldr f '() lst)))
(display (reverse-fr '(a b c 1 2 3 4 5 6)))
(newline)

(define binary->natural-fr
  (lambda (lst)
    (define f
      (lambda (n acc)
        (if (= n 0) 
            (* 2 acc)
            (+ 1 (* 2 acc)))))
    (foldr f 0 lst)))
(display (binary->natural-fr '(0 0 1)))
(newline)


(define append-map-fr
  (lambda (f lst)
    (define ff
      (lambda (s acc)
        (append (f s) acc)))
    (foldr ff '() lst)))

(display (append-map-fr countdown (countdown 5)))
(newline)

(define set-difference-fr
  (lambda (set1 set2)
    (define f
      (lambda (s acc)
        (if (memq s set2)
            acc
            (cons s acc))))
    (foldr f '() set1)))
(display (set-difference-fr '(1 2 3 4 5) '( 2 4 6)))
(newline)

; step1
(define powerset-fr
  (lambda (set)
    (define f
      (lambda (s acc)
        (append acc (map (lambda (x) (cons s x)) acc))))
    (foldr f '(()) set)))

(display (powerset-fr '(1 2 3)))
(newline)

; step2
(define powerset-fr2
  (lambda (set)
    (define f
      (lambda (s acc)
        (let ([t (map (lambda (x) (cons s x)) acc)])
          (foldr cons acc t))))
    (foldr f '(()) set)))

; f (s acc)
; append acc => 
; foldr ?? acc ??

; foldr cons 
; (display (foldr cons '(1 2 3) '(1 2 3)))
; (newline)
(display (powerset-fr2 '(1 2 3)))
(newline)


; bt 23
; (: snowball (-> Number Number))

; bt quine