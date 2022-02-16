;;; Here is a little snippet of Tuesday's lecture.
;;; By the time I get to ^^, the pattern has been formed.

(define plus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1* n (plus n (sub1 m)))))))

(define add1*
  (lambda (n s)
    (add1 s)))

(define times
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (plus n (times n (sub1 m)))))))

(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (times n (^ n (sub1 m)))))))

(define ^^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (^ n (^^ n (sub1 m)))))))

(define ^^^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (^^ n (^^^ n (sub1 m)))))))

;;; At this point, I introduce G^, which is just G with a fixed k.
;;; Also, I introduce builder, just to make the code more readable.
;;; Of course, builder is not recursive, which is why I can use let.

(define G
  (lambda (k)
    (letrec
      ((G^ (lambda (n m)
             (cond
               ((zero? m) (cond
                            ((zero? k) n)
                            ((zero? (sub1 k)) 0)
                            (else 1)))
               (else
                 (let ((builder (cond
                                  ((zero? k) add1*)
                                  ((zero? (sub1 k)) plus)
                                  ((zero? (sub1 (sub1 k))) times)
                                  (else (G (sub1 k))))))
                   (builder n (G^ n (sub1 m)))))))))
      G^)))

; break> r
; > ((G 2) 3 2)
; 9
; > ((G 2) 3 3)
; 27
; > ((G 3) 3 3)

; break> 

;;; Now without builder.

(define G
  (lambda (k)
    (letrec
      ((G^ (lambda (n m)
             (cond
               ((zero? m) (cond
                            ((zero? k) n)
                            ((zero? (sub1 k)) 0)
                            (else 1)))
               ((zero? k) (add1* n (G^ n (sub1 m))))
               ((zero? (sub1 k)) (plus n (G^ n (sub1 m))))
               ((zero? (sub1 (sub1 k))) (times n (G^ n (sub1 m))))
               (else ((G (sub1 k)) n (G^ n (sub1 m))))))))
      G^)))

; break> r
; > ((G 2) 3 2)
; 9
; > ((G 2) 3 3)
; 27
; > ((G 3) 3 3)

; break> 

;;; Let't get rid of the local recursion.  I have curried the code,
;;; so now I just uncurry G making Gu (the "u" is for "uncurry")
;;; by passing k around.

(define Gu
  (lambda (k n m)
    (cond
      ((zero? m) (cond
                   ((zero? k) n)
                   ((zero? (sub1 k)) 0)
                   (else 1)))
      ((zero? k) (add1* n (Gu k n (sub1 m))))
      ((zero? (sub1 k)) (plus n (Gu k n (sub1 m))))
      ((zero? (sub1 (sub1 k))) (times n (Gu k n (sub1 m))))
      (else (Gu (sub1 k) n (Gu k n (sub1 m)))))))

; > (Gu 2 3 2)
; 9
; > (Gu 3 3 2)
; 27
; > (Gu 3 3 3)

; break> r

;;; Next, I introduce a function base for the special base cases.

(define Gu/base
  (lambda (k n m)
    (cond
      ((zero? m) (base k n))
      ((zero? k) (add1* n (Gu/base k n (sub1 m))))
      ((zero? (sub1 k)) (plus n (Gu/base k n (sub1 m))))
      ((zero? (sub1 (sub1 k))) (times n (Gu/base k n (sub1 m))))
      (else (Gu/base (sub1 k) n (Gu/base k n (sub1 m)))))))

(define base
  (lambda (k n)
    (cond
      ((zero? k) n)
      ((zero? (sub1 k)) 0)
      (else 1))))

; > (Gu/base 2 3 2)
; 9
; > (Gu/base 3 3 2)
; 27
; > (Gu/base 3 3 3)

; break> 

;;; Finally, I curry (the "c" is for curry) Gu/base in a simple way.

(define Gc
  (lambda (k)
    (lambda (n m)
      (Gu/base k n m))))

; > ((Gc 2) 3 2)
; 9
; > ((Gc 2) 3 3)
; 27
; > ((Gc 3) 3 3)

; break> r
; > 

; Finally, I define the operators that I had intended to use all along.

(define +o (Gc 0))
(define *o (Gc 1))
(define ^o (Gc 2))
(define ^^o (Gc 3))

; > (+o 2 3)
; 5
; > (*o 2 3)
; 6
; > (^o 2 3)
; 8
; > (^^o 3 3)

; break> r
