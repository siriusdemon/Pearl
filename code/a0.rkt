#lang racket

;;;
(define println
  (lambda (x)
    (display x)
    (newline)))


; 2
(define pie 3.14)

; 3
(define area_circle
  (lambda (radius)
    (* radius radius pie)))

(println (area_circle 5))

; 4
(define circumference
  (lambda (radius)
    (* 2 radius pie)))

(define circle_properties
  (lambda (radius)
    (list (area_circle radius)
          (circumference radius))))

(println (circle_properties 5))

; 5
(define rectangle_area
  (lambda (length breadth)
    (* length breadth)))

(define rectangle_perimeter
  (lambda (length breadth)
    (* 2 (+ length breadth))))

(define rectangle_properties
  (lambda (length-breadth)
    (let ([length (car length-breadth)]
          [breadth (cadr length-breadth)])
      (list (rectangle_area length breadth)
            (rectangle_perimeter length breadth)))))

(println (rectangle_properties '(2 4)))


; 6
(define find-needle
  (lambda (lst3)
    (define (helper lst index)
      (cond 
        [(null? lst) -1]
        [(eq? (car lst) 'needle) index]
        [else (helper (cdr lst) (+ 1 index))]))
    (helper lst3 0)))

(println (find-needle '(hay needle hay)))
(println (find-needle '(hay hay hay)))

; 7
(define abs
  (lambda (n)
    (if (>= n 0) n (- n))))

(println (abs 3))
(println (abs -2))

; 8
(println (map add1 '(1 2 3)))

; 9
(define even even?)
(println (map even '(1 2 3 4 5 6)))

; 10
; 11
(define another-add
  (lambda (n m)
    (cond
      [(zero? n) m]
      [else (another-add (sub1 n) (add1 m))])))

(println (another-add 40 2))