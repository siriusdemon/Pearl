


(define ack
  (lambda (k)
    (lambda (s t)
      (cond
        ((zero? k) (add1 t))
        ((zero? (sub1 k)) (cond
                            ((zero? t) s)
                            (else ((ack (sub1 k)) s ((ack k) s (sub1 t))))))
        ((zero? (sub1 (sub1 k))) (cond
                                   ((zero? t) 0)
                                   (else ((ack (sub1 k)) s ((ack k) s (sub1 t))))))
        ((zero? t) 1)
        (else ((ack (sub1 k)) s ((ack k) s (sub1 t))))))))

> (define succ (ack 0))
> (define + (ack 1))
> (define * (ack 2))
> (define ^ (ack 3))
> (define ^^ (ack 4))
> (define ^^^ (ack 5))
> (define ^^^^ (ack 6))
> (+ 2 3)
5
> (* 2 3)
6
> (^ 2 3)
8
> (^^ 2 3)
16
> (^^^ 2 3)
65536
> (^^^^ 2 3)

(define ack
  (lambda (k)
    (lambda (s t)
      (cond
        ((zero? t) (cond
                     ((zero? k) 1)
                     ((zero? (sub1 k)) s)
                     ((zero? (sub1 (sub1 k))) 0)
                     (else 1)))
        ((zero? k) (add1 t))
        (else ((ack (sub1 k)) s ((ack k) s (sub1 t))))))))

(define ack
  (lambda (k)
    (lambda (s t)
      (cond
        ((zero? k) (add1 t))
        ((zero? t) (cond                           ;;; (ack (sub1 k) _ 1)
                     ((zero? (sub1 k)) s)
                     ((zero? (sub1 (sub1 k))) 0)
                     (else 1)))
        (else ((ack (sub1 k)) s ((ack k) s (sub1 t))))))))

(define ack
  (lambda (k t)
    (cond
      ((zero? k) (add1 t))
      ((zero? t) (ack (sub1 k) 1))
      (else (ack (sub1 k) (ack k (sub1 t)))))))