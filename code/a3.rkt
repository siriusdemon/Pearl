
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
