#lang racket

(provide pc2py compile/run)

(define reg-funcs '())
(define reg-pc 'no_pc)
(define dismount-var 'no_dis)
(define construct-var 'no_const)

(define global-decls '())
(define current-global-decls '())
(define main-def "")
(define union-defs "")
(define python-keywords
  '(False class from or None continue global pass True
          def if raise and del import return as elif
          in try assert else is while async except
          lambda with await finally nonlocal yield break
          for not))

(define add-func
  (lambda (func)
    (set! reg-funcs (cons func reg-funcs))))

(define is-func?
  (lambda (func)
    (assv func reg-funcs)))

(define is-global?
  (λ (var)
    (cond
      [(equal? (global var) reg-pc) #t]
      [(memv var reg-regs)
       => (λ (p) #t)]
      [else #f])))

(define reg-unions '())

(define check-args
  (lambda (union args)
    (cond
      [(null? args) #t]
      [(memq (car args) (cdr args))
       (error 'define-union "duplicated variant `~a' in union `~a'\n" (car args) (car union))]
      [else (check-args union (cdr args))])))

(define add-union
  (lambda (union)
    (if (not (lookup-union (car union)))
        (begin (check-args union (cadr union)) 
               (set! reg-unions (cons union reg-unions)))
        (error 'define-union "duplicated definition of union-type `~a'\n" (car union)))))

(define get-current-global-decls
  (λ ()
    (let ([decls (apply string-append
                        (map (λ (v) (format "    global ~a\n" v))
                             current-global-decls))])
      (begin
        (set! current-global-decls '())
        decls))))

(define reg-regs '())

(define init-storage
  (lambda ()
    (set! reg-funcs '())
    (set! reg-unions '())
    (set! reg-regs '())))

(define new-safe-char
  (lambda (char)
    (cond
      [(eq? #\? char) "_"]
      [(eq? #\! char) "_"]
      [(eq? #\. char) "_"]
      [(eq? #\+ char) "_"]
      [(eq? #\- char) "_"]
      [(eq? #\* char) "_"]
      [(eq? #\/ char) "_"]
      [(eq? #\< char) "_"]
      [(eq? #\> char) "_"]
      [(eq? #\: char) "_"]
      [(eq? #\$ char) "_"]
      [(eq? #\% char) "_"]
      [(eq? #\^ char) "_cap"]
      [(eq? #\& char) "_"]
      [(eq? #\~ char) "_"]
      [(eq? #\_ char) "_"]
      [(and (char>=? char #\0) (char<=? char #\9))
       (string-append "r" (list->string `(,char)))]
      [else (list->string `(,char))])))

(define safe 
  (lambda (sym)
    (let ([str-sym (raw-safe sym)])
      (cond
        [(memv sym python-keywords)
         => (λ (p) (string-append "_" str-sym))]
        [else str-sym]))))

(define raw-safe
  (lambda (sym)
    (if (symbol? sym)
        (let loop ([l (string->list (symbol->string sym))])
          (cond
            [(null? l) ""]
            [else (string-append
                   (new-safe-char (car l))
                   (loop (cdr l)))]))
        sym)))

(define global
  (lambda (sym)
    (string-append "g_" (safe sym))))

(define join
  (lambda (lst separater)
    (let loop ([lst lst]
               [result ""]
               [is-first? #t])
      (cond
        [(null? lst) result]
        [is-first? (loop (cdr lst)
                         (format "~a" (car lst)) #f)]
        [else (loop (cdr lst)
                    (string-append
                     result
                     (format "~a~a" separater (car lst)))
                    #f)]))))

(define file->list
  (lambda (fname)
    (let ([file (open-input-file fname)])
      (let ([data
             (let recurse ([decl (read file)])
               (if (eof-object? decl)
                   '()
                   (cons decl (recurse (read file)))))])
        (close-input-port file)
        data))))

(define pc2py
  (lambda (file-name source-name)
    ;; WARNING: pc2py will erase existing files when generating new ones!
    (when (file-exists? source-name) (delete-file source-name))
    (init-storage)
    (let ([decl* (file->list file-name)])
      (let ([src (open-output-file source-name)])
        (dynamic-wind
         (lambda () #f)
         (lambda ()
           ;; write a generated header file to header-name
           (display (pc2py-header decl*) src)
           (check-correct-info)
           ;; write a generated source file source-name
           (display (pc2py-source) src)
           (set! global-decls '())
           (set! union-defs ""))
         (lambda ()
           (close-output-port src)))))))

(define check-correct-info
  (lambda ()
    (begin
      (if (null? reg-regs)
          (display "Warning: you have defined no registers.\n")
          (void)))))

(define pc2py-append
  (lambda args
    (apply string-append
           (map (lambda (elt)
                  (cond
                    [(symbol? elt) (format "~a" elt)]
                    [(number? elt) (format "~s" elt)]
                    [(string? elt) elt]
                    [else (error 'pc2py-append "Invalid argument ~s" elt)]))
                args))))

(define pc2py-gen-unions
  (lambda (union)
    (let ([name (safe (car union))]
          [tag* (cadr union)]
          [field** (caddr union)])
      (apply string-append
             (map (lambda (tag field*)
                    (let* ([safe-tag (safe tag)]
                           [fnname (cond
                                     [(memv tag python-keywords)
                                      => (λ (p) (pc2py-append name safe-tag))]
                                     [else (pc2py-append name "_" safe-tag)])])
                      (pc2py-append
                       (pc2py-fn-proto fnname field*) "\n"
                       "    return union_t(union_t." name "." safe-tag ", "
                       (string-join (map (λ (v)
                                           (string-append
                                            "\n            "
                                            (safe v) "=" (safe v)))
                                         field*)
                                    ", ")
                       ")\n\n")))
                  tag* field**)))))

;; added by wy for constructor argument name binding
;; lookup-arg looks up the argument name of name.tag at position pos
(define lookup-union
  (lambda (name)
    (let loop ([reg reg-unions])
      (cond
        [(null? reg) #f]
        [(eq? name (caar reg)) (car reg)]
        [else (loop (cdr reg))]))))

(define get-arg-list
  (lambda (name tag)
    (let ([u (lookup-union name)])
      (if (not u) (error 'lookup-union 
                         "union type `~a' not defined\n" name)
          (let loop ([tags (cadr u)] [args (caddr u)])
            (cond
              [(null? tags) 
               (error 'lookup-arg
                      "union type `~a' doesn't have a tag `~a'~n" name tag)]
              [(eq? tag (car tags)) (car args)]
              [else (loop (cdr tags) (cdr args))]))))))

(define lookup-arg
  (lambda (name tag pos)
    (list-ref (get-arg-list name tag) pos)))

(define check-union-case
  (lambda (expr name type case)
    (cond
      [(and (null? type) (not (null? case)))
       (let ([s (open-output-string)])
         (pretty-print expr s)
         (error 'union-case  "~a\nsuperfluous cases for union type `~a': ~a"
                (get-output-string s) name case))]
      [(and (null? case) (not (null? type)))
       (let ([s (open-output-string)])
         (pretty-print expr s)
         (error 'union-case  "~a\nunmatched cases for union type `~a': ~a"
                (get-output-string s) name type))]
      [(and (null? type) (null? case)) #t]
      [(not (memq (car case) type))
       (let ([s (open-output-string)])
         (pretty-print expr s)
         (error 'union-case "~a\nvariant `~a' is not in union type `~a'"
                (get-output-string s) (car case) name))]
      [(memq (car case) (cdr case))
       (let ([s (open-output-string)])
         (pretty-print expr s)
         (error 'union-case  "~a\nduplicated cases `~a' in union-case of type `~a'"
                (get-output-string s) (car case) name))]
      [else (check-union-case expr name (remq (car case) type) (cdr case))])))

(define case-env
  (lambda (env var*)
    (let loop ([env env] [var* var*])
      (if (null? var*)
          env
          (extend-env (car var*) (car var*) (loop env (cdr var*)))))))

(define handle-union-case-case
  (lambda (name env u_obj)
    (lambda (template body)
      (match template 
        [`(,tag . ,var*) #:when (list? var*)
                         (let ([sname (safe name)]
                               [stag (safe tag)])
                           (let ([given (length var*)] [expected (length (get-arg-list name tag))])
                             (if (not (= given expected))
                                 (error 'union-case
                                        "~a\nwrong number of arguments to constructor `~a' of union-type `~a': expected: ~a, given: ~a"
                                        template tag name expected given)
                                 (pc2py-append
                                  "        case union_t." sname "." stag ":\n"
                                  (let loop ([var* var*] [n 0])
                                    (cond
                                      [(null? var*) ""]
                                      [else (string-append 
                                             (pc2py-append
                                              "            " (safe (car var*)) " = " u_obj
                                              "." (safe (lookup-arg name tag n)) "\n")
                                             (loop (cdr var*) (add1 n)))]))
                                  ((parse-function-body #t (case-env env var*) 3) body)
                                  "\n"))))]
        ;; Cannot possibly be effective, commented JBH 12/13
        ;; [else (string-append "default {\n"
        ;;         ((parse-function-body #t (case-env env var*)) body)
        ;;         "}\n")]
        ))))

(define get-last
  (lambda (ls)
    (cond
      ((null? ls) #f)
      ((null? (cdr ls)) (car ls))
      (else (get-last (cdr ls))))))

;; this is for error checking
(define get-body
  (lambda (c)
    (match c
      [`(,test ,body) body])))

(define remove-last
  (lambda (ls)
    (match ls
      [`((else ,body)) '()]
      [`((,test ,body) . ,c*) `((,test ,body) . ,(remove-last c*))])))

(define apply-env
  (lambda (env x)
    (match env
      [`(empty-env) (error 'empty-env "unbound variable: ~s" x)]
      [`(extend-env ,x^ ,a ,env)
       (if (eq? x^ x) a (apply-env env x))])))

(define extend-env
  (lambda (x a env)
    `(extend-env ,x ,a ,env)))

(define empty-env
  (lambda ()
    `(empty-env)))

(define tabs
  (lambda (n)
    (cond
      [(zero? n) ""]
      [else (string-append "    " (tabs (sub1 n)))])))

(define parse-function-body
  (lambda (tail env level)
    (if tail
        (lambda (expr)
          (match expr
            [`(error ,name ,msg) 
             (pc2py-append
              (tabs level) "raise RuntimeError(\"" msg "\")\n")]
            [`(if ,test ,conseq ,alt) 
             (let ((test ((parse-function-body #f env (add1 level)) test))
                   (conseq ((parse-function-body #t env (add1 level)) conseq))
                   (alt ((parse-function-body #t env (add1 level)) alt)))
               (pc2py-append
                (tabs level) "if " test ":\n"
                conseq
                (tabs level) "else:\n"
                alt))]
            [`(cond (else ,body)) 
             (let ((body ((parse-function-body #t env level) body)))
               body)]
            [`(cond . ,c*) 
             (let ((last (get-last c*))
                   (c* (remove-last c*)))
               (cond
                 [(eq? (car last) 'else)
                  (let* ((test0 ((parse-function-body #f env level) (caar c*)))
                         (body0 ((parse-function-body #t env (add1 level)) (get-body (car c*))))
                         (test* (map (parse-function-body #f env level) (map car (cdr c*))))
                         (body* (map (parse-function-body #t env (add1 level)) (map get-body (cdr c*))))
                         (body ((parse-function-body #t env (add1 level)) (cadr last))))
                    (pc2py-append
                     (tabs level) "if " test0 ":\n" body0 "\n"
                     (apply string-append
                            (map (lambda (x y)
                                   (pc2py-append (tabs level) "elif " x ":\n"
                                                 y "\n"))
                                 test* body*))
                     (tabs level) "else:\n" body "\n"))]
                 [else
                  (let* ((test0 ((parse-function-body #f env level) (caar c*)))
                         (body0 ((parse-function-body #t env level) (cadar c*)))
                         (test* (map (parse-function-body #f env level) (map car (cdr c*))))
                         (body* (map (parse-function-body #t env level) (map cadr (cdr c*)))))
                    (pc2py-append
                     "if " test0 ":\n"
                     "    " body0 "\n"
                     (apply string-append
                            (map (lambda (x y)
                                   (pc2py-append "elif " x ":\n"
                                                 y))
                                 test* body*))))]))]
            [`(begin . ,expr*)
             (apply string-append (map (parse-function-body #t env level) expr*))]
            [`(set! ,var ,var1) #:when (eq? var var1) ""]
            [`(set! ,var ,val)
             (let ((val ((parse-function-body #f env level) val)))
               (if (is-global? var)
                   (begin
                     (set! current-global-decls (remove-duplicates (cons (global var) current-global-decls)))
                     (pc2py-append (tabs level) (global var) " = " (string-trim val) "\n"))
                   (pc2py-append (tabs level) (safe var) " = " (string-trim val) "\n")))]
            [`(union-case ,val ,name . ,c*)
             (let ((template* (map car c*))
                   (body* (map get-body c*)))
               (if (not (check-union-case expr name
                                          (cadr (or (lookup-union name)
                                                    (error 'lookup-union 
                                                           "union type `~a' not defined ~n" name)))
                                          (map car template*)))
                   (error 'union-case "union-case doesn't match definition: `~a'\n"
                          name)
                   (letrec ([sname (safe name)]
                            [target_u_obj (global val)]
                            [cases (apply string-append
                                          (map (handle-union-case-case name env target_u_obj)
                                               template*
                                               body*))])
                     (pc2py-append
                      ; "    global " val "\n"
                      ; (get-current-global-decls)
                      "    match " target_u_obj ".type:\n"
                      cases))))]
            [`(let ,bind* ,body) 
             (let ((lhs* (map car bind*))
                   (rhs* (map (parse-function-body #f env level) (map cadr bind*))))
               (pc2py-append
                "\n"
                (apply string-append
                       (map (lambda (x y)
                              (pc2py-append
                               (safe x) " = " y "\n"))
                            lhs* rhs*))
                body
                "\n"))]
            [`(printf ,str . ,parms*) 
             (string-append (tabs level)
                            "racket_printf(" (join (cons (format "~s" str)
                                                         (map (λ (s)
                                                                (cond
                                                                  [(is-global? s) (global s)]
                                                                  [else (safe s)]))
                                                              parms*))
                                                   ", ") ")\n")]
            [`(mount-trampoline ,construct ,dismount ,pc) 
             (set! construct-var (safe construct))
             (set! dismount-var (global dismount))
             (pc2py-append (tabs level)
                           "jump_mount_tram.switch()\n")]
            [`(dismount-trampoline ,dismount)
             (pc2py-append (tabs level)
                           "jumpout.switch()\n")]
            [`(,func) #:when (is-func? func)
                      (pc2py-append reg-pc " = " (safe func) "\n")]
            [`,elsee
             (let ((elsee ((parse-function-body #f env level) elsee)))
               (pc2py-append "return " elsee "\n"))]
            ))
        (lambda (expr)
          (match expr
            ;; [(error ,name ,msg) 
            ;;  (pc2py-append
            ;;   "fprintf(stderr, \"" msg "\");\n exit(1);\n")]
            [`#t  (pc2py-append "True")]
            [`#f  (pc2py-append "False")]
            [`,x #:when (symbol? x) (letrec ([var (apply-env env x)]
                                             [safe-x (safe var)])
                                      (if (is-global? var)
                                          (begin
                                            (set! current-global-decls
                                                  (remove-duplicates
                                                   (cons (global var)
                                                         current-global-decls)))
                                            (global var))
                                          safe-x))]
            [`,x #:when (integer? x) (pc2py-append x)]
            [`(zero? ,x) 
             (let ((x ((parse-function-body #f env level) x)))
               (pc2py-append "(" x " == 0)"))]
            [`(and ,a ,b) 
             (let ((a ((parse-function-body #f env level) a))
                   (b ((parse-function-body #f env level) b)))
               (pc2py-append "(" a " and " b ")"))]
            [`(or ,a ,b) 
             (let ((a ((parse-function-body #f env level) a))
                   (b ((parse-function-body #f env level) b)))
               (pc2py-append "(" a " or " b ")"))]
            [`(not ,x) 
             (let ((x ((parse-function-body #f env level) x)))
               (pc2py-append "(not " x ")"))]
            [`(< ,a ,b) 
             (let ((a ((parse-function-body #f env level) a))
                   (b ((parse-function-body #f env level) b)))
               (pc2py-append "(" a " < " b ")"))]
            [`(> ,a ,b) 
             (let ((a ((parse-function-body #f env level) a))
                   (b ((parse-function-body #f env level) b)))
               (pc2py-append "(" a " > " b ")"))]
            [`(<= ,a ,b) 
             (let ((a ((parse-function-body #f env level) a))
                   (b ((parse-function-body #f env level) b)))
               (pc2py-append "(" a " <= " b ")"))]
            [`(>= ,a ,b) 
             (let ((a ((parse-function-body #f env level) a))
                   (b ((parse-function-body #f env level) b)))
               (pc2py-append "(" a " >= " b ")"))]
            [`(+ ,a ,b) 
             (let ((a ((parse-function-body #f env level) a))
                   (b ((parse-function-body #f env level) b)))
               (pc2py-append a " + " b))]
            [`(* ,a ,b) 
             (let ((a ((parse-function-body #f env level) a))
                   (b ((parse-function-body #f env level) b)))
               (pc2py-append a " * " b))]
            [`(- ,a ,b) 
             (let ((a ((parse-function-body #f env level) a))
                   (b ((parse-function-body #f env level) b)))
               (pc2py-append a " - " b))]
            [`(/ ,a ,b) 
             (let ((a ((parse-function-body #f env level) a))
                   (b ((parse-function-body #f env level) b)))
               (pc2py-append a " / " b))]
            [`(sub1 ,a) 
             (let ((a ((parse-function-body #f env level) a)))
               (pc2py-append "(" a " - 1)"))]
            [`(add1 ,a) 
             (let ((a ((parse-function-body #f env level) a)))
               (pc2py-append "(" a " + 1)"))]
            [`(random ,x) 
             (let ((x ((parse-function-body #f env level) x)))
               (pc2py-append "(random.randint(0, " x ")"))]
            [`(if ,test ,conseq ,alt) 
             (let ((test ((parse-function-body #f env) test))
                   (conseq ((parse-function-body #f env) conseq))
                   (alt ((parse-function-body #f env) alt)))
               (pc2py-append conseq " if " test " else " alt))]
            [`(,func . ,args*) #:when (symbol? func)
                               (let ((args* (map (parse-function-body #f env level) args*)))
                                 (pc2py-append (tabs level)
                                               (safe func) "(" (join args* ", ") ")"))])))))

(define pc2py-gen-funcs
  (lambda (env)
    (lambda (func)
      (let ([name (safe (car func))]
            [body (cadr func)])
        (if (equal? name "main")
            (begin
              (set! main-def (pc2py-append
                              (pc2py-append "def racket_printf(s, *args):\n"
                                            "    import re\n"
                                            "    print(re.sub(r\"~[a-z]\", lambda x: \"{}\", s).format(*args))\n\n"
                                            "if __name__ == '__main__':\n"
                                            "    def _blank():\n"
                                            "        pass\n"
                                            "    jump_mount_tram = greenlet(mount_tram)\n"
                                            "    _dismount_blank = greenlet(_blank)\n")
                              ((parse-function-body #t env 1) body)))
              "")

            (begin
              (pc2py-append
               (pc2py-append "def " name "():\n")
               (let ([bodystr ((parse-function-body #t env 1) body)]
                     [globalstr (get-current-global-decls)])
                 (string-append globalstr "\n"
                                bodystr)))))))))

(define global-env
  (lambda ()
    (let loop ([env (empty-env)] 
               [reg (append (map car reg-funcs) reg-regs)])
      (if (null? reg)
          env
          (extend-env (car reg) (car reg) (loop env (cdr reg)))))))

(define pc2py-source
  (lambda ()
    (let* ([s1 (apply string-append (map pc2py-gen-unions reg-unions))]
           [s2 (apply string-append (map (pc2py-gen-funcs (global-env)) reg-funcs))])
      (let ([s3 (pc2py-append
                 "def mount_tram():\n"
                 "    global " reg-pc "\n"
                 "    global " dismount-var "\n"
                 "    global _dismount_blank\n"
                 "    " dismount-var "= " construct-var "(_dismount_blank)\n\n"
                 "    while True:\n"
                 "        greenlet(" reg-pc ").switch()\n\n")])
        (string-append
         "# Union functions\n"
         s1
         "# Generate functions\n"
         s2
         s3
         "\n"
         main-def)))))

(define pc2py-header
  (lambda (decl*)
    (string-append
     "import sys\n"
     "from enum import Enum\n"
     "from greenlet import greenlet\n"
     "from typing import Any, Callable\n"
     "\n"
     (apply string-append
            (map pc2py-header-parse decl*))
     "# Define the dismount greenlet\n"
     "_dismount_blank = None\n\n"
     "# Define the union classes\n"
     "class union_t(object):\n"
     union-defs
     "    def __init__(self, type: Enum, **kwargs):\n"
     "        self.type = type\n"
     "        for key in kwargs:\n"
     "            setattr(self, key, kwargs[key])\n\n"
     "\n")))

(define pc2py-header-parse
  (lambda (decl)
    (match decl
      [`(load ,file . ,file*)  ""]
      [`(exit)  ""]
      [`(display ,anything . ,anything*)  ""]
      [`(pretty-print ,anything . ,anything*)  ""]
      [`(define-registers . ,reg*) 
       (set! reg-regs reg*)
       (string-append
        "# Define the registers\n"
        (if (null? reg*)
            ""
            (string-append
             (join (map (λ (v)
                          (let ([global-new (global v)])
                            (set! global-decls (cons global-new global-decls))
                            (string-append global-new ": object = None"))) reg*) "\n")
             "\n\n")))]
      [`(define-program-counter ,pc)
       (set! reg-pc (global pc))
       (string-append
        "# Define the program counter\n"
        reg-pc " : Callable[[], Any] = None\n\n")]
      [`(define-union ,name . ,c*) 
       (let ((tag* (map car c*))
             (field** (map cdr c*)))
         (add-union `(,name ,tag* ,field**))
         (let ([name (safe name)])
           (let ([enum-values
                  (apply string-append
                         (map
                          (lambda (tag field* index)
                            (let ([tag (safe tag)])
                              (format "        ~a = ~a\n" tag index)))
                          tag* field** (range (length tag*))))])
             (set! union-defs
                   (string-append union-defs
                                  (pc2py-append
                                   ;"class " name "_t(object):\n"
                                   "    class " name "(Enum):\n"
                                   enum-values "\n")))
             "")))]
      [`(define-label ,name ,body) ""
                                   (begin (add-func `(,name ,body))
                                          "" #;(string-append (if (equal? (safe name) "main")
                                                                  "int "
                                                                  "void ") (safe name) "();\n"))])))

(define pc2py-fn-proto
  (lambda (fn-name param*)
    (let ([declare-params
           (lambda (param*)
             (join (map (lambda (param)
                          (format "~a" (safe param))) param*) ", "))])
      (pc2py-append
       "def " (safe fn-name) "(" (declare-params param*)  "):"))))

(define compile/run
  (lambda (base-name)
    (let ([pc-file (string-append base-name ".ppy")]
          [py-file (string-append base-name ".py")])
      (pc2py pc-file py-file)
      (system (string-append "python ./" py-file)))))