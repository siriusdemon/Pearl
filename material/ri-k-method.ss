; Making continuations representation independent methodically:

; If you use the following method of making continuations representation
; independent you can easily convert to data structure representation
; without having to think about ending up with the right names for
; variables.

; Method by: Will Byrd

; Consider the following dmatch clause for an interpreter named ee that
; has been CPSed:

[(* ,e1 ,e2) (ee e1 env
                 (lambda (v)
                   (ee e2 env
                       (lambda (w)
                         (k (* v w))))))]

; We would like to make this code representation independent with respect to continuations.


; First we change all applications (k v) to (app-k k v).
; This can be accomplished using search and replace in Emacs:
; replace each occurrence of the string '(k ' with '(app-k k '.

[(* ,e1 ,e2) (ee e1 env
                 (lambda (v)
                   (ee e2 env
                       (lambda (w)
                         (app-k k (* v w))))))]

; Of course we must now define app-k:

(define app-k
  (lambda (k v)
    (k v)))

; We can test our code at this point, to ensure we didn't make a mistake.

; Now we are ready to create constructor procedures for our continuations.
; Although we will eventually use a data-structural representation of
; continuations, we always begin with a procedural representation.
; This additional step allows for a more mechanical tranformation, with less
; thought on our part, and with less chance of a mistake.

; When creating constructors for nested continuations, we always begin
; with the innermost continuation.  Since we are working on the * line of the
; interpreter, we will name our constructor *-inner-k.

(define *-inner-k
  (lambda (   )
    ))

; We will fill in the formal paramters to *-inner-k shortly.

; Now we cut and paste into our constructor the entire lambda expression representing
; the innermost continuation.

[(* ,e1 ,e2) (ee e1 env
                 (lambda (v)
                   (ee e2 env
                       )))]

(define *-inner-k
  (lambda (   )
    (lambda (w)
      (app-k k (* v w)))))

; We now determine the free variables in the lambda expression we just
; pasted into our constructor.  These variables become the formal
; parameters to *-inner-k.  By convention, the 'k' argument always comes last.

(define *-inner-k
  (lambda (v k)
    (lambda (w)
      (app-k k (* v w)))))

; We copy the formal parameter list to the region of the interpreter from
; which we cut the lambda expression representing the innermost continuation.

[(* ,e1 ,e2) (ee e1 env
                 (lambda (v)
                   (ee e2 env
                       (v k))))]

; We then copy and paste the name of the constructor into the beginning of this list.

[(* ,e1 ,e2) (ee e1 env
                 (lambda (v)
                   (ee e2 env
                       (*-inner-k v k))))]


; In the constructor function, we want to change the name of the
; formal parameter of the inner lambda to 'v'.  This will make it
; trivial to convert to a data-structural representation of
; continuations.  We must be careful, however, to avoid shadowing the
; formal parameters of *-inner-k.  In the case of *-inner-k, we must
; rename the existing 'v' parameter.

(define *-inner-k
  (lambda (v^ k)
    (lambda (w)
      (app-k k (* v^ w)))))

; Now we are free to rename 'w' to 'v'.

(define *-inner-k
  (lambda (v^ k)
    (lambda (v)
      (app-k k (* v^ v)))))

; We repeat these steps for the outer continuation.

[(* ,e1 ,e2) (ee e1 env
                 (*-outer-k e2 env k))]

(define *-outer-k
  (lambda (e2 env k)
    (lambda (v)
      (ee e2 env
          (*-inner-k v k)))))

; In this case we needn't rename any formal parameters.

; The * line of our interpreter is now representation independent with
; respect to continuations.  Here is our code so far.

[(* ,e1 ,e2) (ee e1 env (*-outer-k e2 env k))]

(define app-k
  (lambda (k v)
    (k v)))

(define *-inner-k
  (lambda (v^ k)
    (lambda (v)
      (app-k k (* v^ v)))))

(define *-outer-k
  (lambda (e2 env k)
    (lambda (v)
      (ee e2 env (*-inner-k v k)))))


;  We are currently using a procedural represention of continuations.
;  It is trivial to convert to a data structural representation.
;  First we define app-k so that it pattern matches on its 'k' input:

(define app-k
  (lambda (k v)
    (dmatch k
      )))

; Now we must redefine the *-inner-k constructor.

(define *-inner-k
  (lambda (v^ k)
    (lambda (v)
      (app-k k (* v^ v)))))

; We do this by deleting the body of the constructor.

(define *-inner-k
  (lambda (v^ k)
    ))

; Then we copy and paste the formal parameter list into the body.

(define *-inner-k
  (lambda (v^ k)
    (v^ k)))

; We add a backquote and commas.

(define *-inner-k
  (lambda (v^ k)
    `(,v^ ,k)))

; Finally, we copy and paste the name of the constructor.

(define *-inner-k
  (lambda (v^ k)
    `(*-inner-k ,v^ ,k)))

; We follow the same steps with *-outer-k.

(define *-outer-k
  (lambda (e2 env k)
    `(*-outer-k ,e2 ,env ,k)))

; Now create a new app-k clause for each constructor.  The pattern for
; each clause is just the body of the associated constructor, without
; the backquote.

(define app-k
  (lambda (k v)
    (dmatch k      
      [(*-inner-k ,v^ ,k)        ]
      [(*-outer-k ,e2 ,env ,k)   ]
      )))

; The template of each clause is the body of the (lambda (v) ...) expression
; in the procedural version of our continuation constructors.

(define app-k
  (lambda (k v)
    (dmatch k
      [(*-inner-k ,v^ ,k)
       (app-k k (* v^ v))]
      [(*-outer-k ,e2 ,env ,k)
       (ee e2 env (*-inner-k v k))])))

; Notice how the prior process has given us the right parameter names:
; the continuation parameter is already named v for us. And we're done!
