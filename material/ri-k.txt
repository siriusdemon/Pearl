How to RI continuations like a champ.

We started with some CPSed RHS

(foo-cps 'cat (λ (v) (foo-cps 'dog (λ (w) (k (cons v w))))))

We made sure to add everywhere calls to app-k

(foo-cps 'cat (λ (v) (foo-cps 'dog (λ (w) (app-k k (cons v w))))))

So, now we can start making our program representation-independent with respect to continuations. We'll do this by working from the inside out. By which we mean, if one continuation is nested inside a second, we'll do the innermost one before we'll do the outermost one. By this mechanism once a continuation has entered representation-idependent land, we won't have to check again if it's actually representation-independent.

Here, that means grabbing the (λ (w) ...). See, we've grabbed it.

(foo-cps 'cat (λ (v) (foo-cps 'dog )))

(λ (w) (app-k k (cons v w)))

And giving it a name.

(define inner-k
  (λ ()
    (λ (w) 
      (app-k k (cons v w)))))

Now, we aren't finished yet, because that expression contains free variables. We'll fill the formal parameter list with the free variables of that expression.

(define inner-k
  (λ (v k)
    (λ (w) 
      (app-k k (cons v w)))))

Here, we put the k as the last argument just as a sort of convention. There's no real reason to do it other than to be consistent. Now we've defined our continuation constructor. Remember where we took the continuation from? Now we'l fill that hole in with an invocation of the constructor, with the free variables.

(foo-cps 'cat (λ (v) (foo-cps 'dog (inner-k v k))))

Having done so, we can CPS the outer one next. Same deal, separate the pieces.

(foo-cps 'cat )

(λ (v) (foo-cps 'dog (inner-k v k)))

We'll again go ahead and build a constructor.

(define outer-k
  (λ ()
    (λ (v)
      (foo-cps 'dog (inner-k v k)))))

And fill in the free variables in the parameter list.

(define outer-k
  (λ (k)
    (λ (v)
      (foo-cps 'dog (inner-k v k)))))

And fill back in the expression.

(foo-cps 'cat (outer-k k))

You should find that the lines of your program get an awful lot shorter as a result of doing this.

So, now we've got our program RI wrt continuations.

We're about to do a kind of a hack that'll prove useful down the line. It also has an immediate benefit.

How'd we define app-k?

(define app-k
  (λ (k v)
    (k v)))

When we do the usual higher-order function rep -> data structures rep transformation, the v is going to be the argument to app-k. But on one of those continuations we aren't using variable name v.

(define inner-k
  (λ (v k)
    (λ (w) ;; here. 
      (app-k k (cons v w)))))

And we can't just make this change willy-nilly; we're already using v.

So, what we're gonna do is add a ^ to the name of every formal parameter in the arguments to our costructors.

(define inner-k
  (λ (v^ k^)
    (λ (w) 
      (app-k k^ (cons v^ w)))))

(define outer-k
  (λ (k^)
    (λ (v)
      (foo-cps 'dog (inner-k v k^)))))

Now we'll be able to use the variable name v.

(define inner-k
  (λ (v^ k^)
    (λ (v) 
      (app-k k^ (cons v^ v)))))

This looks like something we didn't need to do everywhere, just where we had a name collision. Trust me, you'll wanna do it on all of them. When we do our set!s later on, you wanna make sure you haven't locally shadowed the name of a global variable. The ^s will make sure we have different variable names.

This should be testable. Whenever we fill back in a continuation into our program, or when we do a variable name change, we should be able to test it.

Having tested it, we can now make the next step: the transformation from higher-order function representation to data structure representation of continuations. We're currently looking at the following:

(define app-k
  (λ (k v)
    (k v)))

(define outer-k
  (λ (k^)
    (λ (v)
      (foo-cps 'dog (inner-k v k^)))))

(define inner-k
  (λ (v^ k^)
    (λ (v) 
      (app-k k^ (cons v^ v)))))

There's a pretty neat trick to make it so you can test this transformation one at a time. First step, change app-k.

(define app-k
  (λ (k v)
    (match k
      (else (k v)))))

This is totally a correctness-preserving transformation. If the old definition worked, this one will too. But we can now slip in match clauses one at a time, and dispatch by case. Anything that doesn't match one of the forms we've gotten to yet will fall through to the default case.

We wanna turn our constructors into things that make data structures, instead of functions. Simultaneously, we wanna build a match clause we can add to app-k.

(define outer-k
  (λ (k^)
    (λ (v)
      (foo-cps 'dog (inner-k v k^)))))

We'll grab the formal parameter list and the name of the constructor, and build a tagged list.

(define outer-k(outer-k k^)
  (λ (k^)
    (λ (v)
      (foo-cps 'dog (inner-k v k^)))))

This doesn't run or anything, it's just there for the time being. We'll next add a quasiquote out in front of our data-structure, and put a comma before every parameter.

(define outer-k`(outer-k ,k^)
  (λ (k^)
    (λ (v)
      (foo-cps 'dog (inner-k v k^)))))

We'll make this data structure the body of the constructor. It'll also be the left-hand side of a match clause we're about to build.

(define outer-k
  (λ (k^)
    `(outer-k ,k^)
    `(outer-k ,k^)
    (λ (v)
      (foo-cps 'dog (inner-k v k^)))))

What's the right-hand side of the match clause? The body of the continuation we still have laying around.

(define outer-k
  (λ (k^)
    `(outer-k ,k^)
    `(outer-k ,k^)
    (foo-cps 'dog (inner-k v k^))))

We stick those two pieces together and we have our match clause.

(define outer-k
  (λ (k^)
    `(outer-k ,k^)
    (`(outer-k ,k^)
     (foo-cps 'dog (inner-k v k^)))))

Notice v is now a free variable in the match clause? When we got rid of the (λ (v) ...)? That's fine, because we're about to drop that match clause into app-k, and remove it from our outer-k.

(define outer-k
  (λ (k^)
    `(outer-k ,k^)))

(define app-k
  (λ (k v)
    (match k
      (`(outer-k ,k^) (foo-cps 'dog (inner-k v k^)))
      (else (k v)))))

And v now refers to the argument to the continuation. Just like it did before we started this transformation. Let's do the next one.

(define outer-k
  (λ (k^)
    `(outer-k ,k^)))

(define inner-k
  (λ (v^ k^)
    `(inner-k ,v^ ,k^)))

(define app-k
  (λ (k v)
    (match k
      (`(outer-k ,k^) (foo-cps 'dog (inner-k v k^)))
      (`(inner-k ,v^ ,k^) (app-k k^ (cons v^ v)))
      (else (k v)))))

That one went more quickly, because we didn't write out every step. Presumably we have more continuations finish off, at least an empty-k.

(define app-k
  (λ (k v)
    (match k
      (`(outer-k ,k^) (foo-cps 'dog (inner-k v k^)))
      (`(inner-k ,v^ ,k^) (app-k k^ (cons v^ v)))
      (`(empty-k) v)
      (else (k v)))))

So, we've done that one too. Now, once we've done all of our continuations, we can get rid of the else line.

(define app-k
  (λ (k v)
    (match k
      (`(outer-k ,k^) (foo-cps 'dog (inner-k v k^)))
      (`(inner-k ,v^ ,k^) (app-k k^ (cons v^ v)))
      (`(empty-k) v))))

Having done that, we now have some assurance that we've transformed all the continuations to data structures and are no longer using any continuations that are implemented as higher-order functions. This relies on coverage tests though, so no guarantee. 
