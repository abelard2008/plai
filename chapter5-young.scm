#lang plai
(halt-on-errors)
;; abstract syntax
(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [mult (lhs AE?)
        (rhs AE?)]
  [id (s symbol?)]
  [with (name symbol?)
        (named-thing AE?)
        (body AE?)]
  [app (fun-name symbol?) 
       (arg AE?)])

;; abstract data type for functions
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body AE?)])

;; An environment or Deferred Substitution

(define-type DefrdSubst
  [mtSub]
  
  [aSub (name symbol?)
        (named-value number?)
        (all-the-others DefrdSubst?)])

;; concrete syntax
#|
AE = <number>
   | (* <AE> <AE>)
   | (+ <AE> <AE>)
   | (with (<id> <AE>) <AE>)
   | <id>
   | (<id> <AE>)
|#

;; parse : concrete -> abstract
(define (parse c)
  (cond
   [(number? c)
    (num c)]
   [(and (list? c)
         (= 3 (length c))
         (equal? '+ (first c)))
    (add (parse (second c))
         (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? '* (first c)))
    (mult (parse (second c))
          (parse (third c)))]
   [(and (list? c)
         (= 3 (length c))
         (equal? 'with (first c)))
    (with (first (second c))
          (parse (second (second c)))
          (parse (third c)))]
   [(and (list? c)
         (= 2 (length c))
         (symbol? (first c)))
    (app (first c) (parse (second c)))]
   [(symbol? c)
    (id c)]
   [else
    (error 'parse "Bad programmer, no cake ~e" c)]))
(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
(test/exn (parse '(+ 1 1 1))
          "no cake")
(test (parse '(* 3 1))
      (mult (num 3) (num 1)))
(test (parse '(with (x 5) (+ x x)))
      (with 'x (num 5) (add (id 'x) (id 'x))))

;; subst : id AE AE -> AE
(define (subst i v e)
  (type-case
   AE e
   [num (n)
        (num n)]
   [add (lhs rhs)
        (add
         (subst i v lhs)
         (subst i v rhs))]
   [mult (lhs rhs)
         (mult
          (subst i v lhs)
          (subst i v rhs))]
   [id (s)
       (if (equal? s i)
           v
           (id s))]
   [with (i* v* e*)
         (if (equal? i i*)
             (with i*
                   (subst i v v*)
                   e*)
             (with i*
                   (subst i v v*)
                   (subst i v e*)))]
    [app (fun-name arg-expr)
         (app fun-name (subst i v arg-expr))]))  

(test (subst 'x (num 5) (add (id 'x) (id 'x)))
      (add (num 5) (num 5)))

(test (subst 'x (num 5) (add (id 'x) (id 'y)))
      (add (num 5) (id 'y)))

      
;; lookup-fundef : symbol? (listof FunDef) -> FunDef
(define (lookup-fundef name fun-defs)
  (cond
    [(empty? fun-defs)
     (error name "function not found")]
    [else
     (if (symbol=? name (fundef-fun-name (first fun-defs)))
         (first fun-defs)
         (lookup-fundef name (rest fun-defs)))]))

;; lookup-binding : symbol? DefrdSub -> AE?

(define (lookup-binding s ds)
  (type-case
   DefrdSubst ds
   [mtSub ()
          (error 'lookup-binding "Unbound identifier ~e" s)]
   [aSub (name named-value more)
         (if (symbol=? s name)
             named-value
             (lookup-binding s more))]))

;; interp* : AE (listof FunDef) DefrdSub -> meaning
(define (interp* some-ae fun-defs ds)
  (type-case
   AE some-ae
   [num (n)
        n]
   [add (lhs rhs)
        (+ (interp* lhs fun-defs ds)
           (interp* rhs fun-defs ds))]
   [mult (lhs rhs)
         (* (interp* lhs fun-defs ds)
            (interp* rhs fun-defs ds))]
   [id (s)
       ;;(error 'interp "unbound undiscovery infinite: ~e" s)]
       (lookup-binding s ds)]
   [with (i v e)
         (interp* e
                  fun-defs
                  (aSub i 
                        (interp* v fun-defs ds)
                        ds))]
    [app (fun-name arg-expr)
        (local [(define fun-def (lookup-fundef fun-name fun-defs))]
               (interp* (fundef-body fun-def)
                       fun-defs
                       (aSub (fundef-arg-name fun-def)
                             (interp* arg-expr fun-defs ds)
                             ds)))]))
(define (interp ae fundefs)
  (interp* ae fundefs (mtSub)))

(test (interp (parse '5) empty)
      5)
(test (interp (parse '42) empty)
      42)
(test (interp (parse '(with (x (+ 1 1)) (+ x x))) empty)
      4)

(test/exn (interp (parse '(g 5)) (list (fundef 'g 'm (add (id 'n) (num 1)))))
          "Unbound identifier")
(test (parse '(with (n 5) (f 10)))
      (with 'n (num 5) (app 'f (num 10))))
;;steps             
;;i:   'n  v: (num 5)  e: (app 'f (num 10))
;;(interp* v fun-defs ds)
(test (interp* (num 5) (list (fundef 'f 'p (id 'n))) (mtSub))
      5)
#|
;; with(i v e)
;; (interp* e
                  fun-defs
                  (aSub i 
                        (interp* v fun-defs ds)
                        ds))
|#

(test (interp* (app 'f (num 10))
               (list (fundef 'f 'p (id 'n)))
               (aSub 'n
                     5
                     (mtSub)))
      5)
(test 
 (interp* (num 10) 
          (list (fundef 'f 'p (id 'n)))
          (aSub 'n
                5
                (mtSub)))
 10)
(test (interp* (id 'n)
               (list (fundef 'f 'p (id 'n)))
               (aSub 'p
                     10
                     (aSub 'n
                           5
                           (mtSub))))
      5)
#|
(interp* (num 5)
         (list (fundef 'f 'p (id 'n)))
         (mtSub))

(interp* (app 'f (num 10))
         (list (fundef 'f 'p (id 'n)))
         (aSub 'n 5 (mtSub)))

(interp* (id 'n)
         (list (fundef 'f 'p (id 'n)))
         (aSub 'p
               10
               (mtSub)))
|#         
;(lookup-fundef 'f (list (fundef 'f 'p (id 'n))))
;(fundef-arg-name (fundef 'f 'p (id 'n)))
;(interp* (num 10) (list (fundef 'f 'p (id 'n))) (mtSub))
#|
(test/exn (interp (parse '(with (n 5) (f 10))) (list (fundef 'f 'p (id 'n))))
          "Unbound identifier")


(test/exn (interp (parse '(f 5)) (list (fundef 'f 'n (app 'g (add (id 'n)(num 5))))
                                   (fundef 'g 'm (add (id 'n)(num 1)))))
      "Unbound identifier")


(test (interp (parse '(f 5)) (list (fundef 'f 'n (app 'g (add (id 'n)(num 5))))
                                   (fundef 'g 'm (add (id 'm)(num 1)))))
      11)


(test (interp (with 'x (num 5) (add (id 'x) (id 'x))) empty)
      10)
|#




#|
;; induction vs co-induction
;; recursion vs co-recursion

;; Lisp1 vs (we are Lisp2)

(test (interp (parse '(with (x 5)
                            (+ (+ x x)
                               (* x x))))
              empty)
      35)
|#