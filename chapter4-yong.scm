#lang plai

;; 1 + 1
;; + 1 1
;; 1 1 +

;(define-struct num (the-number))
;(define-struct add (the-lhs the-rhs))

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


;; abstract program
(add (num 1)
     (num 1))



;; concrete program
(define e '(+ 1 1))
e
(empty? e)
(cons? e)
(first e)
(second e)
(third e)

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
    (error 'parse "Bd programmer, no cake ~e" c)]))

(parse e)

;; paren'd thing is an s-expression, abbrv'd sexpr
;; <xml><a>stupid</a><way>of writing</way><sexpr /></xml>
;; (xml (a stupid) (way of writing) (sexpr))

(test (parse '(+ 1 1))
      (add (num 1) (num 1)))
(test/exn (parse '(+ 1 1 1))
          "no cake")

(test (parse '(* 3 1))
      (mult (num 3) (num 1)))

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

;; lookup-fundef : symbol? (listof FunDef) -> FunDef
(define (lookup-fundef name fun-defs)
  (cond
    [(empty? fun-defs)
     (error name "function not found")]
    [else
     (if (symbol=? name (fundef-fun-name (first fun-defs)))
         (first fun-defs)
         (lookup-fundef name (rest fun-defs)))]))

;; interp : AE (listof FunDef) -> meaning
(define (interp some-ae fun-defs)
  (type-case
      AE some-ae
    [num (n)
         n]
    [add (lhs rhs)
         (+ (interp lhs fun-defs)
            (interp rhs fun-defs))]
    [mult (lhs rhs)
          (* (interp lhs fun-defs)
             (interp rhs fun-defs))]
    [id (s)
        (error 'interp "Unbound undiscovery infinite: ~e" s)]
    [with (i v e)
      ;(interp (subst i v e))
      (interp (subst i (num (interp v fun-defs)) e) fun-defs)
      ]
    [app (fun-name arg-expr)
         ;; ('double (num 3))
         ;; (fundef 'double 'x (add (id 'x) (id 'x)))
         (local [(define fun-def (lookup-fundef fun-name fun-defs))]
           (interp (subst (fundef-arg-name fun-def)
                          (num (interp arg-expr fun-defs))
                          (fundef-body fun-def))
                   fun-defs))]))
(test (parse '(with (n 5)
                    (f 10)))
      (with 'n (num 5) (app 'f (num 10))))
;;steps
;;i: 'n  v: (num 5) e: (app 'f (num 10))
(test (interp (num 5) (list (fundef 'f 'p (id 'n))))
      5)
(test (subst 'n (num 5) (app 'f (num 10)))
      (app 'f (num 10)))
(test/exn (interp (app 'f (num 10))  (list (fundef 'f 'p (id 'n))))
      "Unbound undiscovery infinite:")

(test (interp (num 10) (list (fundef 'f 'p (id 'n))))
      10)

(test (subst 'p (num 10) (id 'n))
      (id 'n))
(test/exn (interp (id 'n) (list (fundef 'f 'p (id 'n))))
          "Unbound undiscovery infinite:")

(test/exn (interp (parse '(with (n 5)
                      (f 10))) (list (fundef 'f 'p (id 'n))))
      "Unbound undiscovery infinite:")