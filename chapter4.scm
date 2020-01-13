#lang plai
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)(rhs F1WAE?)]
  [sub (lhs F1WAE?)(rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?)(body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)(arg F1WAE?)])
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])
(define (interp expr fun-defs)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs)(interp r fun-defs))]
    [sub (l r) (- (interp l fun-defs)(interp r fun-defs))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body
                         bound-id
                         (num (interp named-expr fun-defs)))
                  fun-defs)]
    [id (v) (error 'interp "free identifier")]
    [app (fun-name arg-expr)
         (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (subst (fundef-body the-fun-def)
                          (fundef-arg-name the-fun-def)
                          (num (interp arg-expr fun-defs)))
                   fun-defs))]))
(define (lookup-fundef fun-name fundefs)
  (cond
    [(empty? fundefs)(error fun-name "function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundef fun-name (rest fundefs)))]))
(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val)
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [app (fun-name arg-expr)
         (app fun-name (subst arg-expr sub-id val))]))
(define parse
  (lambda (sexp)
    (cond
      [(number? sexp) (num sexp)]
      [(symbol? sexp) (id sexp)]
      [(list? sexp)
       (case (first sexp)
         [(+)(add (parse (second sexp))
                  (parse (third sexp)))]
         [(-)(sub (parse (second sexp))
                  (parse (third sexp)))]
         [(with) (with (first (second sexp))
                       (parse (second (second sexp)))
                       (parse (third sexp)))]
         [else (app (first sexp)
                    (parse (second sexp)))]
         )])))
;(app 'double (app 'double (num 5))) => (app 'double (app 'double (num 5)))
; (fundef-arg-name (first (list (fundef 'double 'n (add (id 'n) (id 'n))))))
; => 'double
; (fundef-arg-name (first (list (fundef 'double 'n (add (id 'n) (id 'n))))))
; => 'n
;(subst (add (id 'n) (id 'n)) 'n (num (interp (app 'double (num 5)) (list (fundef 'double 'n (add (id 'n) (id 'n))))))) 
;   = > (add (num 10) (num 10))
;(interp (add (num 10) (num 10)) (list (fundef 'double 'n (add (id 'n) (id 'n))))) 
; => 20
;(interp (parse '{double {double 5}})
;        (list (fundef 'double
;                      'n
 ;                     (add (id 'n) (id 'n)))))
; => 20
; (parse '{double {double 5}})
; => (app 'double (app 'double (num 5)))
;(interp (parse '{f 5})
         ;(list (fundef 'f 'n (app 'g (add (id 'n) (num 5))))
        ;       (fundef 'g 'm (sub (id 'm) (num 1)))))
; => 9