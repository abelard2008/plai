#lang plai
 (define-type AE
    [num (n number?)]
    [add (lhs AE?)
         (rhs AE?)]
    [sub (lhs AE?)
         (rhs AE?)]) 
(define (parse sexp)
    (cond
      [(number? sexp) (num sexp)]
      [(list? sexp)
       (case (first sexp)
         [(+) (add (parse (second sexp))
                   (parse (third sexp)))]
         [(-) (sub (parse (second sexp))
                   (parse (third sexp)))])]))


(define (calc an-ae)

(type-case AE an-ae

[num (n) n]

[add (l r) (+ (calc l) (calc r))]

[sub (l r) (- (calc l) (calc r))]
(define-type AE
      [num (n number?)]
   	   [add (lhs AE?)
           (rhs AE?)]
      [sub (lhs AE?)
           (rhs AE?)])
(define (parse sexp)
      (cond
        [(number? sexp) (num sexp)]
        [(list? sexp)
         (case (car sexp)
           [(+) (add (parse (cadr sexp))
                     (parse (caddr sexp)))]
           [(-) (sub (parse (cadr sexp))
                     (parse (caddr sexp)))])]))
                   (parse (caddr sexp)))])]))
(define (calc an-ae)
(define-type WAE
    (numx (n number?))
    (addx (lhs WAE?) (rhs WAE?))
    (subx (lhs WAE?)(rhs WAE?))
    (with (name symbol?)(named-expr WAE?)(body WAE?))
    (id (name symbol?)))
 (with {x {+ 5 5}}{+ x x})

(type-case AE an-ae

	   [num (n) n]

	   [add (l r) (+ (calc l) (calc r))]

	   [sub (l r) (- (calc l) (calc r))]))

(define-type WAE
    (num (n number?))
    (add (lhs WAE?) (rhs WAE?))
    (sub (lhs WAE?)(rhs WAE?))
    (with (name symbol?)(named-expr WAE?)(body WAE?))
    (id (name symbol?)))

(define-type WAE
    [num (n number?)]
    [add (lhs WAE?)(rhs WAE?)]
    [sub (lhs WAE?) (rhs WAE?)]
    [with (name symbol?) (named-expr WAE?)(body WAE?)]
    [id (name symbol?)])

(define (calc expr)
    (type-case WAE expr
      [num (n) n]
      [add (l r) (+ (calc l) (calc r))]
      [sub (l r) (- (calc l) (calc r))]
      [with (bound-id named-expr bound-body)
            (calc (subst bound-body
                         bound-id
                         (num (calc named-expr))))]
           [id (v) (error 'calc "free identifier")]))

(define (subst expr sub-id val)
    (type-case WAE expr
      [num (n) expr]
      [add (l r) (add (subst l sub-id val)
                      (subst r sub-id val))]
      [sub (l r) (sub (subst l sub-id val)
                      (subst r sub-id val))]
      [with (bound-id named-expr bound-body)
            (if (symbo=? bound-id sub-id)
                expr
                (with bound-id
                      named-expr
                      (subst bound-body sub-id val)))]
      [id (v) (if (symbol=? v sub-id) val expr)]))

(deﬁne (subst expr sub-id val)
       (type-case WAE expr
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
		  [id (v) (if (symbol=? v sub-id) val expr)]))

(define (calc expr)
    (type-case WAE expr
      [num (n) n]
      [add (l r) (+ (calc l) (calc r))]
      [sub (l r) (- (calc l) (calc r))]
      [with (bound-id named-expr bound-body)
            (calc (subst bound-body
                         bound-id
                         (num (calc named-expr))))]
           [id (v) (error 'calc "free identifier")]))