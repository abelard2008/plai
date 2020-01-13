#lang plai
(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?)(rhs FWAE?)]
  [with (name symbol?)(named-expr FWAE?)(body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body FWAE?)]
  [app (fun-expr FWAE?)(arg-expr FWAE?)])
(define (add-numbers x y) ;; need this because we can't just use Scheme + to add FAE-values
  (+ (num-n x) (num-n y)))
(define (interp expr)
  (type-case FWAE expr
    [num (n) expr]
    [add (l r) (add-numbers (interp l) (interp r))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body
                         bound-id
                         (interp named-expr)))]
    [id (v) (error 'interp "free identifier")]
    [fun (bound-id bound-body) expr] 
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr)])
           (interp (subst (fun-body fun-val)
                          (fun-param fun-val)
                          (interp arg-expr))))]))
(define (subst expr sub-id val)
  (type-case FWAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
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
    [fun (bound-id bound-body)
         (if (symbol=? bound-id sub-id)
             (fun bound-id bound-body); expr
             (fun bound-id (subst bound-body sub-id val)))]
    [app (fun-name arg-expr)
         (app fun-name (subst arg-expr sub-id val))]))
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(symbol? sexp) (id sexp)]
        [(list? sexp)
         (case (first sexp)
           ((+)
            (add (parse (second sexp))
                 (parse (third sexp))))
           ((with)                                 ; Notice how we parse with!!!!
            (app (fun (first (second sexp))        ; 
                      (parse (third sexp)))        ; Solves exercise 6.3.1 
                 (parse (second (second sexp)))))  ;
           ((fun)                                  
            (fun (first (second sexp))             
                 (parse (third sexp))))            
           (else
            (app (parse (first sexp))            
                 (parse (second sexp)))))]))

