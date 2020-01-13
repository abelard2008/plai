#lang plai
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)(rhs F1WAE?)]

  [with (name symbol?) (named-expr F1WAE?)(body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)(arg F1WAE?)])
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(value number?)(ds DefrdSub?)])

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-ds))]))
(define (lookup-fundef fun-name fundefs)
  (cond
    [(empty? fundefs)(error fun-name "function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundef fun-name (rest fundefs)))]))

(define (interp expr fun-defs ds)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs ds)(interp r fun-defs ds))]
    [with (bound-id named-expr bound-body)
          (interp bound-body
                  fun-defs
                  (aSub bound-id
                        (interp named-expr
                                fun-defs
                                ds)
                        ds) 
                  )]
    [id (v) (lookup v ds)]
    [app (fun-name arg-expr)
         (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (fundef-body the-fun-def)
                   fun-defs
                   (aSub (fundef-arg-name the-fun-def)
                         (interp arg-expr fun-defs ds)
                         ds)))]))
(define parse
  (lambda (sexp)
    (cond
      [(number? sexp) (num sexp)]
      [(symbol? sexp) (id sexp)]
      [(list? sexp)
       (case (first sexp)
         [(+)(add (parse (second sexp))
                  (parse (third sexp)))]
         [(with) (with (first (second sexp))
                       (parse (second (second sexp)))
                       (parse (third sexp)))]
         [else (app (first sexp)
                    (parse (second sexp)))]
         )])))