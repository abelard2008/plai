#lang plai
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)
       (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(with) (with (first (second sexp))
                     (parse (second (second sexp)))
                     (parse (third sexp)))]
       [else (app (first sexp)
                   (parse (second sexp)))])]
    
    [(symbol? sexp) (id sexp)]))

(define (subst i v sexp)
   (type-case F1WAE sexp
     [num (n) sexp]
     [add (lhs rhs)
          (add (subst i v lhs)
               (subst i v rhs))]
     [with (name named-sexp body)
           (if (symbol=? name i)
               (with name
                     (subst i v named-sexp)
                     body)
               (with name
                     (subst i v named-sexp)
                     (subst i v body)))]
     [app (fun-name arg-name)
          (app fun-name arg-name)]       
     [id (name) 
         (if (symbol=? name i)
             v
             sexp)]))

(define (lookup-fundef fun-name fundefs)
  (cond
    [(empty? fundefs) (error fun-name "function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundef fun-name (rest fundefs)))]))

(define (interp sexp fun-defs)
  (type-case F1WAE sexp
    [num (n) n]
    [add (lhs rhs)
         (+ (interp lhs fun-defs)
            (interp rhs fun-defs))]
    [with (name named-sexp body)
          (interp (subst name
                         (interp named-sexp fun-defs)
                         body))]
    [app (fun-name arg-name)
         (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (subst (fundef-arg-name the-fun-def)
                          (num (interp arg-name fun-defs))
                          (fundef-body the-fun-def)) 
                   fun-defs))]
    [id (name) (error 'interp "free identifier")]))

              
    