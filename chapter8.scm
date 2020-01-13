#lang plai
(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?)(rhs CFAE/L?)]
  [id (name symbol?)]
  [fun (param symbol?)(body CFAE/L?)]
  [app (fun-expr CFAE/L?)(arg-expr CFAE/L?)])

(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body CFAE/L?)
            (env Env?)]
  [exprV (expr CFAE/L?)
         (env Env?)])
(define-type Env
  [mtSub]
  [aSub (name symbol?)(value CFAE/L-Value?)(env Env?)])

(define (num+ x y) ;; need this because we can't just use Scheme + to add FAE-values
  (numV (+ (numV-n x) (numV-n y))))

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
(define (lookup name env)
  (type-case Env env
    [mtSub() (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-ds))]))

(define (interp expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r)(num+ (interp l env)(interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)]
                 [define arg-val (exprV arg-expr env)])
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         arg-val
                         (closureV-env fun-val))))]))
;(interp (parse '{with {x 3} x}) {mtSub})
;(interp {parse '(with (x 3) (+ x x))} (mtSub))
;{parse '(with (x 3) (+ x x))}
;(interp {parse '(with (x 3) (+ x x))} (mtSub))