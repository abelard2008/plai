#lang plai
(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?) (rhs CFAE/L?)]
  [id (name symbol?)]
  [fun (param symbol?) (body CFAE/L?)]
  [app (fun-expr CFAE/L?) (arg-expr CFAE/L?)])

(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body CFAE/L?)
            (env Env?)]
  [exprV (expr CFAE/L?)
         (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) (value CFAE/L-Value?) (env Env?)])

(define (num+ n1 n2)
;  (numV (+ (numV-n n1) (numV-n n2))))
  (numV (+ (numV-n (strict n1)) (numV-n (strict n2)))))

(define (num-zero? n)
  (zero? (numV-n (strict n))))

(define (parse sexp)
  (cond 
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(with) 
        (app (fun (first (second sexp))        ; 
                  (parse (third sexp)))        ; Solves exercise 6.3.1 
             (parse (second (second sexp))))]  ;
              
       [(fun) (fun (first (second sexp))
                   (parse (third sexp)))]
       [else (app (parse (first sexp))
                  (parse (second sexp)))])]
    [(symbol? sexp) (id sexp)]))

(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (expr env)
           (local ([define the-value (strict (interp expr env))])
             (begin
               (printf "Forcing exprV to ~a ~n" the-value)
               the-value))]
    [else e]))
(define (lookup v ds)
  (type-case Env ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name v)
              bound-value
              (lookup v rest-ds))]))

(define (interp expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
           (local ([define fun-val (strict (interp fun-expr env))]
         ;(local ([define fun-val (interp fun-expr env)]
                 [define arg-val (exprV arg-expr env)])
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         arg-val
                         (closureV-env fun-val))))]))
(test (interp (parse '(with {x 3} x)) (mtSub))
      (exprV (num 3) (mtSub)))
(test (interp (parse '(with (x (+ 4 5))
                            (with (y (+ x x))
                                  (with (z y)
                                        (with (x 4)
                                              z))))) (mtSub))
      (exprV
       (id 'y)
       (aSub
        'y
        (exprV (add (id 'x) (id 'x)) (aSub 'x (exprV (add (num 4) (num 5)) (mtSub)) (mtSub)))
        (aSub 'x (exprV (add (num 4) (num 5)) (mtSub)) (mtSub)))))

(test (interp (parse '(with (double (fun (x) (+ x x)))
                      (+ (double 5)
                         (double 10)))) (mtSub))
      (numV 30))
(test (interp (parse '{with (f {undef x})
                4}) (mtSub))
      (numV 4))