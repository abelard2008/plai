#lang plai
(define-type FAE
  (num (n number?))
  (id (name symbol?))
  (add (lt FAE?) (rt FAE?))
  (fun (param symbol?) (body FAE?))
  (app (fun-exp FAE?) (arg-exp FAE?)))

#;(define (Env? x)
  (procedure? x))

#;(define (mtSub)
  (lambda (name)
    (error 'lookup "no binding for identifier")))

#;(define (aSub bound-name bound-value env)
  (lambda (want-name)
    (cond 
      [(symbol=? want-name bound-name) bound-value]
      [else (lookup want-name env)])))

#;(define (lookup name env)
  (env name))

#;(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body FAE?)
            (ds Env?)])
(define (number-or-procedure? v)
  (or (number? v)
      (procedure? v)))

(define-type Env
  [mtSub]
  [aSub (name symbol?) (value number-or-procedure?) (env Env?)])

(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]))

(define-type FAE-Value
  [numV (n number?)]
  [closureV (p procedure?)])

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

(define (interp expr env)
  (type-case FAE expr
    [num (n)  n]
    [add (l r) (+ (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (lambda (arg-val)
                     (interp bound-body
                             (aSub bound-id arg-val env)))]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)]
                 [define arg-val (interp arg-expr env)])
           (fun-val arg-val))]))


#;(define (interp expr env)
  (type-case FAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV (lambda (arg-val)
                     (interp bound-body
                             (aSub bound-id arg-val env))))]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)]
                 [define arg-val (interp arg-expr env)])
           ((closureV-p fun-val)
            arg-val))]))
#;(define (interp expr ds)
  (type-case FAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [id (v) (lookup v ds)]
  ;  [fun (bound-id bound-body) expr]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body ds)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr ds)])
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         (interp arg-expr ds)
                         (closureV-ds fun-val))))]))

(interp {parse '{with {x 3}
                            {fun {y} {+ x y}}}} {mtSub})

(test (interp {parse '{with {x 3}
                            {+ x x}}} {mtSub})
      6)