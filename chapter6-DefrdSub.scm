#lang plai
(define-type FAE
  (num (n number?))
  (id (name symbol?))
  (add (lt FAE?) (rt FAE?))
  (fun (param symbol?) (body FAE?))
  (app (fun-exp FAE?) (arg-exp FAE?)))

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body FAE?)
            (ds DefrdSub?)])

(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?)
          (value FAE-Value?)  ;but only num and fun allowed
          (ds DefrdSub?)])

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

(define (interp expr ds)
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
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-ds))]))
(interp {parse '{with {x 3}
                {fun {y} {+ x y}}}} {mtSub})
;=> (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub)))