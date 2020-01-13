#lang plai
(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?)
       (rhs FWAE?)]
  [with (name symbol?)
        (named-expr FWAE?)
        (body FWAE?)]
  [fun (param symbol?)
       (body FWAE?)]
  [app (fun-expr FWAE?)
       (parameter FWAE?)]
  [id (name symbol?)])

(define-type FWAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body FWAE?)
            (ds DfrdSub?)])

(define-type DfrdSub
  [mtSub]
  [aSub (name symbol?) (value FWAE-Value?) (ds DfrdSub?)])


(define (parse sexp)
  (cond 
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(with) 
        (with (first (second sexp))
              (parse (second (second sexp)))
              (parse (third sexp)))]
       [(fun) (fun (first (second sexp))
                   (parse (third sexp)))]
       [else (app (parse (first sexp))
                  (parse (second sexp)))])]
    [(symbol? sexp) (id sexp)]))

(test (parse '{fun (x) x})
       (fun 'x (id 'x)))     


(define (subst i v sexp)
  (type-case FWAE sexp
    [num (n) (num n)]
    [add (lhs rhs) (add (subst i v lhs) (subst i v rhs))]
    [with (bound-id named-expr body)
          (if (symbol=? i bound-id)
              (with bound-id
                    (subst i v named-expr)
                    body)
              (with bound-id
                    (subst i v named-expr)
                    (subst i v body)))]
    [fun (param-name fun-body)
         (fun param-name (subst i v fun-body))]
    [app (fun-expr parameter)
         (app (subst i v fun-expr)
              (subst i v parameter))]
    [id (name) (if (symbol=? i name)
                   v
                   sexp)]))
(define (addHelper lhs rhs)
  (numV (+ (numV-n lhs) (numV-n rhs))))
     
(define (lookup v ds)
  (type-case DfrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name v)
              bound-value
              (lookup v rest-ds))]))
#|
;deferring interp
(define (interp/DfrdSt sexp ds)
  (type-case FWAE sexp
    [num (n) sexp]
    [add (lhs rhs) (addHelper (interp/DfrdSt lhs ds)
                              (interp/DfrdSt rhs ds))]
    [with (bound-id named-expr body)
          (interp/DfrdSt body
                  (aSub bound-id
                        (interp/DfrdSt named-expr ds)
                        ds))]
    [fun (param-name fun-body)
         sexp]
    [app (fun-sexp parameter)
         (local ([define fun-val (interp/DfrdSt fun-sexp ds)])
           (interp/DfrdSt (fun-body fun-val)
                   (aSub (fun-param fun-val)
                         (interp/DfrdSt parameter ds)
                         ds) ))]
    
    [id (name) (lookup name ds)]))

(test (interp/DfrdSt (parse '{{{fun {x} x}
        {fun (x) {+ x 5}}} 3}) (mtSub))
      8)

(test (interp/DfrdSt (parse '{with {x 3}
                            {fun {y}
                                 {+ x y}}}) (mtSub))
      (fun 'y (add (id 'x) (id 'y))))

(test (interp/DfrdSt (parse '{with {x 3}
                                   {with {f {fun {y} {+ x y}}}
                                         {with {x 5}
                                               {f 4}}}}) {mtSub})
      9)

(test (interp/DfrdSt (parse '{with {x 3}
                                  {fun {y}
                                       {+ x y}}}) {mtSub})
      (fun 'y (add (id 'x) (id 'y))))
|#




;deferring interp with closure
(define (interp/closureDS sexp ds)
  (type-case FWAE sexp
    [num (n) (numV n)]
    [add (lhs rhs) (addHelper (interp/closureDS lhs ds)
                              (interp/closureDS rhs ds))]
    [with (bound-id named-expr body)
          (interp/closureDS body
                  (aSub bound-id
                        (interp/closureDS named-expr ds)
                        ds))]
    [fun (param-name fun-body)
         (closureV param-name fun-body ds)]
    [app (fun-sexp parameter)
         (local ([define fun-val (interp/closureDS fun-sexp ds)])
           (interp/closureDS (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         (interp/closureDS parameter ds)
                         (closureV-ds fun-val))))]
    
    [id (name) (lookup name ds)]))

(test (interp/closureDS (parse '{with {x 3}
                                  {fun {y}
                                       {+ x y}}}) (mtSub))
(closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))

(test (interp/closureDS (parse '{with {x 3} {+ x x}}) (mtSub))
      (numV 6))

#|
(define (interp/subst sexp)
  (type-case FWAE sexp
    [num (n) sexp]
    [add (lhs rhs) (addHelper (interp/subst lhs)
                      (interp/subst rhs))]
    [with (bound-id named-expr body)
          (interp/subst (subst bound-id 
                               (interp/subst named-expr)
                               body))]
    [fun (param-name fun-body)
         sexp]
    [app (fun-sexp parameter)
         (local ([define fun-val (interp/subst fun-sexp)])
           (interp/subst (subst (fun-param fun-val)
                          (interp/subst parameter)
                          (fun-body fun-val))))]
    [id (name) (error 'interp/subst "free identifier")]))

(test (interp/subst (parse '{{{fun {x} x}
        {fun (x) {+ x 5}}} 3}))
      8)
(test (interp/subst (parse '{with {x 3}
                            {fun {y}
                                 {+ x y}}}))
      (fun 'y (add (num 3) (id 'y))))

(test (parse '{with {x 3}
                    {fun {y}
                         {+ x y}}})
      (with 'x (num 3) (fun 'y (add (id 'x) (id 'y)))))
 (test (interp/subst (parse '{with {x 3}
                                   {with {f {fun {y} {+ x y}}}
                                         {with {x 5}
                                               {f 4}}}}))
       7)
(test (interp/subst (parse '{with {x 3}
                                  {fun {y}
                                       {+ x y}}}))
      (fun 'y (add (num 3) (id 'y))))
  
|#
     
                        
    
    