#lang plai
(define-type FAE
  (num (n number?))
  (id (name symbol?))
  (add (lt FAE?) (rt FAE?))
  (fun (param symbol?) (body FAE?))
  (app (fun-exp FAE?) (arg-exp FAE?)))

(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?)
          (value FAE?)  ;but only num and fun allowed
          (ds DefrdSub?)])

(define (num+ x y) ;; need this because we can't just use Scheme + to add FAE-values
  (+ (num-n x) (num-n y)))

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
    [num (n) expr]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [id (v) (lookup v ds)]
    [fun (bound-id bound-body) expr]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr ds)])
           (interp (fun-body fun-val)
                   (aSub (fun-param fun-val)
                         (interp arg-expr ds)
                         ;ds ; bad -> dynamic scope!
                         (mtSub) ;
                         )))]))
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub() (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-ds))]))
;example
;(interp {parse '{with {x 3}
;                {fun {y} {+ x y}}}} {mtSub})
; => (fun 'y (add (id 'x) (id 'y))) 
;3 was lost
 