#lang plai
(define-type CFAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lt CFAE?) (rt CFAE?)]
  [fun (arg-name symbol?) (body CFAE?)]
  [app (fun-exp CFAE?) (arg-expr CFAE?)]
  [if0 (test-exp CFAE?) (then-exp CFAE?) (else CFAE?)])
  
 (define (parse sexp)
  (cond 
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
#|       [(-) (sub (parse (second sexp))
                 (parse (third sexp)))]
       [(*) (mult (parse (second sexp))
                 (parse (third sexp)))]
|#
       [(if0) (if0 (parse (second sexp))
                   (parse (third sexp))
                   (parse (fourth sexp)))]
       [(with) 
        (app (fun (first (second sexp))        ; 
                  (parse (third sexp)))        ; Solves exercise 6.3.1 
             (parse (second (second sexp))))]  ;
#|       [(rec)
         (rec (first (second sexp))
           (parse (second (second sexp)))
           (parse (third sexp)))]       
|#
       [(fun) (fun (first (second sexp))
                   (parse (third sexp)))]
       [else (app (parse (first sexp))
                  (parse (second sexp)))])]
    [(symbol? sexp) (id sexp)]))

 (define-type Env
   [mtEnv]
   [anEnv (id symbol?)
         (val CFAE-Value?)
         (more-subs Env?)])
 
 (define (lookup name env)
   (type-case Env env
     [mtEnv () (error 'lookup "free identifier ~a" name)]
     [anEnv (bound-name bound-value rest-env)
           (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]))
  
(define-type CFAE-Value
         [numV (n number?)]
         [closureV (p procedure?)])

 (define (num-zero? n)
  (zero? (numV-n  n)))
 
;;interp:CFAE Env receiver -> doesn't return
(define (interp expr env k)
  (type-case CFAE expr
    [num (n) (k (numV n))]
    [add (l r) (interp l env
                      (lambda (lv)
                        (interp r env
                               (lambda (rv)
                                 (k (numV (+ (numV-n lv)
                                            (numV-n rv))))))))]
    [if0 (test truth falsity)
        (interp test env
               (lambda (tv)
                 (if (num-zero? tv)
                    (interp truth env k)
                    (interp falsity env k))))]
    [id (v) (k (lookup v env))]
    [fun (param body)
        (k (closureV (lambda (arg-val dyn-k)
                       (interp body (anEnv param arg-val env) dyn-k))))]
    [app (fun-expr arg-expr)
        (interp fun-expr env
               (lambda (fun-val)
                 (interp arg-expr env
                        (lambda (arg-val)
                          ((closureV-p fun-val)
                           arg-val k)))))]))

(define (run exp)
  (let/cc k
    (begin
      (interp exp (mtEnv) k)
      (error "Interp returned, which is an Error!"))))

(test (run (parse '1)) (numV 1))
(test (run (parse '{+ 1 2})) (numV 3))

;(test (run (parse '{+ 4}