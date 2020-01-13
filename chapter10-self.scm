#lang plai
;; abstract syntax
(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?)
       (rhs RCFAE?)]
  [sub (lhs RCFAE?)
       (rhs RCFAE?)]
  [mult (lhs RCFAE?)
        (rhs RCFAE?)]
  [if0 (cond-e RCFAE?)
       (true-e RCFAE?)
       (false-e RCFAE?)]
  [rec (name symbol?)
       (named-expr RCFAE?)
       (body RCFAE?)]
  [id (s symbol?)]
  [fun (arg-name symbol?)
       (body RCFAE?)]
  [app (fun-expr RCFAE?) 
       (arg RCFAE?)])

(define (parse sexp)
  (cond 
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(-) (sub (parse (second sexp))
                 (parse (third sexp)))]
       [(*) (mult (parse (second sexp))
                 (parse (third sexp)))]
       [(if0) (if0 (parse (second sexp))
                   (parse (third sexp))
                   (parse (fourth sexp)))]
       [(with) 
        (app (fun (first (second sexp))        ; 
                  (parse (third sexp)))        ; Solves exercise 6.3.1 
             (parse (second (second sexp))))]  ;
       [(rec)
         (rec (first (second sexp))
           (parse (second (second sexp)))
           (parse (third sexp)))]       
       [(fun) (fun (first (second sexp))
                   (parse (third sexp)))]
       [else (app (parse (first sexp))
                  (parse (second sexp)))])]
    [(symbol? sexp) (id sexp)]))

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (env Env?)])

(define (boxed-RCFAE-Value? v)
  (and (box? v)
       (RCFAE-Value? (unbox v))))

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value RCFAE-Value?)
        (env Env?)]
  [aRecSub (name symbol?)
           (value boxed-RCFAE-Value?)
           (env Env?)])

;;lookup: symbol env -> RCFAE-Value
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]
    [aRecSub (bound-name boxed-bound-value rest-env)
             (if (symbol=? bound-name name)
                 (unbox boxed-bound-value)
                 (lookup name rest-env))]))
;#0=(closureV 'n (if0 (id 'n) (num 1) (mult (id 'n) (app (id 'fac) (sub (id 'n) (num 1))))) (aRecSub 'fac '#&#0# (mtSub)))

;;cyclically-bind-and-interp: symbol RCFAE env --> env
(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box (numV 1729))]
          [define new-env (aRecSub bound-id value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
    (begin
     ; (printf "new-env ~e \n" new-env)
      (set-box! value-holder named-expr-val)
     ; (printf "new-env ~e \n" new-env)
      new-env)))
;#0=(aRecSub 'fac (box (closureV 'n (if0 (id 'n) (num 1) (mult (id 'n) (app (id 'fac) (sub (id 'n) (num 1))))) #0#)) (mtSub))

(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))


(define (mult* n1 n2)
  (numV (* (numV-n n1) (numV-n n2))))


(define (sub- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))

(define (num-zero? n)
  (zero? (numV-n  n)))

(define (interp expr env)
 ; (displayln (list 'expr expr 'env env))
  (type-case RCFAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [sub (l r) (sub- (interp l env) (interp r env))]
    [mult (l r) (mult* (interp l env) (interp r env))]
    [if0 (test truth falsity)
         (if (num-zero? (interp test env))
             (interp truth env)
             (interp falsity env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)])
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         (interp arg-expr env)
                         (closureV-env fun-val))))]
    [rec (bound-id named-expr bound-body)
         (interp bound-body
                 (cyclically-bind-and-interp bound-id
                                             named-expr
                                             env))]))
(test (parse '(rec (fac
                    (fun (n)
                         (if0 n
                              1
                              (* n (fac (- n 1))))))
                (fac 2)))
(rec 'fac (fun 'n (if0 (id 'n) (num 1) (mult (id 'n) (app (id 'fac) (sub (id 'n) (num 1)))))) (app (id 'fac) (num 2))))

;(app (id 'fac) (nun 2)
;(interp (id 'fac) new-env)
;(lookup 'fac new-env)
;(closureV-body (lookup 'fac new-env)): (if0 (id 'n) (num 1) (mult (id 'n) (app (id 'fac) (sub (id 'n) (num 1)))))


(interp (parse '(+ 2 -1)) (mtSub))
              

(test (interp (parse '(fun (x) x)) (mtSub))
      (closureV 'x (id 'x) (mtSub)))

(test (interp (parse '(rec (fac
                            (fun (n)
                                 (if0 n
                                      1
                                      (* n (fac (- n 1))))))
                        (fac 2))) (mtSub)) 
              (numV 2))
;(closureV 'n (if0 (id 'n) (num 1) (mult (id 'n) (app (id 'fac) (sub (id 'n) (num 1))))) (aRecSub 'fac '#&#0# (mtSub)))

#|
(expr #(struct:rec fac #(struct:fun n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1)))))) #(struct:app #(struct:id fac) #(struct:num 2))) env #(struct:mtSub))
(expr #(struct:fun n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1)))))) env #(struct:aRecSub fac #&#(struct:numV 1729) #(struct:mtSub)))
(expr #(struct:app #(struct:id fac) #(struct:num 2)) env #0=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))
(expr #(struct:id fac) env #0=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))
(expr #(struct:num 2) env #0=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) #0#) #(struct:mtSub)))
(expr #0=#(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) env #(struct:aSub n #(struct:numV 2) #1=#(struct:aRecSub fac #&#(struct:closureV n #0# #1#) #(struct:mtSub))))
(expr #0=#(struct:id n) env #(struct:aSub n #(struct:numV 2) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #0# #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) #1#) #(struct:mtSub))))
(expr #0=#(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1)))) env #(struct:aSub n #(struct:numV 2) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #0#) #1#) #(struct:mtSub))))
(expr #0=#(struct:id n) env #(struct:aSub n #(struct:numV 2) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #0# #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) #1#) #(struct:mtSub))))
(expr #0=#(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))) env #(struct:aSub n #(struct:numV 2) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #0#)) #1#) #(struct:mtSub))))
(expr #0=#(struct:id fac) env #(struct:aSub n #(struct:numV 2) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #0# #(struct:sub #(struct:id n) #(struct:num 1))))) #1#) #(struct:mtSub))))
(expr #0=#(struct:sub #(struct:id n) #(struct:num 1)) env #(struct:aSub n #(struct:numV 2) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #0#))) #1#) #(struct:mtSub))))
(expr #0=#(struct:id n) env #(struct:aSub n #(struct:numV 2) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #0# #(struct:num 1))))) #1#) #(struct:mtSub))))
(expr #0=#(struct:num 1) env #(struct:aSub n #(struct:numV 2) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #0#)))) #1#) #(struct:mtSub))))
(expr #0=#(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) env #(struct:aSub n #(struct:numV 1) #1=#(struct:aRecSub fac #&#(struct:closureV n #0# #1#) #(struct:mtSub))))
(expr #0=#(struct:id n) env #(struct:aSub n #(struct:numV 1) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #0# #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) #1#) #(struct:mtSub))))
(expr #0=#(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1)))) env #(struct:aSub n #(struct:numV 1) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #0#) #1#) #(struct:mtSub))))
(expr #0=#(struct:id n) env #(struct:aSub n #(struct:numV 1) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #0# #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) #1#) #(struct:mtSub))))
(expr #0=#(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))) env #(struct:aSub n #(struct:numV 1) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #0#)) #1#) #(struct:mtSub))))
(expr #0=#(struct:id fac) env #(struct:aSub n #(struct:numV 1) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #0# #(struct:sub #(struct:id n) #(struct:num 1))))) #1#) #(struct:mtSub))))
(expr #0=#(struct:sub #(struct:id n) #(struct:num 1)) env #(struct:aSub n #(struct:numV 1) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #0#))) #1#) #(struct:mtSub))))
(expr #0=#(struct:id n) env #(struct:aSub n #(struct:numV 1) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #0# #(struct:num 1))))) #1#) #(struct:mtSub))))
(expr #0=#(struct:num 1) env #(struct:aSub n #(struct:numV 1) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #0#)))) #1#) #(struct:mtSub))))
(expr #0=#(struct:if0 #(struct:id n) #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) env #(struct:aSub n #(struct:numV 0) #1=#(struct:aRecSub fac #&#(struct:closureV n #0# #1#) #(struct:mtSub))))
(expr #0=#(struct:id n) env #(struct:aSub n #(struct:numV 0) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #0# #(struct:num 1) #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) #1#) #(struct:mtSub))))
(expr #0=#(struct:num 1) env #(struct:aSub n #(struct:numV 0) #1=#(struct:aRecSub fac #&#(struct:closureV n #(struct:if0 #(struct:id n) #0# #(struct:mult #(struct:id n) #(struct:app #(struct:id fac) #(struct:sub #(struct:id n) #(struct:num 1))))) #1#) #(struct:mtSub))))
|#