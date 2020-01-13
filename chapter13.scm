#lang plai

(define-type BCFAE
  [num (n number?)]
  [add (lhs BCFAE?)
       (rhs BCFAE?)]
  [sub (lhs BCFAE?)
       (rhs BCFAE?)]
  [id (name symbol?)]
  [if0 (cond-e BCFAE?)
       (true-e BCFAE?)
       (false-e BCFAE?)]
  [fun (param symbol?)
       (body BCFAE?)]
  [app (fun-expr BCFAE?)
       (arg-expr BCFAE?)]
  [newbox (val-expr BCFAE?)]
  [setbox (box-expr BCFAE?)
          (val-expr BCFAE?)]
  [openbox (box-expr BCFAE?)]
  [seqn (first-expr BCFAE?)
        (second-expr BCFAE?)])

(define-type BCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body BCFAE?)
            (env Env?)]
  [boxV (location number?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (location number?)
        (env Env?)])

(define-type Store
  [mtSto]
  [aSto (location integer?)
        (value BCFAE-Value?)
        (store Store?)])

(define-type ValuexStore
  [vxs (value BCFAE-Value?)
       (store Store?)])

;; ----------------------------------------

;; parse : S-expr -> BCFAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(pair? sexp)
     (case (car sexp)
       [(+) (add (parse (second sexp)) (parse (third sexp)))]
       [(-) (sub (parse (second sexp)) (parse (third sexp)))]
       [(if0) (if0 (parse (second sexp))
                   (parse (third sexp))
                   (parse (fourth sexp)))]
       [(with)                                 ; Notice how we parse with!!!!
        (app (fun (first (second sexp))        ; 
                  (parse (third sexp)))        ; Solves exercise 6.3.1 
             (parse (second (second sexp))))]  ;
       [(fun) (fun (first (second sexp)) (parse (third sexp)))]
       [(newbox) (newbox (parse (second sexp)))]
       [(setbox) (setbox (parse (second sexp)) (parse (third sexp)))]
       [(openbox) (openbox (parse (second sexp)))]
       [(seqn) (seqn (parse (second sexp)) (parse (third sexp)))]
       [else (app (parse (first sexp)) (parse (second sexp)))])]))

(test (parse '(with {x 3} {+ x x}))
      {app {fun 'x (add (id 'x) (id 'x))} (num 3)})
(test (parse 3) (num 3))
(test (parse 'x) (id 'x))
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{- 1 2}) (sub (num 1) (num 2)))
(test (parse '{fun {x} x}) (fun 'x (id 'x)))
(test (parse '{1 2}) (app (num 1) (num 2)))
(test (parse '{newbox 1}) (newbox (num 1)))
(test (parse '{setbox 1 2}) (setbox (num 1) (num 2)))
(test (parse '{openbox 1}) (openbox (num 1)))
(test (parse '{seqn 1 2}) (seqn (num 1) (num 2)))

(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))

(define (num- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))

(define (num-zero? n)
  (zero? (numV-n  n)))

;; lookup : symbol DefrdSub -> BCFAE-Value
(define (env-lookup name ds)
  (type-case Env ds
    [mtSub () (error 'lookup "free variable")]
    [aSub (sub-name val rest-ds)
          (if (symbol=? sub-name name)
              val
              (env-lookup name rest-ds))]))

;; store-lookup : number Store -> BCFAE-Value
(define (store-lookup addr st)
  (type-case Store st
    [mtSto () (error 'store-lookup "unallocated")]
    [aSto (sto-addr val rest-st)
          (if (= addr sto-addr)
              val
              (store-lookup addr rest-st))]))
(define next-location
  (local ([define last-loc (box -1)])
    (lambda (store)
      (begin
        (set-box! last-loc (+ 1 (unbox last-loc)))
        (unbox last-loc)))))

;interp: BCFAE Env Store --> Value X Store
(define (interp expr env store)
  (type-case BCFAE expr
    [num (n) (vxs (numV n) store)]
    [add (l r)
         (type-case ValuexStore (interp l env store)
           [vxs (l-value l-store)
                (type-case ValuexStore (interp r env l-store)
                  [vxs (r-value r-store)
                       (vxs (num+ l-value r-value)
                            r-store)])])]
    [sub (l r)
         (type-case ValuexStore (interp l env store)
           [vxs (l-value l-store)
                (type-case ValuexStore (interp r env l-store)
                  [vxs (r-value r-store)
                       (vxs (num- l-value r-value)
                            r-store)])])]
    [id (v) (vxs (store-lookup (env-lookup v env) store) store)]
    [fun (bound-id bound-body)
         (vxs (closureV bound-id bound-body env) store)]
    [app (fun-expr arg-expr)
         (type-case ValuexStore (interp fun-expr env store)
           [vxs (fun-value fun-store)
                (type-case ValuexStore (interp arg-expr env fun-store)
                  [vxs (arg-value arg-store)
                       (local ([define new-loc (next-location arg-store)])
                         (begin
                           (printf "new-loc ~e \n" (closureV-env fun-value))
                         (interp (closureV-body fun-value)
                                 (aSub (closureV-param fun-value)
                                        new-loc
                                       (closureV-env fun-value))
                                 (aSto new-loc
                                       arg-value
                                       arg-store))))])])]
    [if0 (test truth falsity)
         (type-case ValuexStore (interp test env store)
           [vxs (test-value test-store)
                (if (num-zero? test-value)
                    (interp truth env test-store)
                    (interp falsity env test-store))])]
    [newbox (value-expr)
            (type-case ValuexStore (interp value-expr env store)
              [vxs (expr-value expr-store)
                   (local ([define new-loc (next-location expr-store)])
                     (vxs (boxV new-loc)
                          (aSto new-loc expr-value expr-store)))])]
    [setbox (box-expr value-expr)
            (type-case ValuexStore (interp box-expr env store)
              [vxs (box-value box-store)
                   (type-case ValuexStore (interp value-expr env box-store)
                     [vxs (value-value value-store)
                          (vxs value-value
                               (aSto (boxV-location box-value)
                                     value-value
                                     value-store))])])]
    [openbox (box-expr)
             (type-case ValuexStore (interp box-expr env store)
               [vxs (box-value box-store)
                    (vxs (store-lookup (boxV-location box-value)
                                       box-store)
                         box-store)])]
    [seqn (e1 e2)
          (type-case ValuexStore (interp e1 env store)
            [vxs (e1-value e1-store)
                 (interp e2 env e1-store)])]))
(test (interp (parse '(+ 2 3)) (mtSub) (mtSto))
      (vxs (numV 5) (mtSto)))

(test (interp (app {fun 'x (add (id 'x) (id 'x))} (num 5)) (mtSub) (mtSto))
      (vxs (numV 10) (aSto 0 (numV 5) (mtSto))))
(test (interp (parse '{with {double {fun {x} {+ x x}}} {double 5}}) (mtSub) (mtSto))
      (vxs (numV 10) (aSto 2 (numV 5) (aSto 1 (closureV 'x (add (id 'x) (id 'x)) (mtSub)) (mtSto)))))

(test (interp (parse '{with {b {newbox 0}}
                  {seqn {setbox b {+ 1 {openbox b}}}
                        {openbox b}}}) (mtSub) (mtSto))
      (vxs (numV 1) (aSto 3 (numV 1) (aSto 4 (boxV 3) (aSto 3 (numV 0) (mtSto))))))
(test (interp (parse '{with {b {newbox 0}}
                            {if0 {seqn {setbox b 11}
                                       {openbox b}}  ;; CHANGED THIS to not get bottom!
                                 {openbox b}
                                 {openbox b}}}) (mtSub) (mtSto))       
      (vxs (numV 11) (aSto 5 (numV 11) (aSto 6 (boxV 5) (aSto 5 (numV 0) (mtSto))))))
(test  (interp (parse '{with {switch {newbox 0}}
                        {with {toggle {fun {dum}
                                           {if0 {openbox switch} 
                                                {seqn {setbox switch 1}
                                                      1}
                                                {seqn {setbox switch 0}
                                                      0}}}}
                              {+ {toggle 99} {toggle 99}}}}) (mtSub) (mtSto))
       (vxs 
        (numV 1)
        (aSto
         7
         (numV 0)
         (aSto
          11
          (numV 99)
          (aSto
           7
           (numV 1)
           (aSto
            10
            (numV 99)
            (aSto
             9
             (closureV
              'dum
              (if0 (openbox (id 'switch)) (seqn (setbox (id 'switch) (num 1)) (num 1)) (seqn (setbox (id 'switch) (num 0)) (num 0)))
              (aSub 'switch 8 (mtSub)))
             (aSto 8 (boxV 7) (aSto 7 (numV 0) (mtSto))))))))))