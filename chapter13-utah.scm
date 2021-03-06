#lang plai

(define-type BCFAE
  [num (n number?)]
  [add (lhs BCFAE?)
       (rhs BCFAE?)]
  [sub (lhs BCFAE?)
       (rhs BCFAE?)]
  [id (name symbol?)]
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
            (ds Env?)]
  [boxV (address integer?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value BCFAE-Value?)
        (rest Env?)])

(define-type Store
  [mtSto]
  [aSto (address integer?)
        (value BCFAE-Value?)
        (rest Store?)])

(define-type Value*Store
  [v*s (value BCFAE-Value?)
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

;; ----------------------------------------

;; interp : BCFAE DefrdSub Store -> Value*Store
(define (interp a-fae ds st)
  (type-case BCFAE a-fae
    [num (n) (v*s (numV n) st)]
    [add (l r) (interp-two l r ds st
                           (lambda (v1 v2 st)
                             (v*s (num+ v1 v2) st)))]
    [sub (l r) (interp-two l r ds st
                           (lambda (v1 v2 st)
                             (v*s (num- v1 v2) st)))]
    [id (name) (v*s (lookup name ds) st)]
    [fun (param body-expr)
         (v*s (closureV param body-expr ds) st)]
    [app (fun-expr arg-expr)
         (interp-two fun-expr arg-expr ds st
                     (lambda (fun-val arg-val st)
                       (begin
                         (printf "arg-val ~e \n" arg-val)
                       (interp (closureV-body fun-val)
                               (aSub (closureV-param fun-val)
                                     arg-val
                                     (closureV-ds fun-val))
                               st))))]
    [newbox (expr)
            (type-case Value*Store (interp expr ds st)
              [v*s (val st)
                   (local [(define a (malloc st))]
                     (v*s (boxV a)
                          (aSto a val st)))])]
    [setbox (bx-expr val-expr)
            (interp-two bx-expr val-expr ds st
                        (lambda (bx-val val st3)
                          (v*s val
                               (aSto (boxV-address bx-val)
                                     val
                                     st3))))]
    [openbox (bx-expr)
             (type-case Value*Store (interp bx-expr ds st)
               [v*s (bx-val st)
                    (v*s (store-lookup (boxV-address bx-val)
                                       st)
                         st)])]
    [seqn (a b) (interp-two a b ds st
                            (lambda (v1 v2 st)
                              (v*s v2 st)))]))


;;interp-two : BCFAE BCFAE DefrdSub Store
;;                  (Value Value Store -> Value*Store)
;;             -> Value*Store
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         (type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3)
                (handle val1 val2 st3)])]))

;; num-op : (number number -> number) -> (BCFAE-Value BCFAE-Value -> BCFAE-Value)
(define (num-op op op-name x y)
  (numV (op (numV-n x) (numV-n y))))

(define (num+ x y) (num-op + '+ x y))
(define (num- x y) (num-op - '- x y))

;; malloc : Store -> integer
(define (malloc st)
  (+ 1 (max-address st)))

;; max-address : Store -> integer
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))

;; lookup : symbol DefrdSub -> BCFAE-Value
(define (lookup name ds)
  (type-case Env ds
    [mtSub () (error 'lookup "free variable")]
    [aSub (sub-name val rest-ds)
          (if (symbol=? sub-name name)
              val
              (lookup name rest-ds))]))

;; store-lookup : number Store -> BCFAE-Value
(define (store-lookup addr st)
  (type-case Store st
    [mtSto () (error 'store-lookup "unallocated")]
    [aSto (sto-addr val rest-st)
          (if (= addr sto-addr)
              val
              (store-lookup addr rest-st))]))

;; ----------------------------------------
;; Old, functional tests:

(test (interp (parse 10)
              (mtSub)
              (mtSto))
      (v*s (numV 10) (mtSto)))
(test (interp (parse '{+ 10 17})
              (mtSub)
              (mtSto))
      (v*s (numV 27) (mtSto)))
(test (interp (parse '{- 10 7})
              (mtSub)
              (mtSto))
      (v*s (numV 3) (mtSto)))
(test (interp (parse '{{fun {x} {+ x 12}}
                       {+ 1 17}})
              (mtSub) 
              (mtSto))
      (v*s (numV 30) (mtSto)))
(test (interp (parse 'x)
              (aSub 'x (numV 10) (mtSub))
              (mtSto))
      (v*s (numV 10) (mtSto)))

(test (interp (parse '{{fun {x}
                            {{fun {f}
                                  {+ {f 1}
                                     {{fun {x}
                                           {f 2}}
                                      3}}}
                             {fun {y} {+ x y}}}}
                       0})
              (mtSub)
              (mtSto))
      (v*s (numV 3) (mtSto)))

(test/exn (interp (parse 'x) (mtSub) (mtSto))
          "free variable")

;; ----------------------------------------
;; Store tests:

(test (interp (parse '{seqn 1 2})
              (mtSub)
              (mtSto))
      (v*s (numV 2) (mtSto)))

(test (interp (parse '{{fun {b} {openbox b}}
                       {newbox 10}})
              (mtSub)
              (mtSto))
      (v*s (numV 10) 
           (aSto 1 (numV 10) (mtSto))))

(test (interp (parse '{{fun {b} {openbox b}}
                       {seqn
                        {newbox 9}
                        {newbox 10}}})
              (mtSub)
              (mtSto))
      (v*s (numV 10) 
           (aSto 2 (numV 10) 
                 (aSto 1 (numV 9) (mtSto)))))

(test (interp (parse '{{{fun {b} 
                             {fun {a}
                                  {openbox b}}}
                        {newbox 9}}
                       {newbox 10}})
              (mtSub)
              (mtSto))
      (v*s (numV 9) 
           (aSto 2 (numV 10) 
                 (aSto 1 (numV 9) (mtSto)))))

(test (interp (parse '{{fun {b} {seqn
                                 {setbox b 12}
                                 {openbox b}}}
                       {newbox 10}})
              (mtSub)
              (mtSto))
      (v*s (numV 12) 
           (aSto 1
                 (numV 12)
                 (aSto 1 
                       (numV 10)
                       (mtSto)))))

(test/exn (interp (parse '{openbox x})
                  (aSub 'x (boxV 1) (mtSub))
                  (mtSto))
          "unallocated")