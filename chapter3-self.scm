#lang plai
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)
       (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(with) (with (first (second sexp))
                     (parse (second (second sexp)))
                     (parse (third sexp)))])]
    [(symbol? sexp) (id sexp)]))
(test (parse '(with (x 5) (+ x x))) (with 'x (num 5) (add (id 'x) (id 'x)))) 
                   
 (define (subst i v sexp)
   (type-case WAE sexp
     [num (n) sexp]
     [add (lhs rhs)
          (add (subst i v lhs)
               (subst i v rhs))]
     [with (name named-sexp body)
           (if (symbol=? name i)
               (with name
                     (subst i v named-sexp)
                     body)
               (with name
                     (subst i v named-sexp)
                     (subst i v body)))]
          
     [id (name) 
         (if (symbol=? name i)
             v
             sexp)]))
 
  (test (subst 'x (num 5) (add (id 'x) (id 'x))) (add (num 5) (num 5)))
  
 (define (calc sexp)
   (type-case WAE sexp
     [num (n) n]
     [add (lhs rhs)
          (+ (calc lhs)
             (calc rhs))]
     [with (name named-sexp body)
           (calc (subst name 
                        (num (calc named-sexp))
                        body))]
     [id (name) (error 'calc "free identifier")]))
  (test (calc (parse '(with (x 5) (+ x x))))       
        10)
  (test (calc (parse '(with (x 5) (with (y 6) (+ x y)))))
        11)
     
     

 