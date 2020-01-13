#lang plai
(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)])
       
(define (parse expr)
  (cond 
    [(number? expr) (num expr)]
    [(list? expr)
     (case (first expr)
     [(+) (add (parse (second expr))
               (parse (third expr)))])]))
(define (calc expr)
  (type-case AE expr
    [num (n) n]
    [add (lhs rhs)
     (+ (calc lhs)
        (calc rhs))]))
(test (calc (parse '(+ 2 3))) 5)    