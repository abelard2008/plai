#lang plai
(define-syntax define-cps
  (syntax-rules ()
    [(define-cps (f arg) body)
     (define-cps f (lambda (arg) body))]
    [(define-cps v val)
     (define v ((cps val) (lambda (x) x)))]))

(define the-receiver (box 'dummy-value))
(define receiver-prompt (box 'dummy-value))

(define (web-read/k p k)
  (begin 
    (set-box! receiver-prompt p)
    (set-box! the-receiver k)
    (error 'web-read/k "run (resume) to enter number and simulate cliking Submit")))
(define (resume)
  (begin
    (display (unbox receiver-prompt))
    ((unbox the-receiver) (read))))

(define-syntax cps
  (syntax-rules (+ lambda web-read)
    [(cps (+ e1 e2))
     (lambda (k)
       ((cps e1) (lambda (l-val)
                   ((cps e2) (lambda (r-val)
                               (k (+ l-val r-val)))))))]
    [(cps (lambda (a) body))
     (lambda (k)
       (k (lambda (a dyn-k)
            ((cps body) dyn-k))))]
    [(cps (web-read prompt))
     (lambda (k)
       (web-read/k prompt k))]
    [(cps (f a))
     (lambda (k)
       ((cps f) (lambda (f-val)
                  ((cps a) (lambda (a-val)
                             (f-val a-val k))))))]
    [(cps v)
     (lambda (k) (k v))]))

(define-syntax run
  (syntax-rules ()
    [(run e) ((cps e)
              (lambda (x)
                (printf "terminating with value ~e" x)))]))

(define-cps (g x) (+ x x))
(define-cps (h f) (lambda (x) (f x)))
(define-cps (dummy x) ((h g) 10))
(run (dummy 1729))
(lambda (k)
  (web-read/k "First number:"
              (lambda (l-val)
                (web-read/k "second number:"
                            (lambda (r-val)
                              (k (+ l-val r-val)))))))