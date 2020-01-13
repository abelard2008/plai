#lang plai

(define the-receiver (box 'dummy-value))
(define receiver-prompt (box 'dummy-value))

(define (web-display n)
  (printf "Web output: ~a ~n" n))

(define (web-read/k p k)
  (begin 
    (set-box! receiver-prompt p)
    (set-box! the-receiver k)
    (error 'web-read/k "run (resume) to enter number and simulate cliking Submit")))

(define (resume)
  (begin
    (display (unbox receiver-prompt))
    ((unbox the-receiver) (read))))

(define (web-read/r prompt receiver)
  (begin
    (display prompt)
    (receiver (read))))

(define (get-form-field user-input val)
    (cond
      ((symbol=? val 'n1) (second (first user-input)))
      ((symbol=? val 'n2) (second (second user-input)))))


(define (f2 user-input)
  (local ([define v1 (get-form-field user-input 'n1)]
          [define v2 (get-form-field user-input 'n2)])
    (begin 
      (web-display
       (+ v1
          v2)))))

(define (web-read/r/fields scndPrompt second user-input names)
  (begin
    (display scndPrompt)
    (second (list (list (first names) user-input) (list 'n2 (read))))))

(define (f1 user-input)
  (web-read/r/fields "Second number:"
                     f2
                     user-input
                     (list 'n1)))

(web-read/r "first number:"
            f1)
            
#|
(require xml
         net/url
         mzlib/thread
         mzlib/etc)

;;;; We can then access the app at: http://<IP ADDRESS>:8080/h 
;;;;                               or http://<IP ADDRESS>:8080/g 

;; An args is
;;  (listof (cons symbol string))

;; dispatch-table : (listof (cons string ((listof string) args -> )))
(define dispatch-table null)

;; add-handler : string ((listof string) args -> ) -> void
;;  Adds a handler to the dispatch table. The handler
;;  is called for a request that matches the given name.
(define (add-handler name handler)
  (set! dispatch-table (cons (cons name handler)
                             dispatch-table)))

;; serve:  ->
;;  Runs a web server to handle GET requests through
;;  dispatch-table
(define (serve [port-number 8080])
  ;; The `run-server' function is about 55 lines in
  ;;   plt/collects/mzlib/thread.ss
  ;; It starts a TCP listener, farms each accepted
  ;; connection to a new thread, and terminates the
  ;; thread if it runs too long.
  (run-server
   ;; Port number:
   port-number
   ;; Handler:
   (lambda (in out)
     (let ([m (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+\r"
                            (read-line in))])         
       (when m
         ;; Discard rest of request header (up to blank line):
         (regexp-match #rx#"(?-s:^\r$)" in)
         ;; Dispatch
         (return-page (dispatch (cadr m)) in out))))
   ;; Timeout in msec:
   60000))

;; dispatch : string -> 
;;  Handles the given path, sending a reply back through
;;  the current output port, then jumping to (current-done-k)
(define (dispatch path)
  (let* ([url (string->url path)]
         [base (map path/param-path (url-path url))]
         [h (assoc (car base) dispatch-table)])
    (if h
        ((cdr h) base (url-query url))
        `(html (head (title "Error"))
               (body
                (font ((color "red"))
                      "Unknown page: " ,path))))))

;; return-page : xexpr -> 
;;  Doesn't return; it prints the given page to the current out
;;  port, then terminates the thread
(define (return-page xexpr in out)
  ;; Send response
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display (xexpr->string xexpr) out)
  (newline out)
  ;; Close (and flush) ports
  (close-input-port in)
  (close-output-port out))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  web-read/r
#|
(define table (make-hash))

(define (remember v)
  (let ([key (symbol->string (gensym))])
    (hash-set! table key v)
    key))

(define (lookup key)
  (hash-ref table key))
|#
(define (web-read/k prompt cont)
  `(html 
      (head (title "Web Read"))
      (body ,prompt
            (form ([action "/resume-k"] [method "get"])
                  (input ([type "text"] [name "value"]
                                        [value ""]))
                  (input ([type "submit"] [name "enter"] 
                                          [value "Enter"]))
                  ))))

(define (resume-k-handler base args)
  [begin
    (printf "resume-k-handler ~e\n" args)])
 ;; ((lookup (cdr (assoc 'key args)))
 ;;  (read (open-input-string (cdr (assoc 'value args)))))])

(add-handler "resume-k" resume-k-handler)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More servlets

(define (h-handler base args)
  (do-h identity))

(define (do-h cont)
  [begin
    (printf "~e\n" cont)
  (web-read/k "First number"
              (lambda (v1)
                (web-read/k "Second number"
                            (lambda (v2)
                              (identity (number->string (+ v1 v2)))))))])

(add-handler "h" h-handler)

(define receiver #f)
(define prompt #f)
(define (web-display n)
  (printf "Web output: ~a ~n" n))
(define (web-read/k p k)
  (begin 
    (set! prompt p)
    (set! receiver k)
    (error 'web-read/k "Run (resume)")))
(define (resume)
  (begin (display prompt) (receiver (read))))
#;(web-read/k "first number: "
              (lambda (v1)
                (web-read/k "second number: "
                            (lambda (v2)
                              (web-display
                               (+ v1 v2))))))
|#  