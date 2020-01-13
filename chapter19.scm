#lang plai
(define route-producer
  (local ([define resume (box false)])
    (lambda (real-send)
      (local ([define send (lambda (value-to-send)
                             (let/cc k
                               (begin
                                 (set-box! resume k)
                                 (real-send value-to-send))))])
        (if (unbox resume)
            ((unbox resume) 'dummy)
            (begin
              (send 'providence)
              (send 'houston)
              (send 'bangalore)))))))

(define route-producer1
  (local ([define resume (box false)])
            (lambda (real-send)
              (local ([define send-to (box real-send)]
                      [define send (lambda (value-to-send)
                                     (set-box! send-to 
                                              (let/cc k
                                                (begin
                                                  (set-box! resume k)
                                                  ((unbox send-to) value-to-send)))))])
                (if (unbox resume)
                   ((unbox resume) real-send)
                   (begin
                     (send 'providence)
                     (send 'houston)
                     (send 'bangalore)))))))

(define (get producer)
  (let/cc k (producer k))
  )

(list (get route-producer1)
     (get route-producer1)
     (get route-producer1))

;(list (let/cc k (route-producer1 k)))
     ;(let/cc k (route-producer1 k)))

                        