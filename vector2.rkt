#lang racket

;(require vector)

(provide make-vector2
         vector2-ref
         vector2-set!)


(struct vector2
  (pri-vec
   width
   height)
  #:methods gen:custom-write
  [(define (write-proc vec2 port mode)
     (let [(pri-vec (vector2-pri-vec vec2))
           (width (vector2-width vec2))
           (height (vector2-height vec2))]
       (display "#(" port)
       (for ([idy (in-range height)])
         (display (vector-copy
                   pri-vec
                   (convert-pos vec2
                                (list 0 idy))
                   (convert-pos vec2
                                (list (sub1 width) idy)))
                  port)
         (unless (= idy (sub1 height))
           (display "\n  " port)))
       (display ")" port)))])

; thank you sorawee
(define unset-sym (gensym))

(define (make-vector2 dims [v 0])
  (vector2 (make-vector
            (* (first dims) (second dims)) v)
           (first dims)
           (second dims)))

(define (convert-pos vec pos)
  ( + (* (vector2-width vec)
         (first pos))
      (second pos)))

(define (check-pos vec pos)
  (and ((first pos) . < . (vector2-width vec))
      ((second pos) . < . (vector2-height vec))))
    
            
(define (vector2-ref vec pos [default unset-sym])
  (unless (check-pos vec pos)
    (if (equal? default unset-sym)
        (raise-user-error "ref position out of range:" pos)
        default))
  (vector-ref
   (vector2-pri-vec vec)
   (convert-pos vec pos)))

(define (vector2-set! vec pos v)
  (vector-set!
   (vector2-pri-vec vec)
   (convert-pos vec pos)
   v)
  vec)
