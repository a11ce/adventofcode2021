#lang racket

;(require vector)

(provide make-vector2 
         vector2-ref
         vector2-set!
         list*->vector2
         (except-out (struct-out vector2)
                     vector2))


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
         (display (vector2-row-list vec2 idy) port)
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
  (+
   (* (vector2-width vec)
      (second pos))
   (first pos)))

(define (check-pos vec pos)
  (and ((first pos) . < . (vector2-width vec))
       ((second pos) . < . (vector2-height vec))
       ((first pos) . >= . 0)
       ((second pos) . >= . 0)))
    
            
(define (vector2-ref vec pos [default unset-sym])
  (if (check-pos vec pos)
      (vector-ref
       (vector2-pri-vec vec)
       (convert-pos vec pos))
      (if (equal? default unset-sym)
          (raise-user-error "ref position out of range:" pos)
          default)))

(define (vector2-set! vec pos v)
  (unless (check-pos vec pos)
    (raise-user-error "set! position out of range:" pos))
  (vector-set!
   (vector2-pri-vec vec)
   (convert-pos vec pos)
   v)
  vec)

(define (vector2-row-list vec idy)
  (vector->list (vector-copy (vector2-pri-vec vec)
                             (convert-pos vec (list
                                               0 idy))
                             (convert-pos vec (list
                                               0 (add1 idy))))))

(define (list*->vector2 lst)
  (define width (length (first lst)))
  (define height (length lst))
  (define vec (make-vector2 (list width height)))
  
  (for* ([idy (in-range height)]
         [idx (in-range width)])
    (define v (list-ref (list-ref lst idy) idx))
    (vector2-set! vec
                  (list idx idy)
                  v)

    )
  vec)