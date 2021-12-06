#lang racket

(define init-fish
  (map string->number (string-split (read-line (open-input-file "input.txt"))
                                    ",")))


(define (part1 fish count)
  (define (loop fish acc)
    (if (= acc count)
        (length fish)
        (let* ([num-new (length (filter zero? fish))]
               [new-fish (append
                          (map (lambda (f)
                                 (if (zero? f)
                                     6
                                     (sub1 f)))
                               fish)
                          (make-list num-new 8))])
          (loop new-fish (add1 acc)))))
  (loop fish 0))

(part1 init-fish 80)

(define (vector-add! vec pos v)
  (vector-set! vec pos (+ v (vector-ref vec pos))))

(define (init-fish->vec fish)
  (define fish-counts (make-vector 9 0))
  (for ([fish-n fish])
    (vector-add! fish-counts fish-n 1))
  fish-counts)

(define (part2 fish days)
  (define fish-counts (init-fish->vec fish))
  (for ([day-idx (in-range days)])
    (define num-new
      (vector-ref fish-counts 0))
    (for ([fish-n (in-range 8)])
      (vector-set! fish-counts fish-n (vector-ref fish-counts (+ 1 fish-n))))
    (vector-add! fish-counts 6 num-new)
    (vector-set! fish-counts 8 num-new))
    
  (apply + (vector->list fish-counts)))


(part2 init-fish 256)