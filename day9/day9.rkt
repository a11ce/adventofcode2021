#lang racket

(require "../vector2.rkt")

(define grid (list*->vector2
              (file->list "input.txt"
                          (lambda (port)
                            (define line (read-line port))
                            (if (eof-object? line)
                                eof
                                (map (compose string->number string)
                                     (string->list line)))))))

(define cardinal-dirs
  '((1 0)
    (-1 0)
    (0 1)
    (0 -1)))

(define (low-points grid)
  (filter
   (lambda (v) (not (false? v)))
   (for*/list
       ([idx (in-range (vector2-width grid))]
        [idy (in-range (vector2-height grid))])
     (if (andmap (lambda (d)
                   (< (vector2-ref grid (list idx idy))
                      (vector2-ref grid (map + (list idx idy) d) +inf.0)))
                 cardinal-dirs)            
                
         (list idx idy)
         #f))))

(define (part1 grid)
  (apply +
         (map (lambda (p)
                (add1 (vector2-ref grid p)))
              (low-points grid))))

(part1 grid)

; flood fill
(define (basin grid p)
  (define seen '())
  (define (basin-aux grid p)
    (if (or (= 9 (vector2-ref grid p 9))
            (member p seen))
        seen
        (begin
          (set! seen (cons p seen))
          (apply append
                 (map (lambda (d)
                        (basin-aux grid
                                   (map + p d)))
                      cardinal-dirs)))))
  (basin-aux grid p)
  
  (if (or (= 9 (vector2-ref grid p 9))
          (member p seen))
      seen
      (remove-duplicates
       (apply append
              (map (lambda (d)
                     (basin grid (map + p d) (cons p seen)))
                   cardinal-dirs)))))


(define (part2 grid)
  (apply *
         (take
          (sort (map (lambda (p)
                       (length (basin grid p)))
                     (low-points grid))
                >)
          3)))

(part2 grid)