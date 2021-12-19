#lang racket

(define (part1 lst acc)
  (if (empty? (rest lst))
      acc
      (part1 (rest lst)
             (if (> (second lst)
                    (first lst))
                 (add1 acc)
                 acc))))

(part1 (file->list "input.txt") 0)


(define (window-sum lst)
  (+ (first lst)
     (second lst)
     (third lst)))
  

(define (part2 lst acc)
  (if (empty? (cdddr lst))
      acc
      (part2 (rest lst)
             (if (> (window-sum (rest lst))
                    (window-sum  lst))
                 (add1 acc)
                 acc))))


(part2 (file->list "input.txt") 0)