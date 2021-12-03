#lang racket

(define report (file->list "input.txt"
                           (lambda (port)
                             (define line (read-line port))
                             (if (eof-object? line)
                                 eof
                                 (map (lambda (c) (string->number (string c)))
                                      (string->list line))))))

(define (bin-list->dec lst)
  (define (loop lst idx)
    (if (empty? lst)
        0
        (+ (loop (rest lst) (+ idx 1))
           (* (expt 2 idx) (first lst)))))
  (loop (reverse lst) 0))
                                       
(define (common-bits vals which-way)
  (map (lambda (v) (if (v . (if (equal? which-way 'most) >= <) . 0)
                       1 0))
       vals))

(define (freq-sums lst)
  (define (loop lst acc-lst)
    (if (empty? lst)
        acc-lst
        (loop (rest lst) (map (lambda (acc new)
                                (+ acc (if (= 0 new)
                                           -1 1)))
                              acc-lst
                              (first lst)))))
  (loop lst (build-list (length (first lst)) (lambda (n) 0))))
  
(define (part1 lst)
  (define freqs (freq-sums lst))
  (apply * (map bin-list->dec (list (common-bits freqs 'most)
                                    (common-bits freqs 'least)))))

(part1 report)

(define (find-rating lst which-one)
  (define (loop lst idx)
    (define search-bits (common-bits (freq-sums lst) which-one))
    (define remaining (filter (lambda (v)
                                (= (list-ref v idx)
                                   (list-ref search-bits idx)))
                              lst))
    (if (= 1 (length remaining))
        (first remaining)
        (loop remaining (add1 idx))))
  (loop lst 0))

(define (part2 lst)
  (define freqs (freq-sums lst))
  (apply * (map bin-list->dec
                (list 
                 (find-rating lst 'most)
                 (find-rating lst 'least)))))

(part2 report)