#lang racket

(define commands (file->list "input.txt"
                             (lambda (port)
                               (define first-read (read port))
                               (if (eof-object? first-read)
                                   eof
                                   (list first-read (read port))))))

(define (part1 lst)
  (define (loop lst hor dep)
    (if (empty? lst)
        (* hor dep)
        (match (first lst)
          [(list 'forward x) (loop (rest lst) (+ hor x) dep)]
          [(list 'down x)    (loop (rest lst) hor       (+ dep x))]
          [(list 'up x)      (loop (rest lst) hor       (- dep x))])))
  (loop lst 0 0))
      

(part1 commands)


(define (part2 lst)
  (define (loop lst hor dep aim)
    (if (empty? lst)
        (* hor dep)
        (match (first lst)
          [(list 'forward x) (loop (rest lst) (+ hor x) (+ dep (* aim x)) aim)]
          [(list 'down x)    (loop (rest lst) hor       dep               (+ aim x))]
          [(list 'up x)      (loop (rest lst) hor       dep               (- aim x))])))
  (loop lst 0 0 0))

(part2 commands)