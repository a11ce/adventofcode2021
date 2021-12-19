#lang racket

(define init-crabs (map string->number (string-split (read-line (open-input-file "input.txt"))
                                                     ",")))

(define avg-loc (/ (apply + init-crabs)
                   (length init-crabs)))

(define (fuel-needed-p1 crabs point)
  (apply + (map (lambda (c)
                  (abs (- c point)))
                crabs)))

; i was preparing to use gradient descent if this was too slow
; but it wasn't
(define (minimize-fuel fuel-needed crabs)
  (define (loop cur-test)
    (define cur-f (fuel-needed crabs cur-test)) 
    (define left  (fuel-needed crabs (sub1 cur-test)))
    (define riht  (fuel-needed crabs (add1 cur-test)))
    (if (and (cur-f . < . left)
             (cur-f . < . riht))
        (fuel-needed crabs cur-test)
        (loop (if (left . < . riht)
                  (sub1 cur-test)
                  (add1 cur-test)))))
  (loop (round ( / (apply max crabs) 2))))


(minimize-fuel fuel-needed-p1 init-crabs)

; this is also fast enough lol
#|
(apply min (map (lambda (p) (fuel-needed-p1 init-crabs p))
                (sequence->list (in-range (apply max init-crabs)))))
|#

(define (fuel-needed-p2 crabs point)
  (apply + (map (lambda (c)
                  (define n (abs (- c point)))
                  ; triangular numbers
                  (/ (* n (add1 n)) 2))
                crabs)))

(minimize-fuel fuel-needed-p2 init-crabs)