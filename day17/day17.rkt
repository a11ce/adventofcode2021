#lang racket

(struct cord (x y))

; easier to just hardcode input, this is the example given
(define input (cord '(20 30)
                    '(-10 -5)))

(define (in-range? pos x-range y-range)
  (and (<= (first x-range) (cord-x pos) (second x-range))
       (<= (first y-range) (cord-y pos) (second y-range))))

(define (too-far? pos x-range y-range)
  (or ((cord-x pos) . > . (second x-range))
      ((cord-y pos) . < . (first y-range))))

(define (check-launch vel x-range y-range [maxHeight 0] [pos (cord 0 0)])
  (let ([new-xp (+ (cord-x pos) (cord-x vel))]
        [new-yp (+ (cord-y pos) (cord-y vel))]
        [new-xv (cond [(> (cord-x vel) 0) (sub1 (cord-x vel))]
                      [(< (cord-x vel) 0) (add1 (cord-x vel))]
                      [else (cord-x vel)])]
        [new-yv (sub1 (cord-y vel))])
    (cond
      [(in-range? (cord new-xp new-yp)
                  x-range y-range)
       maxHeight]
      [(too-far? (cord new-xp new-yp)
                 x-range y-range) #f]
      [else (check-launch (cord new-xv new-yv)
                          x-range y-range
                          (max maxHeight new-yp)
                          (cord new-xp new-yp))])))


(define (part1 x-range y-range)
  (apply max (for*/list ([idx (in-range (second x-range))]
                         [idy (in-range 0 300)])
               (let ([res (check-launch (cord idx idy) x-range y-range)])
                 (if res res 0)))))

(part1 (cord-x input) (cord-y input))

(define (part2 x-range y-range)
  (define x-far (apply max (map abs x-range)))
  (define y-far (apply max (map abs y-range)))
  
  (length
   (filter (λ (v) (not (false? v)))
           (for*/list
               ([idx (in-range 0 (add1 x-far))]
                [idy (in-range (sub1 (* -1 y-far)) (add1 y-far))])
             (check-launch  (cord idx idy) x-range y-range)))))

(part2 (cord-x input) (cord-y input))