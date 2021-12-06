#lang racket

(require math/array)

(define lines (file->list "input.txt"
                          (lambda (port)
                            (define line (read-line port))
                            (if (eof-object? line)
                                eof
                                (map (lambda (p)
                                       (map (lambda (c)
                                              (string->number c))
                                            (string-split p ",")))
                                     (string-split line " -> "))))))


(define (grid-size ls)
  (+ 1 (apply max (map
                   (lambda (l)
                     (apply max
                            (map (lambda (p) (apply max p)) l)))
                   ls))))


(define (make-board size)
  (vector*->array
   (make-vector
    size
    (make-vector size 0))
   number?))



(define (array-add! arr pos v)
  (array-set! arr (list->vector pos)
              (+ v (array-ref arr (list->vector pos)))))

(define (do-line! l board)
  ;(displayln l)
  (let ([x1 (first (first l))]
        [y1 (second (first l))]
        [x2 (first (second l))]
        [y2 (second (second l))])

    ; TODO there has to be a better way to do this
    (define x-seq (if (= x1 x2)
                      (make-list (add1 (abs (- y1 y2)))
                                 x1)
                      (in-inclusive-range x1 x2
                                          (if (> x1 x2)
                                              -1
                                              1))))
    (define y-seq (if (= y1 y2)
                      (make-list (add1 (abs (- x1 x2)))
                                 y1)
                      (in-inclusive-range y1 y2
                                          (if (> y1 y2)
                                              -1
                                              1))))

    (for [(idx x-seq)
          (idy y-seq)]
  
      (array-add! board (list idx idy) 1))))

(define (hor-vert? line)
  (or (= (first (first line))
         (first (second line)))
      (= (second (first line))
         (second (second line)))))


(define (count-overlaps lines)
  (define board (make-board (grid-size lines)))
  (for ([line lines])
    ; (displayln line)
    (do-line! line board))
  (apply +
         (map
          (lambda (row)
            (length (filter (lambda (v) (>= v 2))
                            row)))
          (array->list* board))))

(count-overlaps (filter hor-vert? lines))
(count-overlaps lines)