#lang racket

(define (read-moves-and-boards path)
  (let ([in (open-input-file path)])
    (list (map string->number (string-split (read-line in) ","))
          (drop-right
           (port->list (lambda (p)
                         (if (eof-object? (peek-byte in))
                             eof
                             (for/list
                                 ([idx (in-range 5)])
                               (for/list
                                   ([idy (in-range 5)])
                                 (read p)))))
                       in) 1))))


(define-values (moves boards) (apply values (read-moves-and-boards "input.txt")))

(define (board-won board)
  (define (aux board)
    (ormap (lambda (row)
             (andmap (lambda (v) (equal? v 'called))
                     row))
           board))
  (or (aux board)
      (aux (apply map list board))))

(define (score-board board)
  (apply +
         (map (lambda (row) (apply + (filter number? row)))
              board)))


(define (do-move move boards)
  (map (lambda (board)
         (map (lambda (row)
                (map 
                 (lambda (v)
                   (if (equal? move v)
                       'called v))
                 row))
              board))
       boards))

(define (part1 moves boards)
  (define (loop moves boards)
    (unless (empty? moves)
      (define new-boards (do-move (first moves) boards))
      (define pos-winner (filter board-won new-boards))
      (if (empty? pos-winner)
          (loop (rest moves) new-boards)
          (* (first moves) (score-board (first pos-winner))))))
  (loop moves boards))


(part1 moves boards)

(define (part2 moves boards)
  (define (loop moves boards)
    (unless (empty? moves)
      (define new-boards (do-move (first moves) boards))
      (define non-wins (filter (lambda (b) (not (board-won b))) new-boards))
      (if (equal? 1 (length non-wins))
          (part1 (rest moves) non-wins)
         ; (* (first moves) (score-board (first non-wins)))
          (loop (rest moves) non-wins))))
  (loop moves boards))

(part2 moves boards)