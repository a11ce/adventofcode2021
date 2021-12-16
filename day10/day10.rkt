#lang racket

(define lines (file->list "input.txt"
                          (lambda (port)
                            (define line (read-line port))
                            (if (eof-object? line)
                                eof
                                (string->list line)))))

(define matching-chars
  '((#\( #\))
    (#\[ #\])
    (#\{ #\})
    (#\< #\>)))

(define char-points
  '((#\) 3)
    (#\] 57)
    (#\} 1197)
    (#\> 25137)))

(define (lookup v d)
  (second (assoc v d)))

(define (bad-char line [stack '()])
  (if (empty? line)
      0
      (case (first line)
        [(#\( #\[ #\{ #\<)
         (bad-char (rest line)
                   (cons (first line) stack))]
        [else
         (if (equal? (first line)
                     (lookup (first stack) matching-chars))
             (bad-char (rest line) (rest stack))
             (lookup (first line) char-points))])))

(define (part1 lines)
  (apply + (map
            bad-char
            lines)))

(part1 lines)

(define (completion-string line [stack '()])
  (if (empty? line)
      stack
      (case (first line)
        [(#\( #\[ #\{ #\<)
         (completion-string
          (rest line) (cons (first line) stack))]
        [else
         (if (equal? (first line)
                     (lookup (first stack) matching-chars))
             (completion-string (rest line) (rest stack))
             '())])))

(define completion-points
  '((#\( 1)
    (#\[ 2)
    (#\{ 3)
    (#\< 4)))

(define (completion-score c-str [acc 0])
  (if (empty? c-str)
      acc
      (completion-score
       (rest c-str)
       (+ (lookup (first c-str) completion-points)
          (* 5 acc)))))

(define (part2 lines)
  (define scores
    (sort (filter (λ (s) (not (zero? s)))
                  (map
                   (λ (line)
                     (completion-score
                      (completion-string line)))
                   lines))
          <))
  (list-ref scores (quotient (length scores) 2)))

(part2 lines)