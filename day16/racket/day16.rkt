#lang racket
(require brag/support)
(require "bits.rkt")

(define (hex->bin-string s)
  (string-join
   (map (λ (c)
          (~a (number->string (string->number (string c) 16 ) 2)
              #:align 'right
              #:left-pad-string "0"
              #:min-width 4))
        (string->list s))
   ""))

(define bit-string (hex->bin-string (read-line (open-input-file "input.txt"))))

(define bits-prog
  (syntax->datum (parse
                  (map (λ (bit pos)
                         (token
                          (string bit)
                          (string bit)
                          ; this is good for debugging the parser
                          #:column pos))
                       (string->list bit-string)
                       (stream->list (in-range (string-length bit-string)))))))

(define (bin-list->number lst)
  (define (loop lst idx)
    (if (empty? lst)
        0
        (+ (loop (rest lst) (add1 idx))
           (* (expt 2 idx)
              (string->number (cadadr (first lst)))))))
  (loop (reverse lst) 0))

(define (part1 prog)
  (apply +
         (map (λ (p)
                (case (first p)
                  [(Packet)
                   (bin-list->number (rest (second p)))]
                  [else 0]))
              (rest prog))))

(part1 bits-prog)
