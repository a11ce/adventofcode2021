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
              (string->number
               (if (equal? (first (first lst)) 'bit)
                   (cadadr (first lst))
                   (second (first lst))))))))
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

;;; not very happy with this yet,,
;;; guessing ill be revisiting it for later problems though

(struct Packet (version length) #:transparent)
(struct OperatorPacket Packet (opcode length-type length-val subpackets) #:transparent)
(struct LiteralPacket  Packet (value) #:transparent)

(define opcodes '(sum mult min max #f gt lt eq))

(define (literal-val->number val)
  (bin-list->number
   (apply append (map (λ (g)
                        (rest (third g)))
                      (rest val)))))


(define (packet*-length packet type)
  (if (equal? type 'packets)
      (if (LiteralPacket? packet) 1
          (add1 (apply + (map (λ (p) (packet*-length p type))
                              (OperatorPacket-subpackets packet)))))
      (if (LiteralPacket? packet) (Packet-length packet)
          (apply + (Packet-length packet) (map (λ (p) (packet*-length p type))
                                               (OperatorPacket-subpackets packet))))))

;;;;;
;
;  "The oldest and strongest emotion of mankind is fear,
;   and the oldest and strongest kind of fear is fear of the unknown"
;                                              -  H.P. Lovecraft
(define (compile-packets packets [search-type #f] [search-count #f] [search-acc 1])
  (match (first packets)
    [(list `Packet version contents)
     (define version-num (bin-list->number (rest version)))
     (match contents
       
       [(list `OperatorPacket op-type subpacket-info)
        (define opcode (list-ref opcodes (bin-list->number (rest op-type))))
        (define length-type (if (equal? 'zero (first (second subpacket-info)))
                                'bits 'packets))
        (define length-val (bin-list->number (rest (third subpacket-info))))
        (define packet-len (+ 6 (if (equal? length-type 'bits) 16 12)))

        (define (compile-subpackets [acc 0] [rel-idx 1])
          (if (>= acc length-val) '()
              (local
                [(define next-packet (compile-packets (drop packets rel-idx)))
                 (define np-len (packet*-length next-packet length-type))]
                (cons next-packet
                      (compile-subpackets (+ acc (if (equal? length-type 'bits) np-len 1))
                                          (+ rel-idx (packet*-length next-packet 'packets)))))))
        (define subpackets (compile-subpackets))
        
        (OperatorPacket version-num packet-len opcode length-type length-val subpackets)]
       
       [(list `LiteralPacket _ literal-val)
        (define packet-len (+ 6 (* 5 (length (rest literal-val)))))
        (LiteralPacket version-num packet-len (literal-val->number literal-val)) ])]
    
    [(list `padding zero ...) #f]))
     

(define (compile-bits prog)
  (compile-packets (rest prog)))


(define (interp prog)
  (if (LiteralPacket? prog)
      (LiteralPacket-value prog)
      (local [(define subvals (map interp (OperatorPacket-subpackets prog)))]
        (case (OperatorPacket-opcode prog)
          [(sum)  (apply + subvals)]
          [(mult) (apply * subvals)]
          [(min)  (apply min subvals)]
          [(max)  (apply max subvals)] 
          [(gt)   (if (apply > subvals) 1 0)]
          [(lt)   (if (apply < subvals) 1 0)]
          [(eq)   (if (apply = subvals) 1 0)]))))

  
(interp (compile-bits bits-prog))
  
