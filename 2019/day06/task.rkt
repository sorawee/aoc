#lang racket

(require "../../aoc.rkt")

(define (search data v)
  (cond
    [(hash-ref data v #f) => (λ (u) (cons v (search data u)))]
    [else '()]))

(define (read-all)
  (for/hash ([line (in-lines)])
    (apply values (reverse (string-split line ")")))))

(define-task task-1
  (define data (read-all))
  (for/sum ([key (in-hash-keys data)]) (length (search data key))))

(tests
 #:>> (task-1 #<<EOF
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
EOF
              ) is 42)

(define-task task-2
  (define data (read-all))
  (define left (search data "YOU"))
  (define right (search data "SAN"))
  (+ (length left)
     (length right)
     (- (* 2 (add1 (set-count (set-intersect left right)))))))

(tests
 #:>> (task-2 #<<EOF
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN
EOF
              ) is 4)
