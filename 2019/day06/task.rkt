#lang racket

(require "../../aoc.rkt")

;; type node? = string?
;; type graph? = (hashof node? node?)

;; ancestors :: graph? node? -> (listof node?)
(define (ancestors data v)
  (cond
    [(hash-ref data v #f) => (λ (u) (cons v (ancestors data u)))]
    [else '()]))

;; read-all :: -> graph?
(define (read-all)
  (for/hash ([line (in-lines)])
    (apply values (reverse (string-split line ")")))))

(define-task task-1
  (define data (read-all))
  (for/sum ([key (in-hash-keys data)]) (length (ancestors data key))))

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
  (define left (ancestors data "YOU"))
  (define right (ancestors data "SAN"))
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
