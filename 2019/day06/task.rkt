#lang aoc

;; type node? = string?
;; type graph? = (hashof node? node?)

;; ancestors :: graph? node? -> (listof node?)
(def (ancestors data v)
  (cond
    [(hash-ref data v #f) => (λ (u) (cons v (ancestors data u)))]
    [else '()]))

;; my-read :: -> graph?
(def (my-read)
  (for/hash ([line (in-lines)])
    (apply values (reverse (string-split line ")")))))

(def-task task-1
  #:let data (my-read)
  (for/sum ([key (in-hash-keys data)])
    (length (ancestors data key))))

(tests
 #:in $~nl"""
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
"""
 #:on task-1 is 42)

(def-task task-2
  #:let data (my-read)
  #:let left (ancestors data "YOU")
  #:let right (ancestors data "SAN")
  (+ (length left)
     (length right)
     (- (* 2 (add1 (set-count (set-intersect left right)))))))

(tests
 #:in $~nl"""
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
"""
 #:on task-2 is 4)
