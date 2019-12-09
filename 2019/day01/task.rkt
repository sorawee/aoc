#lang aoc

;; calc-module :: number? -> number?
(define (calc-module mass)
  (- (quotient mass 3) 2))

(tests
 #:>> (calc-module 12) is 2)

;; calc-module* :: number? -> number?
(define (calc-module* mass)
  (define mass-need (calc-module mass))
  (cond
    [(<= mass-need 0) 0]
    [else (+ mass-need (calc-module* mass-need))]))

(tests
 #:fn calc-module
 #:on 14 is 2
 #:on 1969 is 654
 #:on 100756 is 33583)

(tests
 #:fn calc-module*
 #:on 14 is 2
 #:on 1969 is 966
 #:on 100756 is 50346)

(define-task task-1
  (for/sum ([mass (in-port)])
    (calc-module mass)))

(define-task task-2
  (for/sum ([mass (in-port)])
    (calc-module* mass)))

(tests
 #:in $~nl"""
14
1969
100756
"""
 #:on task-1 is (+ 2 654 33583)
 #:on task-2 is (+ 2 966 50346))
