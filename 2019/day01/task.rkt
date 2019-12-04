#lang racket

(require "../../aoc.rkt")

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
 #:with 14
 #:on calc-module is 2
 #:on calc-module* is 2

 #:with 1969
 #:on calc-module is 654
 #:on calc-module* is 966

 #:with 100756
 #:on calc-module is 33583
 #:on calc-module* is 50346)

(define-task task-1
  (for/sum ([mass (in-port)])
    (calc-module mass)))


(define-task task-2
  (for/sum ([mass (in-port)])
    (calc-module* mass)))

(tests
 #:with "14\n1969\n100756"
 #:on task-1 is (+ 2 654 33583)
 #:on task-2 is (+ 2 966 50346))
