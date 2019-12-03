#lang racket

(require "../aoc.rkt")

;; calc-module :: number? -> number?
(define (calc-module mass)
  (- (quotient mass 3) 2))

(tests
 (calc-module 12) ==> 2
 (calc-module 14) ==> 2
 (calc-module 1969) ==> 654
 (calc-module 100756) ==> 33583)

;; calc-module* :: number? -> number?
(define (calc-module* mass)
  (define mass-need (calc-module mass))
  (cond
    [(<= mass-need 0) 0]
    [else (+ mass-need (calc-module* mass-need))]))

(tests
 (calc-module* 14) ==> 2
 (calc-module* 1969) ==> 966
 (calc-module* 100756) ==> 50346)

(define-task task-1
  (for/sum ([mass (in-port)])
    (calc-module mass)))

(tests
 (task-1 "14\n1969\n100756") ==> (+ 2 654 33583))

(define-task task-2
  (for/sum ([mass (in-port)])
    (calc-module* mass)))

(tests
 (task-2 "14\n1969\n100756") ==> (+ 2 966 50346))
