#lang racket

(require "../../aoc.rkt")

;; cell-config? = (list id:number? dist:number?)
;; location? = (hashof complex? (listof cell-config?))

;; draw :: location? (listof string?) id:number? -> any
(define (draw locs xs id)
  (for/fold ([pos 0] [t 0]) ([item (in-list xs)])
    (match-define (pregexp "(.)(.+)" (list _ dir (app string->number len)))
      item)
    (define dir-vec (match dir ["R" 1+0i] ["L" -1-0i] ["U" 0+1i] ["D" 0-1i]))
    (for/fold ([pos pos] [t t]) ([i (in-range len)])
      (hash-update! locs (+ pos dir-vec) (curry cons (list id (add1 t))) '())
      (values (+ pos dir-vec) (add1 t)))))

;; calc-min :: location? (complex? (listof cell-config?) -> any/c) -> number?
(define (calc-min locs proc)
  (inexact->exact
   (for*/fold ([min-val +inf.0])
              ([(key val) (in-hash locs)]
               [visited (in-value (remove-duplicates (map first val)))]
               #:when (> (length visited) 1))
     (min min-val (proc key val)))))

;; run-task :: (complex? (listof cell-config?) -> any/c) -> number?
(define (run-task proc)
  (define locs (make-hash))
  (for ([line (in-lines)] [i (in-naturals)])
    (draw locs (string-split line ",") i))
  (calc-min locs proc))

(define-task task-1
  (run-task (match-lambda**
             [(p _) (+ (abs (real-part p)) (abs (imag-part p)))])))

(define-task task-2
  (run-task (match-lambda** [(_ (list (list _ dist) ...)) (apply + dist)])))

(tests
 #:with (~ "R8,U5,L5,D3"
           "U7,R6,D4,L4")
 #:on task-1 is 6
 #:on task-2 is 30

 #:with (~ "R75,D30,R83,U83,L12,D49,R71,U7,L72"
           "U62,R66,U55,R34,D71,R55,D58,R83")
 #:on task-1 is 159
 #:on task-2 is 610

 #:with (~ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
           "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
 #:on task-1 is 135
 #:on task-2 is 410)
