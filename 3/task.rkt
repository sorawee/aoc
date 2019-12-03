#lang racket

(require "../aoc.rkt")

;; posn? = (list number? number?)
;; cell-config? = (list id:number? dist:number?)
;; location? = (hashof posn? (listof cell-config?))

;; draw :: location? (listof string?) id:number? -> any
(define (draw locations xs name)
  (for/fold ([x 0] [y 0] [t 0]) ([item (in-list xs)])
    (match-define (pregexp "(.)(.+)" (list _ dir (app string->number len)))
      item)
    (define (update-x i)
      (match dir
        ["R" i]
        ["L" (- i)]
        ["U" 0]
        ["D" 0]))
    (define (update-y i)
      (match dir
        ["R" 0]
        ["L" 0]
        ["U" i]
        ["D" (- i)]))
    (for ([i (in-range 1 (add1 len))])
      (hash-update! locations (list (+ x (update-x i)) (+ y (update-y i)))
                    (curry cons (list name (+ t i))) '()))
    (values (+ x (update-x len))
            (+ y (update-y len))
            (+ t len))))

;; calc-min :: location? (posn? (listof cell-config?) -> any/c) -> number?
(define (calc-min locations proc)
  (inexact->exact
   (for*/fold ([min-val +inf.0])
              ([(key val) (in-hash locations)]
               [visited (in-value (remove-duplicates (map first val)))]
               #:when (> (length visited) 1))
     (min min-val (proc key val)))))

;; parse+draw :: -> location?
(define (parse+draw)
  (define locations (make-hash))
  (for ([line (in-lines)] [i (in-naturals)])
    (draw locations (string-split line ",") i))
  locations)

(tests
 (task-1 "R8,U5,L5,D3\nU7,R6,D4,L4") ==> 6
 (task-1 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83") ==> 159
 (task-1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7") ==> 135)

(define-task task-1
  (calc-min (parse+draw)
            (λ (key val)
              (match-define (list x y) key)
              (+ (abs x) (abs y)))))

(tests
 (task-2 "R8,U5,L5,D3\nU7,R6,D4,L4") ==> 30
 (task-2 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83") ==> 610
 (task-2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7") ==> 410)

(define-task task-2
  (calc-min (parse+draw) (λ (key val) (apply + (map second val)))))
