#lang aoc
(require "../intcode/intcode.rkt")

(tests
 #:name "day 2"

 (def/io basic-interp
   #:let intcode (my-read)
   (send intcode interp/input-seq '())
   intcode)

 #:let in $~bl"""
1,9,10,3,
2,3,11,0,
99,
30,40,50
"""
 #:let out $~bl"""
3500,9,10,70,
2,3,11,0,
99,
30,40,50
"""
 #:>> (basic-interp in) is (my-read out)

 #:let in "1,0,0,0,99"
 #:let out "2,0,0,0,99"
 #:>> (basic-interp in) is (my-read out)

 #:let in "2,3,0,3,99"
 #:let out "2,3,0,6,99"
 #:>> (basic-interp in) is (my-read out)

 #:let in "2,4,4,5,99,0"
 #:let out "2,4,4,5,99,9801"
 #:>> (basic-interp in) is (my-read out)

 #:let in "1,1,1,4,99,5,6,0,99"
 #:let out "30,1,1,4,2,5,6,0,99"
 #:>> (basic-interp in) is (my-read out))

(def (run-task intcode a b)
  (send intcode !!! 1 a)
  (send intcode !!! 2 b)
  (send intcode interp/input-seq '())
  (send intcode !! 0))

(def-task task-1 (run-task (my-read) 12 2))

(tests
 #:>> (task-1 "1,9,10,3,2,3,11,0,99,30,40,50,99,99") is 5050)

(def magic-num (make-parameter 19690720))

;; NOTE: use hash-count instead of 100 to make the test passes
(def-task task-2
  #:let intcode (my-read)
  (for*/first ([noun (in-range 0 (send intcode size))]
               [verb (in-range 0 (send intcode size))]
               #:when (= (run-task (send intcode copy) noun verb)
                         (magic-num)))
    (+ (* 100 noun) verb)))

(tests
 #:>> (task-2 "1,0,0,0,99,19690000,720") is (+ (* 100 5) 6))
