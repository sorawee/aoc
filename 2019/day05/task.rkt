#lang aoc

(require "../intcode/intcode.rkt")

(module+ test
  (define lifted-basic-interp (lift-input basic-interp)))

(tests
 #:let in "1002,4,3,4,33"
 #:let out "1002,4,3,4,99"
 #:>> (lifted-basic-interp in) is (parse-string out)

 #:let in "1101,100,-1,4,0"
 #:let out "1101,100,-1,4,99"
 #:>> (lifted-basic-interp in) is (parse-string out))

(define (basic-interp/io vec in)
  (last (first (interp-to-complete vec in))))

(tests
 #:let in "1002,4,3,4,33"
 #:let out "1002,4,3,4,99"
 #:>> (lifted-basic-interp in) is (parse-string out)

 #:let in "1101,100,-1,4,0"
 #:let out "1101,100,-1,4,99"
 #:>> (lifted-basic-interp in) is (parse-string out))

(define-task task-1
  (basic-interp/io (the-input) 1))

(tests
 #:let lifted-basic-interp/io (lift-input basic-interp/io)

 #:>> (lifted-basic-interp/io "3,9,8,9,10,9,4,9,99,-1,8" 8) is 1
 #:>> (lifted-basic-interp/io "3,9,8,9,10,9,4,9,99,-1,8" 7) is 0

 #:>> (lifted-basic-interp/io "3,9,7,9,10,9,4,9,99,-1,8" 8) is 0
 #:>> (lifted-basic-interp/io "3,9,7,9,10,9,4,9,99,-1,8" 7) is 1

 #:>> (lifted-basic-interp/io "3,3,1108,-1,8,3,4,3,99" 8) is 1
 #:>> (lifted-basic-interp/io "3,3,1108,-1,8,3,4,3,99" 7) is 0

 #:>> (lifted-basic-interp/io "3,3,1107,-1,8,3,4,3,99" 8) is 0
 #:>> (lifted-basic-interp/io "3,3,1107,-1,8,3,4,3,99" 7) is 1

 #:>> (lifted-basic-interp/io "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 0) is
 0
 #:>> (lifted-basic-interp/io "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 2) is
 1

 #:>> (lifted-basic-interp/io "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 0) is 0
 #:>> (lifted-basic-interp/io "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 2) is 1

 #:let in $~bl"""
3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
"""

 #:>> (lifted-basic-interp/io in 5) is 999
 #:>> (lifted-basic-interp/io in 8) is 1000
 #:>> (lifted-basic-interp/io in 9) is 1001)

(define-task task-2
  (basic-interp/io (the-input) 5))
