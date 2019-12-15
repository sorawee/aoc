#lang aoc

(require "../intcode/intcode.rkt")

(tests
 (def/io/params (test-basic-interp [in #f])
   #:let intcode (my-read)
   (send intcode interp/input-seq (list in))
   intcode)

 #:let in "1002,4,3,4,33"
 #:let out "1002,4,3,4,99"
 #:>> (test-basic-interp #:in in) is (my-read out)

 #:let in "1101,100,-1,4,0"
 #:let out "1101,100,-1,4,99"
 #:>> (test-basic-interp #:in in) is (my-read out)

 #:let in "3,0,99"
 #:let out "123,0,99"
 #:>> (test-basic-interp #:in in 123) is (my-read out))

(def/io/params (basic-interp/io in)
  (last (send (my-read) interp/input-seq (list in))))

(tests
 #:>> (basic-interp/io #:in "4,0,99" 0) is 4
 #:>> (basic-interp/io #:in "3,0,4,0,99" 123) is 123)

(def-task task-1 (basic-interp/io 1))

(tests
 #:>> (basic-interp/io
       #:in "3,9,8,9,10,9,4,9,99,-1,8" 8) is 1
 #:>> (basic-interp/io
       #:in "3,9,8,9,10,9,4,9,99,-1,8" 7) is 0

 #:>> (basic-interp/io
       #:in "3,9,7,9,10,9,4,9,99,-1,8" 8) is 0
 #:>> (basic-interp/io
       #:in "3,9,7,9,10,9,4,9,99,-1,8" 7) is 1

 #:>> (basic-interp/io
       #:in "3,3,1108,-1,8,3,4,3,99" 8) is 1
 #:>> (basic-interp/io
       #:in "3,3,1108,-1,8,3,4,3,99" 7) is 0

 #:>> (basic-interp/io
       #:in "3,3,1107,-1,8,3,4,3,99" 8) is 0
 #:>> (basic-interp/io
       #:in "3,3,1107,-1,8,3,4,3,99" 7) is 1

 #:>> (basic-interp/io
       #:in "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 0) is 0
 #:>> (basic-interp/io
       #:in "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 2) is 1

 #:>> (basic-interp/io
       #:in "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 0) is 0
 #:>> (basic-interp/io
       #:in "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 2) is 1

 #:let in $~bl"""
3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
"""
 #:>> (basic-interp/io #:in in 5) is 999
 #:>> (basic-interp/io #:in in 8) is 1000
 #:>> (basic-interp/io #:in in 9) is 1001)

(def-task task-2 (basic-interp/io 5))
