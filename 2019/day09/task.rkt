#lang aoc

(require "../intcode/intcode.rkt")

(def ((basic-interp/io in) vec)
  (first (interp-to-complete vec in)))

(def-task task-1
  (first ((basic-interp/io 1) (my-read))))

(tests
 #:fn (compose1 (basic-interp/io +nan.0) my-read)
 #:on "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" is
 '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99)
 #:on "104,1125899906842624,99" is '(1125899906842624)
 #:on "1102,34915192,34915192,7,4,7,99,0" is '(1219070632396864))

(def-task task-2
  (first ((basic-interp/io 2) (my-read))))
