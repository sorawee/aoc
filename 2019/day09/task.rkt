#lang aoc

(require "../intcode/intcode.rkt")

(define (basic-interp/io vec in)
  (first (interp-to-complete vec in)))

(define-task task-1
  (first (basic-interp/io (the-input) 1)))

(tests
 #:fn ((lift-input basic-interp/io) _ +nan.0)
 #:on "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" is
 '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99)
 #:on "104,1125899906842624,99" is '(1125899906842624)
 #:on "1102,34915192,34915192,7,4,7,99,0" is '(1219070632396864))

(define-task task-2
  (first (basic-interp/io (the-input) 2)))
