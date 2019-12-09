#lang aoc

(require "../intcode/intcode.rkt")

(define (basic-interp/io vec in)
  (first (interp-to-complete vec in)))

(define-task task-1
  (first (basic-interp/io (the-input) 1)))

(tests
 #:>> ((lift-input basic-interp/io)
       "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
       +nan.0) is
 '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99)
 #:>> ((lift-input basic-interp/io)
       "104,1125899906842624,99"
       +nan.0) is '(1125899906842624)
 #:>> ((lift-input basic-interp/io)
       "1102,34915192,34915192,7,4,7,99,0"
       +nan.0) is '(1219070632396864))

(define-task task-2
  (first (basic-interp/io (the-input) 2)))
