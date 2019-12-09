#lang aoc

(require "../intcode/intcode.rkt")

(define N (make-parameter 5))

(define (interp-pass amp-confs input)
  (for/fold ([input input]) ([amp-conf (in-list amp-confs)])
    (match-define (list in out runner) amp-conf)
    (displayln input out)
    (runner)
    (read in)))

(define (get-amp-confs vec perm)
  (define amp-confs
    (for/list ([i (in-range (N))])
      (define-values (in-for-in out-for-out) (make-pipe))
      (define-values (in-for-out out-for-in) (make-pipe))
      (list in-for-out
            out-for-out
            (make-interp (hash-copy vec) in-for-in out-for-in))))
  (for ([amp-conf (in-list amp-confs)] [phase (in-list perm)])
    (match-define (list _ out _) amp-conf)
    (displayln phase out))
  amp-confs)

(define (interp-single amp-confs)
  (interp-pass amp-confs 0))

(define-task task-1
  (define vec (the-input))
  (for/max ([perm (in-permutations '(0 1 2 3 4))])
    (interp-multi (get-amp-confs vec perm))))

(tests
 #:let in "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
 #:>> (interp-multi (get-amp-confs (parse-string in) '(4 3 2 1 0))) is 43210
 #:>> (task-1 in) is 43210

 #:let in $~bl"""
3,23,3,24,1002,24,10,24,1002,23,-1,23,
101,5,23,23,1,24,23,23,4,23,99,0,0
"""
 #:>> (interp-multi (get-amp-confs (parse-string in) '(0 1 2 3 4))) is 54321
 #:>> (task-1 in) is 54321

 #:let in $~bl"""
3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
"""
 #:>> (interp-multi (get-amp-confs (parse-string in) '(1 0 4 3 2))) is 65210
 #:>> (task-1 in) is 65210)

(define (interp-multi amp-confs)
  (let loop ([input 0])
    (define val (interp-pass amp-confs input))
    (cond
      [(eof-object? val) input]
      [else (loop val)])))

(define-task task-2
  (define vec (the-input))
  (for/max ([perm (in-permutations '(5 6 7 8 9))])
    (interp-multi (get-amp-confs vec perm))))

(tests
 #:let in $~bl"""
3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5
"""
 #:>> (interp-multi (get-amp-confs (parse-string in) '(9 8 7 6 5))) is
 139629729

 #:>> (task-2 in) is 139629729

 #:let in $~bl"""
3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10
"""
 #:>> (interp-multi (get-amp-confs (parse-string in) '(9 7 8 5 6))) is
 18216

 #:>> (task-2 in) is 18216)
