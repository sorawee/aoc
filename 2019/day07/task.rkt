#lang aoc

(require "../intcode/intcode.rkt")

(def (interp-pass amp-confs input)
  (for/fold ([input input]) ([amp-conf (in-list amp-confs)])
    (match-define (list writer reader) amp-conf)
    (writer input)
    (reader)))

(def (get-amp-confs intcode perm)
  #:let amp-confs
  (for/list ([phase (in-list perm)])
    (def q (make-queue))
    (def the-intcode (send intcode copy))
    (list
     (enqueue! q _)
     (generator ()
       (parameterize ([(send the-intcode get-in-chan) (thunk (dequeue! q))]
                      [(send the-intcode get-out-chan) yield])
         (send the-intcode interp)))))
  (for ([amp-conf (in-list amp-confs)] [phase (in-list perm)])
    (match-define (list writer _) amp-conf)
    (writer phase))
  amp-confs)

(def (interp-single amp-confs)
  (interp-pass amp-confs 0))

(def-task task-1
  #:let intcode (my-read)
  (for/max ([perm (in-permutations '(0 1 2 3 4))])
    (interp-single (get-amp-confs intcode perm))))

(tests
 #:let in "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
 #:>> (interp-single (get-amp-confs (my-read in) '(4 3 2 1 0))) is 43210
 #:>> (task-1 in) is 43210

 #:let in $~bl"""
3,23,3,24,1002,24,10,24,1002,23,-1,23,
101,5,23,23,1,24,23,23,4,23,99,0,0
"""
 #:>> (interp-single (get-amp-confs (my-read in) '(0 1 2 3 4))) is 54321
 #:>> (task-1 in) is 54321

 #:let in $~bl"""
3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
"""
 #:>> (interp-single (get-amp-confs (my-read in) '(1 0 4 3 2))) is 65210
 #:>> (task-1 in) is 65210)

(def (interp-multi amp-confs)
  (let loop ([input 0])
    (def val (interp-pass amp-confs input))
    (cond
      [val (loop val)]
      [else input])))

(def-task task-2
  #:let intcode (my-read)
  (for/max ([perm (in-permutations '(5 6 7 8 9))])
    (interp-multi (get-amp-confs intcode perm))))

(tests
 #:let in $~bl"""
3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5
"""
 #:>> (interp-multi (get-amp-confs (my-read in) '(9 8 7 6 5))) is
 139629729

 #:>> (task-2 in) is 139629729

 #:let in $~bl"""
3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10
"""
 #:>> (interp-multi (get-amp-confs (my-read in) '(9 7 8 5 6))) is
 18216

 #:>> (task-2 in) is 18216)
