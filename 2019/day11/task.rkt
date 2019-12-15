#lang aoc

(require "../intcode/intcode.rkt")

(def runner (make-parameter (thunk (send (my-read) interp/writer-reader))))

(def (run-task grid)
  #:let (list writer reader) ((runner))
  (let loop ([dir 0+i] [pos 0])
    (def color (hash-ref grid pos 0))
    (writer color)
    (def paint (reader))
    (when paint
      (def turn (reader))
      (hash-set! grid pos paint)
      (def dir*
        (match turn
          [0 (* 0+i dir)]
          [1 (* 0-i dir)]))
      (loop dir* (+ pos dir*))))
  grid)

(def-task task-1 (hash-count (run-task (make-hash))))

(tests
 #:>> (parameterize ([runner (thunk
                              (list void
                                    (generator ()
                                      (yield-from (list 1 0
                                                        0 0
                                                        1 0
                                                        1 0
                                                        0 1
                                                        1 0
                                                        1 0))
                                      #f)))])
        (task-1)) is 6)

(def-task task-2
  (run-task (make-hash '([0 . 1]))))

(def-task task-2 #:name show
  #:let grid (task-2)
  (def N 50)
  (for ([i (in-range N (- -1 N) -1)])
    (for ([j (in-range (- N) (add1 N))])
      (printf "~a" (if (= 0 (hash-ref grid (make-rectangular j i) 0))
                       "."
                       "*")))
    (newline)))


