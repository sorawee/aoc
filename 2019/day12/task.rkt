#lang aoc

(def AXIS 3)

(def (step/axis old)
  (for/list ([e (in-list old)])
    (match-define (list e-pos e-vel) e)
    (def e-vel*
      (+ e-vel
         (for/sum ([e* (in-list old)])
           (def e*-pos (first e*))
           (sgn (- e*-pos e-pos)))))
    (def e-pos* (+ e-pos e-vel*))
    (list e-pos* e-vel*)))

(def (init-input in)
  (for/list ([axis (in-range AXIS)])
    (for/list ([row (in-list in)])
      (list (list-ref row axis) 0))))

(def (repeat/axis old limit)
  (for/fold ([now old]) ([i (in-range limit)])
    (step/axis now)))

(tests
 (def ex-input-1
   (init-input
    '([-1 0 2]
      [2 -10 -7]
      [4 -8 8]
      [3 5 -1])))

 #:>> (repeat/axis (first ex-input-1) 10) is
 '([2 -3] [1 -1] [3 3] [2 1])

 #:>> (repeat/axis (second ex-input-1) 10) is
 '([1 -2] [-8 1] [-6 2] [0 -1])

 #:>> (repeat/axis (third ex-input-1) 10) is
 '([-3 1] [0 3] [1 -3] [4 -1]))

(def transpose (apply map list _))

(def input (make-parameter
            (init-input
             '([0 6 1]
               [4 4 19]
               [-11 1 8]
               [2 19 15]))))

(def limit (make-parameter 1000))

(def-task task-1
  #:let by-axis (for/list ([axis (in-list (input))])
                  (repeat/axis axis (limit)))
  (for/sum ([group (transpose by-axis)])
    (for/product ([subgroup (transpose group)])
      (apply + (map abs subgroup)))))

(tests
 #:>> (parameterize ([input ex-input-1]
                     [limit 10])
        (task-1)) is 179

 (def ex-input-2
   (init-input
    '([-8 -10 0]
      [5 5 10]
      [2 -7 3]
      [9 -8 -3])))

 #:>> (parameterize ([input ex-input-2]
                     [limit 100])
        (task-1)) is 1940)

(def (repeat/axis* old)
  #:let hsh (make-hash)
  (let loop ([now old] [i 0])
    (match (hash-ref hsh now #f)
      [#f
       (hash-set! hsh now i)
       (loop (step/axis now) (add1 i))]
      [old
       (assert (= old 0))
       ;; if it's not 0, then we need to use Chinese remainder theorem
       ;; but since it is, we don't need to!
       i])))

(def-task task-2
  (apply
   lcm
   (for/list ([axis (in-list (input))])
     (repeat/axis* axis))))

(tests
 #:>> (parameterize ([input ex-input-1])
        (task-2)) is 2772

 #:>> (parameterize ([input ex-input-2])
        (task-2)) is 4686774924)
