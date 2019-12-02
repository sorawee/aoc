#lang s-exp "../aoc.rkt"

(require rosette)

(define ! vector-ref)

;; NOTE: I just want to use Rosette for fun, but getting Rosette to work
;; properly will be difficult here.
;; In particular, if the program counter becomes a symbolic value,
;; the symbolic evaluation won't terminate! Fixing it will require
;; symbolic reflection, but at that point we could have just solved
;; it with the "traditional" method.
;;
;; In any case, the input that I got didn't cause the pc to become symbolic,
;; so... YOLO.

;; interp :: (vectorof number?) -> (vectorof number?)
(define (interp vec)
  (let loop ([pc 0])
    (match (vector-ref vec pc)
      [99 vec]
      [op
       (define left (! vec (! vec (+ pc 1))))
       (define right (! vec (! vec (+ pc 2))))
       (define res (! vec (+ pc 3)))
       (vector-set! vec res ((if (= op 1) + *) left right))
       (loop (+ pc 4))])))

;; parse-string :: string? -> (vectorof number?)
(define (parse-string s)
  (for/vector ([e (in-list (string-split s ","))])
    (string->number e)))

(tests
 (interp (parse-string "1,9,10,3,2,3,11,0,99,30,40,50"))
 ==>
 (parse-string "3500,9,10,70,2,3,11,0,99,30,40,50")

 (interp (parse-string "1,0,0,0,99"))
 ==>
 (parse-string "2,0,0,0,99")

 (interp (parse-string "2,3,0,3,99"))
 ==>
 (parse-string "2,3,0,6,99")

 (interp (parse-string "2,4,4,5,99,0"))
 ==>
 (parse-string "2,4,4,5,99,9801")

 (interp (parse-string "1,1,1,4,99,5,6,0,99"))
 ==>
 (parse-string "30,1,1,4,2,5,6,0,99"))

(define-task task-1
 (define vec (parse-string (read-line)))
 (vector-set! vec 1 12)
 (vector-set! vec 2 2)
 (void (interp vec))
 (! vec 0))

(tests
 (! (interp (parse-string "1,12,2,3,2,3,11,0,99,30,40,50,99,99")) 0) ==> 5050
 (task-1 "1,9,10,3,2,3,11,0,99,30,40,50,99,99") ==> 5050)

(define-task task-2
 (define vec (parse-string (read-line)))
 (define-symbolic noun verb integer?)
 (vector-set! vec 1 noun)
 (vector-set! vec 2 verb)
 (void (interp vec))
 (define mod (solve (assert (= (! vec 0) 19690720))))
 (+ (* 100 (evaluate noun mod))
    (evaluate verb mod)))

(tests
 (task-2 "1,0,0,0,99,19690718,2") ==> (+ (* 100 5) 6))
