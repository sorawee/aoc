#lang s-exp "../aoc.rkt"

(require rosette)

(define ! vector-ref)

;; NOTE: we are lucky that this actually works.
;; In particular, pc happens to always be concrete throughout the execution.
;; If this is not the case, we might need to employ symbolic reflection.

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

(task-1
 (define vec (parse-string (read-line)))
 (vector-set! vec 1 12)
 (vector-set! vec 2 2)
 (void (interp vec))
 (! vec 0))

(task-2
 (define vec (parse-string (read-line)))
 (define-symbolic noun verb integer?)
 (vector-set! vec 1 noun)
 (vector-set! vec 2 verb)
 (void (interp vec))
 (define mod (solve (assert (= (! vec 0) 19690720))))
 (+ (* 100 (evaluate noun mod))
    (evaluate verb mod)))
