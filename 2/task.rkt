#lang rosette

(require "../aoc.rkt"
         syntax/parse/define)

(define !! vector-ref)

;; NOTE: at the moment, for/all in Rosette is inefficient for
;; concrete evaluation (for task-2:alternative).
;; I have a patch that makes it fast, but it's not yet merged
;; to the upstream.

;; interp :: (vectorof number?) -> (vectorof number?)
(define (interp vec)
  (let loop ([prog-counter 0])
    (for/all ([op (vector-ref vec prog-counter) '(1 2 99)])
      (match op
        [99 vec]
        [(or 1 2)
         (define left (!! vec (!! vec (+ prog-counter 1))))
         (define right (!! vec (!! vec (+ prog-counter 2))))
         (define res (!! vec (+ prog-counter 3)))
         (vector-set! vec res ((if (= op 1) + *) left right))
         (loop (+ prog-counter 4))]
        [_ (assert #f 'wrong-op-code)]))))

;; parse-string :: string? -> (vectorof number?)
(define (parse-string s)
  (for/vector ([e (in-list (string-split s ","))])
    (string->number e)))

(define parse+interp (compose1 interp parse-string))

(tests
 (parse+interp "1,9,10,3,2,3,11,0,99,30,40,50")
 ==>
 (parse-string "3500,9,10,70,2,3,11,0,99,30,40,50")

 (parse+interp "1,0,0,0,99")
 ==>
 (parse-string "2,0,0,0,99")

 (parse+interp "2,3,0,3,99")
 ==>
 (parse-string "2,3,0,6,99")

 (parse+interp "2,4,4,5,99,0")
 ==>
 (parse-string "2,4,4,5,99,9801")

 (parse+interp "1,1,1,4,99,5,6,0,99")
 ==>
 (parse-string "30,1,1,4,2,5,6,0,99"))

(define (run-task a b [vec (parse-string (read-line))])
  (vector-set! vec 1 a)
  (vector-set! vec 2 b)
  (interp vec)
  (!! vec 0))

(define-task task-1
  (run-task 12 2))

(tests
 (task-1 "1,9,10,3,2,3,11,0,99,30,40,50,99,99") ==> 5050
 #:because
 (!! (parse+interp "1,12,2,3,2,3,11,0,99,30,40,50,99,99") 0) ==> 5050)

(define magic-num 19690720)

(define-task task-2
  (define-symbolic noun verb integer?)
  (define mod (solve (assert (= (run-task noun verb) magic-num))))
  (+ (* 100 (evaluate noun mod)) (evaluate verb mod)))

(tests
 (task-2 "1,0,0,0,99,19690000,720") ==> (+ (* 100 5) 6)
 #:because
 (!! (parse+interp "1,5,6,0,99,19690000,720") 0) ==> magic-num)

(define-task task-2:alternative
  (define vec (parse-string (read-line)))
  (for*/first ([noun (in-range 0 (vector-length vec))]
               [verb (in-range 0 (vector-length vec))]
               #:when (= (run-task noun verb (vector-copy vec)) magic-num))
    (+ (* 100 noun) verb)))

(tests
 (task-2:alternative "1,0,0,0,99,19690000,720") ==> (+ (* 100 5) 6))
