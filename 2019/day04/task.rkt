#lang aoc

;; the match trick is due to Matthew Butterick

;; has-adjacent? :: (listof number?) -> boolean?
(def (has-adjacent? xs)
  (match xs
    [(list _ ... x x _ ...) #t]
    [_ #f]))


;; has-adjacent*? :: (listof number?) -> boolean?
(def (has-adjacent*? xs)
  (match xs
    [(and (list _ ... x x _ ...)
          (not (list _ ... x x x _ ...))) #t]
    [_ #f]))

(tests
 #:in '(1 1)
 #:on has-adjacent? is #t
 #:on has-adjacent*? is #t

 #:in '(1 2)
 #:on has-adjacent? is #f
 #:on has-adjacent*? is #f

 #:in '(1 1 2)
 #:on has-adjacent? is #t
 #:on has-adjacent*? is #t

 #:in '(2 1 1)
 #:on has-adjacent? is #t
 #:on has-adjacent*? is #t

 #:in '(1 2 1 1 1)
 #:on has-adjacent? is #t
 #:on has-adjacent*? is #f

 #:in '(1 2 1 1 1 3 3 1 3)
 #:on has-adjacent? is #t
 #:on has-adjacent*? is #t)

;; increasing? :: (listof number?) -> boolean?
(def increasing? (curry apply <=))

;; convert :: number? -> (listof number?)
(def (convert i)
  (map char->integer (string->list (number->string i))))

;; calc :: number? number? ((listof number?) -> boolean?) -> (listof number?)
(def (calc start end proc)
  (for*/list ([i (in-range start (add1 end))]
              [xs (in-value (convert i))]
              #:when (proc xs))
    i))

;; run-task :: ((listof number?) -> boolean?) -> number?
(def (run-task proc)
  (length (calc (read) (read) proc)))

(def-task task-1
  (run-task (conjoin has-adjacent? increasing?)))

(def-task task-2
  (run-task (conjoin has-adjacent*? increasing?)))
