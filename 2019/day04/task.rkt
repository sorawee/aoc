#lang racket

(require "../../aoc.rkt")

(define (adjacent-len xs)
  (map length
       (for/fold ([prev '((#f))] #:result (rest (reverse prev)))
                 ([x (in-list xs)])
         (match-define (cons (cons y ys) zs) prev)
         (if (eq? x y)
             (cons (list* x y ys) zs)
             (cons (list x) prev)))))

(tests
 #:>> (adjacent-len (convert 111111)) is '(6)
 #:>> (adjacent-len (convert 223450)) is '(2 1 1 1 1)
 #:>> (adjacent-len (convert 123789)) is '(1 1 1 1 1 1)

 #:>> (adjacent-len (convert 112233)) is '(2 2 2)
 #:>> (adjacent-len (convert 123444)) is '(1 1 1 3)
 #:>> (adjacent-len (convert 111122)) is '(4 2))

(define (has-adjacent? xs)
  (ormap (>= _ 2) (adjacent-len xs)))

(define (has-adjacent*? xs)
  (ormap (= _ 2) (adjacent-len xs)))

(define increasing? (curry apply <=))

(define (convert i)
  (map char->integer (string->list (number->string i))))

(define (calc start end proc)
  (for*/list ([i (in-range start (add1 end))]
              [xs (in-value (convert i))]
              #:when (proc xs))
    i))

(define (run-task proc)
  (length (calc (read) (read) proc)))

(define-task task-1
  (run-task (conjoin has-adjacent? increasing?)))

(define-task task-2
  (run-task (conjoin has-adjacent*? increasing?)))
