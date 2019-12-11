#lang aoc

(provide make-interp
         interp-to-complete
         basic-interp
         my-read
         !!
         !!!)
(require racket/generator)

(def !! (hash-ref _ _ 0))
(def !!! hash-set!)

(def current-base (make-parameter 0))

;; make-interp :: (hash number? number?) input-port? output-port? -> (-> any)
(def (make-interp vec in out)
  #:head (parameterize ([current-base 0]))
  #:head (generator ())
  #:head (let loop ([prog-counter 0]))
  (def (get-arg pos #:literal? [literal? #f])
    (def v (+ prog-counter pos))
    (def mode
      (remainder (quotient op+arg-code (expt 10 (add1 pos))) 10))
    (def output (match mode
                     [0 (!! vec v)]
                     [1 v]
                     [2 (+ (current-base) (!! vec v))]))
    (if literal? output (!! vec output)))
  (def op+arg-code (hash-ref vec prog-counter))
  (def op (remainder op+arg-code 100))
  (match op
    [(or 1 2)
     (def left (get-arg 1))
     (def right (get-arg 2))
     (def res (get-arg 3 #:literal? #t))
     (hash-set! vec res ((match op [1 +] [2 *]) left right))
     (loop (+ prog-counter 4))]
    [3 (hash-set! vec (get-arg 1 #:literal? #t) (read in))
       (loop (+ prog-counter 2))]
    [4 (displayln (get-arg 1) out)
       (yield)
       (loop (+ prog-counter 2))]
    [(or 5 6)
     (def left (get-arg 1))
     (def right (get-arg 2))
     (cond
       [((match op [5 (negate zero?)] [6 zero?]) left)
        (loop right)]
       [else (loop (+ prog-counter 3))])]
    [(or 7 8)
     (def left (get-arg 1))
     (def right (get-arg 2))
     (def pos (get-arg 3 #:literal? #t))
     (hash-set! vec pos (if ((match op [7 <] [8 =]) left right) 1 0))
     (loop (+ prog-counter 4))]
    [9
     (def val (get-arg 1))
     (current-base (+ (current-base) val))
     (loop (+ prog-counter 2))]
    [99 (close-input-port in)
        (close-output-port out)
        vec]))

;; interp-to-complete :: (hash number? number?) number? ->
;;                       (list (listof number?) (hash number? number?))
(def (interp-to-complete vec in)
  (define-values (in-for-in out-for-out) (make-pipe))
  (define-values (in-for-out out-for-in) (make-pipe))
  (displayln in out-for-out)
  (def gen (make-interp vec in-for-in out-for-in))
  (list (for*/list ([_ (sequence-map list (in-producer gen))]
                    [out (in-value (read in-for-out))])
          #:break (eof-object? out)
          out)
        (gen)))

(def (basic-interp vec)
  (second (interp-to-complete vec +nan.0)))

;; my-read :: string? -> (hash number? number?)
(def/io my-read
  #:let toks (string-split (string-replace (read-line) "\n" "") ",")
  #:head (make-hash)
  (for/list ([e (in-list toks)] [i (in-naturals)])
    (cons i (string->number e))))
