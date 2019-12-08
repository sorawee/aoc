#lang aoc

(provide make-interp
         interp-to-complete
         basic-interp
         parse-string
         the-input
         lift-input)
(require racket/generator)

(define !! vector-ref)

;; make-interp :: (vectorof number?) input-port? output-port? -> (-> any)
(define (make-interp vec in out)
  (generator ()
    (let loop ([prog-counter 0])
      (define (get-arg pos #:literal? [literal? #f])
        (define v (+ prog-counter pos))
        (define mode
          (remainder (quotient op+arg-code (expt 10 (add1 pos))) 10))
        (define output (match mode [0 (!! vec v)] [1 v]))
        (if literal? output (!! vec output)))
      (define op+arg-code (vector-ref vec prog-counter))
      (define op (remainder op+arg-code 100))
      (match op
        [(or 1 2)
         (define left (get-arg 1))
         (define right (get-arg 2))
         (define res (get-arg 3 #:literal? #t))
         (vector-set! vec res ((match op [1 +] [2 *]) left right))
         (loop (+ prog-counter 4))]
        [3 (vector-set! vec (get-arg 1 #:literal? #t) (read in))
           (loop (+ prog-counter 2))]
        [4 (displayln (get-arg 1) out)
           (yield)
           (loop (+ prog-counter 2))]
        [(or 5 6)
         (define left (get-arg 1))
         (define right (get-arg 2))
         (cond
           [((match op [5 (negate zero?)] [6 zero?]) left)
            (loop right)]
           [else (loop (+ prog-counter 3))])]
        [(or 7 8)
         (define left (get-arg 1))
         (define right (get-arg 2))
         (define pos (!! vec (+ prog-counter 3)))
         (vector-set! vec pos (if ((match op [7 <] [8 =]) left right) 1 0))
         (loop (+ prog-counter 4))]
        [99 (close-input-port in)
            (close-output-port out)
            vec]))))

;; interp-to-complete :: (vectorof number?) number? ->
;;                       (list (listof number?) (vectorof number?))
(define (interp-to-complete vec in)
  (define-values (in-for-in out-for-out) (make-pipe))
  (define-values (in-for-out out-for-in) (make-pipe))
  (displayln in out-for-out)
  (define gen (make-interp vec in-for-in out-for-in))
  (list (for*/list ([_ (sequence-map list (in-producer gen))]
                    [out (in-value (read in-for-out))])
          #:break (eof-object? out)
          out)
        (gen)))

(define (basic-interp vec)
  (second (interp-to-complete vec +nan.0)))

;; parse-string :: string? -> (vectorof number?)
(define (parse-string s)
  (for/vector ([e (in-list (string-split (string-replace s "\n" "") ","))])
    (string->number e)))

(define (the-input)
  (parse-string (read-line)))

(define (lift-input f)
  (λ (x . xs) (apply f (parse-string x) xs)))
