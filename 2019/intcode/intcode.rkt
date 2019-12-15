#lang aoc

(provide my-read)
(require racket/generator)

(def intcode%
  (block
   #:head (print-mixin)
   #:head (equality-mixin)
   (class* object% (equality<%> print<%>)
     (super-new)
     (init intcode)

     (def vec intcode)
     (def current-base (make-parameter 0))
     (def current-in-chan (make-parameter read))
     (def current-out-chan (make-parameter println))

     (define/public (get-code) vec)
     (define/public (get-base) current-base)
     (define/public (get-in-chan) current-in-chan)
     (define/public (get-out-chan) current-out-chan)

     (define/public (!! pos) (hash-ref vec pos 0))
     (define/public (!!! pos v) (hash-set! vec pos v))
     (define/public (size) (hash-count vec))
     (define/public (copy) (new intcode% [intcode (hash-copy vec)]))

     (public interp)
     (def (interp)
       #:head (parameterize ([current-base 0]))
       #:head (let loop ([prog-counter 0]))
       (def (get-arg pos #:literal? [literal? #f])
         (def v (+ prog-counter pos))
         (def mode (remainder (quotient op+arg-code (expt 10 (add1 pos))) 10))
         (def output (match mode
                       [0 (!! v)]
                       [1 v]
                       [2 (+ (current-base) (!! v))]))
         (if literal? output (!! output)))
       (def op+arg-code (!! prog-counter))
       (def op (remainder op+arg-code 100))
       (match op
         [(or 1 2)
          (def left (get-arg 1))
          (def right (get-arg 2))
          (def res (get-arg 3 #:literal? #t))
          (!!! res ((match op [1 +] [2 *]) left right))
          (loop (+ prog-counter 4))]
         [3
          (!!! (get-arg 1 #:literal? #t) ((current-in-chan)))
          (loop (+ prog-counter 2))]
         [4 ((current-out-chan) (get-arg 1))
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
          (!!! pos (if ((match op [7 <] [8 =]) left right) 1 0))
          (loop (+ prog-counter 4))]
         [9
          (def val (get-arg 1))
          (current-base (+ (current-base) val))
          (loop (+ prog-counter 2))]
         [99 vec]))

     (public basic-interp)
     (def (basic-interp ins)
       (parameterize ([current-in-chan (sequence->generator ins)]
                      [current-out-chan yield])
         (sequence->list (in-generator (interp)))))

     (define/public (get-equality) vec)
     (define/public (get-printable) vec))))

;; my-read :: -> (is-a?/c intcode%)
(def/io my-read
  #:let toks (string-split (string-replace (read-line) "\n" "") ",")
  (new intcode%
       [intcode (make-hash
                 (for/list ([e (in-list toks)] [i (in-naturals)])
                   (cons i (string->number e))))]))
