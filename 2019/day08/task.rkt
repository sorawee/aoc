#lang aoc

(define width (make-parameter 25))
(define height (make-parameter 6))

(define (get-layers s)
  (define grouped (for/list ([row (in-slice (width) s)]) row))
  (for/list ([group (in-slice (height) grouped)]) group))

(define-task task-1
  (define layers (get-layers (read-line)))
  (define the-layer (argmin (λ (l) (count (equal? _ #\0) (flatten l)))
                            layers))
  (* (count (equal? _ #\1) (flatten the-layer))
     (count (equal? _ #\2) (flatten the-layer))))

(tests
 #:>> (parameterize ([width 2]
                     [height 2])
        (task-1 "123456789012")) is 1)

(define-task task-2
  (define layers (get-layers (read-line)))
  (string-join
   (for/list ([row (in-range (height))])
     (list->string
      (for/list ([col (in-range (width))])
        (for/or ([layer (in-list layers)])
          (match (list-ref (list-ref layer row) col)
            [#\0 #\.]
            [#\1 #\x]
            [#\2 #f])))))
   "\n"))

(define-task task-2 #:name show
  (displayln (task-2 (read-line))))

(tests
 #:>> (parameterize ([width 2]
                     [height 2])
        (task-2 "0222112222120000")) is
 $~nl"""
.x
x.
""")
