#lang aoc

(def width (make-parameter 25))
(def height (make-parameter 6))

(def/io my-read
  #:let grouped (for/list ([row (in-slice (width) (read-line))]) row)
  (for/list ([group (in-slice (height) grouped)]) group))

(def-task task-1
  #:let layers (my-read)
  #:let the-layer (argmin (λ (l) (count (equal? _ #\0) (flatten l)))
                          layers)
  (* (count (equal? _ #\1) (flatten the-layer))
     (count (equal? _ #\2) (flatten the-layer))))

(tests
 #:>> (parameterize ([width 2] [height 2])
        (task-1 "123456789012")) is 1)

(def-task task-2
  #:let layers (my-read)
  #:head ((string-join _ "\n"))
  #:head (for/list ([row (in-range (height))]))
  #:head (list->string)
  #:head (for/list ([col (in-range (width))]))
  #:head (for/or ([layer (in-list layers)]))
  (match (list-ref (list-ref layer row) col)
    [#\0 #\.]
    [#\1 #\x]
    [#\2 #f]))

(def-task task-2 #:name show
  (displayln (task-2)))

(tests
 #:>> (parameterize ([width 2] [height 2])
        (task-2 "0222112222120000")) is
 $~nl"""
.x
x.
""")
