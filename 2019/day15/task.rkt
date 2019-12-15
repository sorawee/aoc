#lang aoc

(require "../intcode/intcode.rkt")

(def DIR '(1 -1 0+i 0-i))

(def (find-path prev v)
  (let loop ([v v] [result '()])
    (def u (hash-ref prev v #f))
    (cond
      [u (loop u (cons (- v u) result))]
      [else result])))

(def (shortest-path board start)
  #:let visited (mutable-set)
  #:let q (make-queue)
  #:let prev (make-hash)
  (set-add! visited start)
  (enqueue! q start)
  (let loop ()
    (cond
      [(queue-empty? q) prev]
      [else
       (def now (dequeue! q))
       (for* ([dir (in-list DIR)]
              [new-pos (in-value (+ now dir))]
              #:unless (set-member? visited new-pos)
              #:unless (equal? (hash-ref board new-pos "#") "#"))
         (set-add! visited new-pos)
         (hash-set! prev new-pos now)
         (enqueue! q new-pos))
       (loop)])))

(def maze%
  (class object% (super-new)
    (init-field intcode)

    (def board (make-hash '([0 . "."])))
    (def current-pos 0)
    (match-define (list writer reader) (send intcode interp/writer-reader))

    (def (move pos)
      (for ([dir (in-list (find-path (shortest-path board current-pos) pos))])
        (writer
         (match dir
           [0+i 1]
           [0-i 2]
           [-1 3]
           [1 4]))
        ;; NOTE: need to call reader to flush the queue
        (assert (member (reader) '(1 2))))
      (set! current-pos pos))

    (define/public (get-target)
      (for/first ([(k v) (in-hash board)]
                  #:when (equal? v "*"))
        k))

    (define/public (get-board) board)

    (def (try-move dir)
      (writer (match dir
                [-1 3]
                [1 4]
                [0+i 1]
                [0-i 2]))
      #:let new-pos (+ current-pos dir)
      (match (reader)
        [0 (hash-set! board new-pos "#")]
        [1 (hash-set! board new-pos ".")
           (set! current-pos new-pos)]
        [2 (hash-set! board new-pos "*")
           (set! current-pos new-pos)]))

    (define/public (main-loop)
      (let loop ()
        (when (for/or ([(pos v) (in-hash board)]
                       #:when (member v '("." "*"))
                       [dir (in-list DIR)]
                       #:unless (hash-has-key? board (+ pos dir)))
                (move pos)
                (try-move dir))
          (loop))))

    (public render)
    (def (render)
      #:let y-max (for/max ([pos (in-hash-keys board)])
                    (imag-part pos))
      #:let x-max (for/max ([pos (in-hash-keys board)])
                    (real-part pos))
      #:let y-min (for/min ([pos (in-hash-keys board)])
                    (imag-part pos))
      #:let x-min (for/min ([pos (in-hash-keys board)])
                    (real-part pos))
      (for ([i (in-range y-max (sub1 y-min) -1)])
        (for ([j (in-range x-min (add1 x-max))])
          (display (hash-ref board (make-rectangular j i) " ")))
        (newline))
      (newline))))

(def-task task-1
  #:let maze (new maze% [intcode (my-read)])
  (send maze main-loop)
  (length (shortest-path (send maze get-board) 0 (send maze get-target))))

(def-task task-2
  #:let maze (new maze% [intcode (my-read)])
  (send maze main-loop)
  #:let board (send maze get-board)
  #:let prev (shortest-path board (send maze get-target))
  (for/max ([(k v) (in-hash board)] #:when (equal? v "."))
    (length (find-path prev k))))
