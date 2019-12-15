#lang aoc

(require "../intcode/intcode.rkt")

(def game%
  (class object% (super-new)
    (init-field intcode
                [silent? #t])

    (def reader
      (generator ()
        (parameterize ([(send intcode get-in-chan) (λ () (play))]
                       [(send intcode get-out-chan) yield])
          (send intcode interp))))

    (def (play)
      (unless silent?
        (render))
      #:let (list _ ball-x) (get-pos "o")
      #:let (list _ pad-x) (get-pos "_")
      (cond
        [(= ball-x pad-x) 0]
        [(> ball-x pad-x) 1]
        [else -1]))

    (define board (make-hash))
    (define score 0)

    (define/public (set-cell! y x t)
      (hash-set! board (list y x) t))

    (define/public (set-score! s)
      (set! score s))

    (define/public (get-board) board)
    (define/public (get-score) score)

    (define/public (get-pos what)
      (for/first ([(k v) (in-hash board)] #:when (equal? v what))
        k))

    (public render)
    (def (render)
      #:let y-max (for/max ([pos (in-hash-keys board)])
                    (first pos))
      #:let x-max (for/max ([pos (in-hash-keys board)])
                    (second pos))
      (display "\033c")
      (for ([i (in-range (add1 y-max))])
        (for ([j (in-range (add1 x-max))])
          (display (hash-ref board (list i j))))
        (newline))
      (newline))

    (define/public (main-loop)
      (let loop ()
        (match (list (reader) (reader) (reader))
          [(list -1 0 x) (set-score! x)
                         (loop)]
          [(list x y 0) (set-cell! y x " ")
                        (loop)]
          [(list x y 1) (set-cell! y x "|")
                        (loop)]
          [(list x y 2) (set-cell! y x "#")
                        (loop)]
          [(list x y 3) (set-cell! y x "_")
                        (loop)]
          [(list x y 4) (set-cell! y x "o")
                        (loop)]
          [_ (void)])))))

(def-task task-1
  #:let game (new game% [intcode (my-read)])
  (send game main-loop)
  (for/sum ([(k v) (send game get-board)] #:when (equal? v "#"))
    1))

(def (play game)
  (send (get-field intcode game) !!! 0 2)
  (send game main-loop)
  (send game get-score))

(def-task task-2
  (play (new game% [intcode (my-read)])))

(def-task task-2 #:name show
  (play (new game%
             [intcode (my-read)]
             [silent? #f])))
