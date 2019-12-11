#lang aoc

(def grid%
  (class object%
    (super-new)

    (init-field grid)
    (field [height (length grid)]
           [width (length (first grid))])

    (define/public (get-cell y x)
      (and (<= 0 y (sub1 height))
           (<= 0 x (sub1 width))
           (list-ref (list-ref grid y) x)))

    (define/public (in-grid [c #f])
      (in-generator
       #:arity 2
       (for* ([i (in-range height)]
              [j (in-range width)]
              #:when (or (not c) (equal? c (get-cell i j))))
         (yield i j))))))

(def/io my-read
  (new grid% [grid (for/list ([line (in-lines)])
                     (string->list line))]))

(def ((bad? i j y x) grid)
  #:let dy (- y i)
  #:let dx (- x j)
  #:let common (gcd dy dx)
  (for/or ([k (in-range 1 common)])
    (def y* (+ i (* (/ dy common) k)))
    (def x* (+ j (* (/ dx common) k)))
    ;; this sucks... why can't we have nice continuations?
    (def result
      (match (send grid get-cell y* x*)
        [#f '#:break]
        [#\# #t]
        [_ #f]))
    #:break (equal? result '#:break)
    result))

(tests
 #:in (my-read $~nl"""
X.#.T
..#.T
..#.T
""")
 #:on (bad? 0 0 0 4) is #t
 #:on (bad? 0 0 1 4) is #f
 #:on (bad? 0 0 2 4) is #t)

(def (how-many grid i j)
  #:head (sub1)
  (for*/sum ([(y x) (send grid in-grid #\#)]
             #:unless ((bad? i j y x) grid))
    1))

(def (calc-max grid)
  #:let pos
  (argmax (apply how-many grid _)
          (sequence->list (in-values-sequence (send grid in-grid #\#))))
  (list pos (apply how-many grid pos)))

(tests
 (def large-example $~nl"""
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
"""))

(tests
 #:fn (compose1 calc-max my-read)
 #:on $~nl"""
.#..#
.....
#####
....#
...##
""" is '((4 3) 8)
 #:on $~nl"""
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
""" is '((8 5) 33)
 #:on $~nl"""
#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.
""" is '((2 1) 35)
 #:on $~nl"""
.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..
""" is '((3 6) 41)
 #:on large-example is '((13 11) 210))

(def-task task-1
  (second (calc-max (my-read))))

(define 12:00 (atan -1 0))

(def (pt< a b)
  #:let (list _ _ theta-a i-a) a
  #:let (list _ _ theta-b i-b) b
  (cond
    [(= theta-a theta-b) (< i-a i-b)]
    [else (< theta-a theta-b)]))

(def (enum grid)
  #:let N (add1 (max (get-field width grid) (get-field height grid)))
  #:let xs (for*/list ([i (in-range (- N) N)] [j (in-range (- N) N)]
                       #:unless (and (= i 0) (= j 0)))
             (list i j (atan i j) (gcd i j)))
  #:let xs (sort xs pt<)
  #:let xs (group-by third xs)
  (define-values (before after)
    (splitf-at xs (λ (g) (< (third (first g)) 12:00))))
  (append after before))

(def ((get-nth limit) grid)
  #:let (list (list orig-y orig-x) _) (calc-max grid)
  (let loop ([loc (enum grid)] [limit limit])
    (match loc
      [(list (list) as ...) (loop as limit)]
      [(list (list (list y x _ ...) bs ...) as ...)
       (def y* (+ orig-y y))
       (def x* (+ orig-x x))
       (cond
         [(equal? (send grid get-cell y* x*) #\#)
          (if (= limit 1)
              (list y* x*)
              (loop (append as (list bs)) (sub1 limit)))]
         [else (loop (cons bs as) limit)])]
      [(list) (error 'out-of-bounds)])))

(def-task task-2
  ((get-nth 200) (my-read)))

(tests #:name "hello"
 #:in (my-read $~nl"""
.#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....#...###..
..#.#.....#....##
""")
 #:on (get-nth 1) is '(1 8)
 #:on (get-nth 2) is '(0 9)
 #:on (get-nth 3) is '(1 9)
 #:on (get-nth 4) is '(0 10)
 #:on (get-nth 5) is '(2 9)
 #:on (get-nth 6) is '(1 11)
 #:on (get-nth 7) is '(1 12)
 #:on (get-nth 8) is '(2 11)
 #:on (get-nth 9) is '(1 15)

 #:on (get-nth 10) is '(2 12)
 #:on (get-nth 11) is '(2 13)
 #:on (get-nth 12) is '(2 14)
 #:on (get-nth 13) is '(2 15)
 #:on (get-nth 14) is '(3 12)
 #:on (get-nth 15) is '(4 16)
 #:on (get-nth 16) is '(4 15)
 #:on (get-nth 17) is '(4 10)
 #:on (get-nth 18) is '(4 4)

 #:in (my-read large-example)
 #:on (get-nth 1) is '(12 11)
 #:on (get-nth 2) is '(1 12)
 #:on (get-nth 3) is '(2 12)
 #:on (get-nth 10) is '(8 12)
 #:on (get-nth 20) is '(0 16)
 #:on (get-nth 50) is '(9 16)
 #:on (get-nth 100) is '(16 10)
 #:on (get-nth 199) is '(6 9)
 #:on (get-nth 200) is '(2 8)
 #:on (get-nth 201) is '(9 10)
 #:on (get-nth 299) is '(1 11)
 )
