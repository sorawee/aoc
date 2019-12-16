#lang aoc

(def grid%
  (class object%
    (super-new)

    (init-field grid)
    (field [height (length grid)]
           [width (length (first grid))])

    (define/public (get-cell pos)
      (and (<= 0 (imag-part pos) (sub1 height))
           (<= 0 (real-part pos) (sub1 width))
           (list-ref (list-ref grid (imag-part pos)) (real-part pos))))

    (define/public (in-grid [c #f])
      (for*/list ([i (in-range height)]
                  [j (in-range width)]
                  [pos (in-value (make-rectangular j i))]
                  #:when (or (not c) (equal? c (get-cell pos))))
        pos))))

(def/io my-read
  (new grid% [grid (for/list ([line (in-lines)])
                     (string->list line))]))

(struct point [pos angle round rel-round] #:transparent)
(define 12:00 (atan -1 0))

(def (enum grid orig)
  #:let xs (for*/list ([pos (in-list (send grid in-grid #\#))]
                       #:unless (equal? pos orig))
             (def diff (- pos orig))
             (def dy (imag-part diff))
             (def dx (real-part diff))
             (point pos (atan dy dx) (gcd dy dx) #f))
  #:let xs (sort xs < #:key point-angle)
  (define-values (before after)
    (splitf-at xs (λ (p) (< (point-angle p) 12:00))))
  #:let xs (append after before) ; angle fixed
  #:let groups (group-by point-angle xs)
  (for/list ([group (in-list groups)]
             #:when #t
             [e (in-list (sort group < #:key point-round))]
             [i (in-naturals)])
    (struct-copy point e [rel-round i])))

(def (how-many grid orig)
  (count (λ (p) (= (point-rel-round p) 0)) (enum grid orig)))

(def (calc-max grid)
  #:let pos
  (argmax (how-many grid _) (send grid in-grid #\#))
  (list pos (how-many grid pos)))

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
""" is '(3+4i 8)
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
""" is '(5+8i 33)
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
""" is '(1+2i 35)
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
""" is '(6+3i 41)
 #:on large-example is '(11+13i 210))

(def-task task-1
  (second (calc-max (my-read))))


(def ((get-nth limit) grid)
  #:let (list orig _) (calc-max grid)
  #:let xs (sort (enum grid orig) < #:key point-rel-round)
  (point-pos (list-ref xs (sub1 limit))))

(def-task task-2 ((get-nth 200) (my-read)))

(tests
 #:in (my-read $~nl"""
.#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....#...###..
..#.#.....#....##
""")
 #:on (get-nth 1) is 8+1i
 #:on (get-nth 2) is 9
 #:on (get-nth 3) is 9+i
 #:on (get-nth 4) is 10
 #:on (get-nth 5) is 9+2i
 #:on (get-nth 6) is 11+i
 #:on (get-nth 7) is 12+i
 #:on (get-nth 8) is 11+2i
 #:on (get-nth 9) is 15+i

 #:in (my-read large-example)
 #:on (get-nth 1) is 11+12i
 #:on (get-nth 2) is 12+i
 #:on (get-nth 3) is 12+2i
 #:on (get-nth 10) is 12+8i
 #:on (get-nth 20) is 16
 #:on (get-nth 50) is 16+9i
 #:on (get-nth 100) is 10+16i
 #:on (get-nth 199) is 9+6i
 #:on (get-nth 200) is 8+2i
 #:on (get-nth 201) is 10+9i
 #:on (get-nth 299) is 11+i)
