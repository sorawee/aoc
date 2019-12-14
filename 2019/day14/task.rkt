#lang aoc

(def (parse-one s)
  #:let (list a b) (string-split s " ")
  (list (string->number a) b))

(def/io my-read
  (for/hash ([line (in-lines)])
    (match-define (pregexp "^(.*?)=>(.*?)$" (list _ left right)) line)
    (let* ([left (map string-trim (string-split left ","))]
           [right (string-trim right)]
           [left (map parse-one left)]
           [right (parse-one right)])
      (values (second right) (list (first right) left)))))

(def (get data amt what)
  #:let reserve (make-hash)
  (let loop ([amt amt] [what what])
    (match what
      ["ORE" amt]
      [_
       (match (hash-ref reserve what 0)
         [0
          (match-define (list output-amt ingrediants) (hash-ref data what))
          (def num-bundles (ceiling (/ amt output-amt)))
          (def net-output-amt (* num-bundles output-amt))
          (hash-set! reserve what net-output-amt)
          (+ (for/sum ([item (in-list ingrediants)])
               (match-define (list amt what) item)
               (loop (* num-bundles amt) what))
             (loop amt what))]
         [n #:when (>= n amt)
            (hash-set! reserve what (- n amt))
            0]
         [n (hash-set! reserve what 0)
            (loop (- amt n) what)])])))

(def-task task-1
  (get (my-read) 1 "FUEL"))

(tests
 (def ex-1 $~nl"""
157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
""")

 (def ex-2 $~nl"""
2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF
""")

 (def ex-3 $~nl"""
171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX
""")

 #:fn task-1
 #:on $~nl"""
10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
""" is 31
 #:on $~nl"""
9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL
""" is 165
 #:on ex-1 is 13312
 #:on ex-2 is 180697
 #:on ex-3 is 2210736)

(def AVAIL-ORE (make-parameter 1000000000000))

(def (binsearch data low high)
  #:let result
  (let loop ([low low] [high high] [ans #f])
    (cond
      [(<= low high)
       (def mid (quotient (+ low high) 2))
       (cond
         [(<= (get data mid "FUEL") (AVAIL-ORE))
          (loop (add1 mid) high mid)]
         [else (loop low (sub1 mid) ans)])]
      [else ans]))
  (and (not (equal? result high)) result))

(def-task task-2
  #:let data (my-read)
  (let loop ([n 1])
    (or (binsearch data 1 n) (loop (* n 2)))))

(tests
 #:fn task-2
 #:on ex-1 is 82892753
 #:on ex-2 is 5586022
 #:on ex-3 is 460664)
