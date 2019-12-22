#lang aoc

(require math)

(def/io my-read
  (for/list ([line (in-lines)])
    (match line
      ["deal into new stack" 'reverse]
      [(pregexp "^cut (.*)$" (list _ n)) `(cut ,(string->number n))]
      [(pregexp "^deal with increment (.*)$" (list _ n))
       `(increment ,(string->number n))])))

(def ((next-pos instr) pos)
  (for/fold ([pos pos]) ([instr (in-list instr)])
    (match instr
      ['reverse (- (add1 pos))]
      [`(cut ,n) (- pos n)]
      [`(increment ,n) (* pos n)])))

(def-task task-1
  (modulo ((next-pos (my-read)) 2019) 10007))

(tests
 (def (test-next-pos s)
   #:let instr (my-read s)
   #:let M 10
   #:let vec (make-vector M #f)
   (for ([pos (in-range M)])
     (vector-set! vec (modulo ((next-pos instr) pos) M) pos))
   (vector->list vec))

 #:fn test-next-pos
 #:on $~nl"""
deal with increment 7
deal into new stack
deal into new stack
""" is '(0 3 6 9 2 5 8 1 4 7)
 #:on $~nl"""
cut 6
deal with increment 7
deal into new stack
""" is '(3 0 7 4 1 8 5 2 9 6)
 #:on $~nl"""
deal with increment 7
deal with increment 9
cut -2
""" is '(6 3 0 7 4 1 8 5 2 9)
 #:on $~nl"""
deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1
""" is '(9 2 5 8 1 4 7 0 3 6))

(def-task task-2
  #:let pos-goal 2020
  #:let M 119315717514047
  #:let n 101741582076661
  #:let next-pos (next-pos (my-read))

  ;; next-pos is a linear function. That is,
  ;; next-pos(x) = (a * x) + b
  ;; so we want to find a an b

  #:let mat (matrix [[10 1] [20 1]])
  #:let v (col-matrix [(next-pos 10) (next-pos 20)])
  #:let u (matrix-solve mat v)
  #:let a (array-ref u (vector 0 0))
  #:let b (array-ref u (vector 1 0))

  ;; applying next-pos for n times on x has the following effect
  ;; n = 0: x (mod M)
  ;; n = 1: ax + b (mod M)
  ;; n = 2: a(ax+b)+b = a^2 x + ab + b (mod M)
  ;; n = 3: a(a^2 x + ab + b) + b = a^3 x + a^2 b + ab + b (mod M)
  ;; in general: a^n x + b (a^{n-1} + ... + a + 1) =
  ;; a^n x + b (a^n - 1) (a - 1)^{-1} (mod M)
  ;; Now, we want to find x such that
  ;; pos-goal = a^n x + b (a^n - 1) (a - 1)^{-1} (mod M)
  ;; x = (pos-goal - b (a^n - 1) (a - 1)^{-1}) (a^n)^{-1} (mod M)

  (modulo
   (* (- pos-goal
         (* b
            (sub1 (modular-expt a n M))
            (modular-inverse (sub1 a) M)))
      (modular-inverse (modular-expt a n M) M))
   M))
