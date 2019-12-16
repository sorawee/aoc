#lang aoc

(define base '(0 1 0 -1))

(def (amplify n)
  (for/stream ([x (in-cycle (for*/stream ([x (in-list base)]
                                          [i (in-range n)])
                              x))]
               [i (in-naturals)]
               #:unless (eq? i 0))
    x))

(def (string-> s)
  (for/list ([c (in-string s)]) (- (char->integer c) 48)))

(def (last-digit x)
  (abs (remainder x 10)))

(def/io my-read
  (string-> (read-line)))

(def ((run-task-1 r) xs)
  (for/fold ([xs xs] #:result (take xs 8)) ([_ (in-range r)])
    (for/list ([i (in-range (length xs))])
      (last-digit
       (for/sum ([x (amplify (add1 i))]
                 [i (in-list xs)])
         (* i x))))))

(def-task task-1 ((run-task-1 100) (my-read)))

(tests
 #:>> ((run-task-1 4) (my-read "12345678")) is '(0 1 0 2 9 4 9 8)
 #:fn (compose1 (run-task-1 100) my-read)
 #:on "80871224585914546619083218645595" is '(2 4 1 7 6 1 7 6)
 #:on "19617804207202209144916044189917" is '(7 3 7 4 5 4 1 8)
 #:on "69317163492948606335995924319873" is '(5 2 4 3 2 1 3 3))

(def ((run-task-2 r factor) xs)
  #:let offset (string->number (string-append* (map number->string (take xs 7))))
  #:let len (length xs)
  ;; the pattern in the second half is going to be
  ;; from 00000001111111 to 00000000000001
  ;; and we are in the second half.
  ;; This means to compute an element at index i,
  ;; it only needs to know elements after i
  ;; but not any other elements.
  #:let xs (append* (make-list factor xs))
  #:let LEN (length xs)
  (for/fold ([xs (drop xs offset)] #:result (take xs 8))
            ([_ (in-range r)])
    (for/foldr ([sum 0] [result '()] #:result result)
               ([x (in-list xs)])
      (def v (last-digit (+ sum x)))
      (values v (cons v result)))))

(def-task task-2 ((run-task-2 100 10000) (my-read)))

(tests
 #:fn (compose1 (run-task-2 100 10000) my-read)
 #:on "03036732577212944063491565474664" is '(8 4 4 6 2 0 2 6)
 #:on "02935109699940807407585447034323" is '(7 8 7 2 5 2 7 0)
 #:on "03081770884921959731165446850517" is '(5 3 5 5 3 7 3 1))
