#lang racket

(provide tests
         define-task
         debug:
         #%app
         ~)

(require syntax/parse/define
         rackunit
         fancy-app)

(define-simple-macro (tests xs ...)
  (module+ test (tests+ xs ...)))

(begin-for-syntax
  (define-splicing-syntax-class input-clause
    (pattern
     {~seq #:on left:expr {~datum is} right:expr}
     #:with result (syntax/loc #'left (check-equal? (left the-input) right)))
    (pattern
     {~seq #:on left:expr {~datum satisfies} right:expr}
     #:with result (syntax/loc #'left (check-pred right (left the-input)))))

  (define-splicing-syntax-class check-clause
    (pattern
     {~seq #:>> left:expr {~datum is} right:expr}
     #:with result (syntax/loc #'left (check-equal? left right)))
    (pattern
     {~seq #:>> left:expr {~datum satisfies} right:expr}
     #:with result (syntax/loc #'left (check-pred right left)))))

(define-syntax-parser tests+
  [(_ #:with e:expr :input-clause ... xs ...)
   #`(begin
       (let ([v e]) (let ([the-input v]) result) ...)
       (tests+ xs ...))]
  [(_ :check-clause xs ...)
   #`(begin result (tests+ xs ...))]
  [(_) #'(begin)])

(tests
 #:with "abc"
 #:on string-length is 3
 #:on identity satisfies non-empty-string?

 #:>> (add1 1) is 2
 #:>> (add1 0) satisfies positive?

 #:with 5
 #:on add1 is 6
 #:on sub1 is 4)

(define-simple-macro (define-task task-name:id xs ...)
  (begin
    (provide task-name)
    (define (task-name s)
      (with-input-from-string s (thunk xs ...)))))

(define-simple-macro (debug: e)
  (debug:core 'e e))

(define (debug:core repr v)
  (printf "~a:\n" repr)
  (for ([line (in-list (string-split (pretty-format v) "\n"))])
    (printf "    ~a\n" line))
  (printf "\n")
  v)


(define (~ . xs) (string-join xs "\n"))
