#lang racket

(provide tests
         define-task
         debug:
         #%app
         ~)

(require syntax/parse/define
         rackunit
         fancy-app
         (for-syntax racket/syntax))

(define-simple-macro (tests xs ...)
  (module+ test (tests+ xs ...)))

(begin-for-syntax
  (define-splicing-syntax-class check-clause
    (pattern
     {~seq left:expr {~and op {~datum is}} right:expr}
     #:with result (syntax/loc #'left (check-equal? left right))
     #:with residue #'(op right))
    (pattern
     {~seq left:expr {~and op {~datum satisfies}} right:expr}
     #:with result (syntax/loc #'left (check-pred right left))
     #:with residue #'(op right))
    (pattern
     {~seq left:expr {~and op {~datum does-not-satisfies}} right:expr}
     #:with result (syntax/loc #'left (check-pred (negate right) left))
     #:with residue #'(op right))))

(define-syntax-parser tests+
  [(_ #:with e:expr {~seq #:on c:check-clause} ... xs ...)
   #:with v (generate-temporary 'v)
   #:with (left ...) (map (λ (e) (quasisyntax/loc e (#,e v)))
                          (attribute c.left))
   #`(begin
       (let ([v e]) (tests+ {~@ #:>> left . c.residue}) ...)
       (tests+ xs ...))]
  [(_ #:>> :check-clause xs ...) #`(begin result (tests+ xs ...))]
  [(_ e:expr xs ...) #`(begin e (tests+ xs ...))]
  [(_) #'(begin)])

(tests
 #:with "abc"
 #:on string-length is 3
 #:on identity satisfies non-empty-string?

 #:>> (add1 1) is 2
 #:>> (add1 0) satisfies positive?

 #:with 5
 #:on add1 is 6
 #:on sub1 is 4

 (define hello 1)
 #:>> hello is 1)

(begin-for-syntax
  (define-splicing-syntax-class task-name
    (pattern {~seq name:id #:name subname:id}
             #:with the-name (format-id #'name "~a:~a" #'name #'subname))
    (pattern {~seq name:id} #:with the-name #'name)))

(define-simple-macro (define-task :task-name xs ...)
  (begin
    (provide the-name)
    (define (the-name s)
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
