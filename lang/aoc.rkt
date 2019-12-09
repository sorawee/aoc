#lang racket

(provide (all-from-out racket)
         def
         tests
         define-task
         debug:
         #%app
         ~nl
         ~bl
         for/max
         for/min)

(require syntax/parse/define
         rackunit
         fancy-app
         (for-syntax racket/syntax))

(define-simple-macro (def (name+arg ...) xs ...) (define (name+arg ...) (def+ xs ...)))

(define-syntax-parser def+
  [(_ #:head (form ...) xs ...)
   #'(form ... (def+ xs ...))]
  [(_ x xs ...) #'(begin x (def+ xs ...))]
  [(_) #'(begin)])

(define-simple-macro (tests {~optional {~seq #:name s:str}} xs ...)
  (module+ test ({~? {~@ test-case s} test-begin} (tests+ xs ...))))

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
  [(_ #:in e:expr {~seq #:on c:check-clause} ... xs ...)
   #:with v (generate-temporary 'v)
   #:with (left ...) (map (λ (e) (quasisyntax/loc e (#,e v)))
                          (attribute c.left))
   #`(begin
       (let ([v e]) (tests+ {~@ #:>> left . c.residue}) ...)
       (tests+ xs ...))]
  [(_ #:fn e:expr {~seq #:on c:check-clause} ... xs ...)
   #:with v (generate-temporary 'v)
   #:with (left ...) (map (λ (e) (quasisyntax/loc e (v #,e)))
                          (attribute c.left))
   #`(begin
       (let ([v e]) (tests+ {~@ #:>> left . c.residue}) ...)
       (tests+ xs ...))]
  [(_ #:>> :check-clause xs ...) #`(begin result (tests+ xs ...))]
  [(_ #:let var:id e:expr xs ...) #'(let ([var e]) (tests+ xs ...))]
  [(_ #:do e:expr xs ...) #'(begin e (tests+ xs ...))]
  [(_ e:expr xs ...) #`(begin e (tests+ xs ...))]
  [(_) #'(begin)])

(tests
 #:in "abc"
 #:on string-length is 3
 #:on identity satisfies non-empty-string?

 #:>> (add1 1) is 2
 #:>> (add1 0) satisfies positive?

 #:in 5
 #:on add1 is 6
 #:on sub1 is 4

 #:let hello (- 2 1)
 #:>> hello is (+ 0 1))

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
  (fprintf (current-error-port) "~a:\n" repr)
  (for ([line (in-list (string-split (pretty-format v) "\n"))])
    (fprintf (current-error-port) "    ~a\n" line))
  (fprintf (current-error-port) "\n")
  v)

(define ~nl string-trim)
(define ~bl (string-replace _ "\n" ""))

(define-syntax-parser for/max
  [(_ clauses body ... tail-expr)
   #:with orig this-syntax
   #'(for/fold/derived original
       ([current-max -inf.0])
       clauses
       body ...
       (define maybe-new-max tail-expr)
       (if (> maybe-new-max current-max)
           maybe-new-max
           current-max))])

(define-syntax-parser for/min
  [(_ clauses body ... tail-expr)
   #:with orig this-syntax
   #'(for/fold/derived original
       ([current-min +inf.0])
       clauses
       body ...
       (define maybe-new-min tail-expr)
       (if (< maybe-new-min current-min)
           maybe-new-min
           current-min))])
