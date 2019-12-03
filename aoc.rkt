#lang racket

(provide tests
         define-task
         debug:)

(require syntax/parse/define
         rackunit
         (for-syntax racket/syntax))

(define-simple-macro (tests xs ...)
  (module+ test (tests+ xs ...)))

(define-syntax-parser tests+
  [(_ A {~datum ==>} B xs ...)
   #`(begin #,(syntax/loc #'A (check-equal? A B)) (tests+ xs ...))]
  [(_ #:because xs ...) #'(tests+ xs ...)]
  [(_ A xs ...) #'(begin A (tests+ xs ...))]
  [(_) #'(begin)])

(define-simple-macro (define-task task-name:id xs ...)
  (begin
    (provide task-name)
    (define (task-name [s #f])
      (define (proc) xs ...)
      (cond
        [s (with-input-from-string s proc)]
        [else (proc)]))))

(define-simple-macro (debug: e)
  (debug:core 'e e))

(define (debug:core repr v)
  (printf "~a:\n" repr)
  (for ([line (in-list (string-split (pretty-format v) "\n"))])
    (printf "    ~a\n" line))
  (printf "\n")
  v)
