#lang racket

(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out racket) #%module-begin)
         tests
         define-task)

(require syntax/parse/define
         rackunit
         (for-syntax racket/syntax))

(define-simple-macro (tests xs ...)
  (module+ test (tests+ xs ...)))

(define-syntax-parser tests+
  [(_ A {~datum ==>} B xs ...)
   #`(begin #,(syntax/loc #'A (check-equal? A B)) (tests+ xs ...))]
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

(define-simple-macro (module-begin xs ...)
  (#%module-begin xs ...))
