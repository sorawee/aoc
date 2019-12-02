#lang racket

(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out racket) #%module-begin)
         tests
         task-1 task-2)

(require syntax/parse/define
         rackunit)

(define-simple-macro (tests xs ...)
  (module+ test (tests+ xs ...)))

(define-syntax-parser tests+
  [(_ A {~datum ==>} B xs ...)
   #`(begin #,(syntax/loc #'A (check-equal? A B)) (tests+ xs ...))]
  [(_ A xs ...) #'(begin A (tests+ xs ...))]
  [(_) #'(begin)])

(define-simple-macro (task-1 xs ...)
  (module+ task-1 xs ...))

(define-simple-macro (task-2 xs ...)
  (module+ task-2 xs ...))

(define-simple-macro (module-begin xs ...)
  (#%module-begin xs ...))
