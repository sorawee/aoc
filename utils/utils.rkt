#lang racket

(provide section)
(require syntax/parse/define)

(define-simple-macro (section desc:str xs ...)
  (begin xs ...))
