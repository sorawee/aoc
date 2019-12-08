#lang racket

(module reader syntax/module-reader
  aoc/lang/aoc
  #:read $-read
  #:read-syntax $-read-syntax

  (require racket/match
           syntax/readerr)

  (define ($-read in)
    (parameterize ([current-readtable (make-$-readtable)])
      (read in)))

  (define ($-read-syntax src in)
    (parameterize ([current-readtable (make-$-readtable)])
      (read-syntax src in)))

  (define (make-$-readtable)
    (make-readtable (current-readtable)
                    #\$ 'terminating-macro read-dollar))

  (define read-dollar
    (case-lambda
      [(ch in) (my:read in)]
      [(ch in src line col pos) (my:read-syntax src in line col pos)]))

  (define (my:read in)
    (match (regexp-match #px"^ *([^\" ]*) *\"\"\"(.*?)\"\"\"" in)
      [(list _ f s) (list (string->symbol (bytes->string/utf-8 f))
                          (bytes->string/utf-8 s))]
      [_ (define ch (peek-char in))
         (define-values (line col pos) (port-next-location in))
         ((if (eof-object? ch) raise-read-eof-error raise-read-error)
          "unexpected input"
          (object-name in) line col pos
          (if (eof-object? ch) 0 1))]))

  (define (my:read-syntax src in line col pos)
    (datum->syntax #f (my:read in) (list src line col pos #f))))
