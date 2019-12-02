#lang racket

(require racket/cmdline)

(define the-day
  (for*/fold ([result 0] #:result (number->string result))
             ([dir (directory-list)]
              [day (in-value (string->number (path->string dir)))]
              #:when (directory-exists? dir)
              #:when (number? day))
    (max result day)))

(define mode
  (command-line
   #:once-each
   [("--day") day "The day" (set! the-day day)]
   #:args (mode) mode))

(define (runner)
  (dynamic-require `(submod (file "task.rkt") ,(string->symbol mode)) #f))

(current-directory the-day)

(match mode
  [(or "task-1" "task-2") (with-input-from-file "input.txt" runner)]
  ["test" (runner)])
