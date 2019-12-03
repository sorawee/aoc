#lang racket

(require racket/cmdline
         net/url)

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

(current-directory the-day)

(match mode
  ["test" (dynamic-require '(submod (file "task.rkt") test) #f)]
  ["download"
   (define cookie (file->string "../.cookie"))
   (unless (file-exists? "input.txt")
     (with-output-to-file "input.txt"
       (thunk
        ((compose1
          display
          port->bytes
          (curryr get-pure-port (list (format "Cookie: session=~a" cookie)))
          string->url)
         (format "https://adventofcode.com/2019/day/~a/input" the-day)))))]
  [_
   (with-input-from-file "input.txt"
     (dynamic-require '(file "task.rkt") (string->symbol mode)))])
