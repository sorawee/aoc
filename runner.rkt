#lang racket

(require racket/cmdline
         net/url
         gregor
         gregor/period
         "./utils/utils.rkt")

;; today
;; NOTE: use Bermuna because it's one hour earlier than New York
(define the-today (today #:tz "Atlantic/Bermuda"))

;; dec-01 before today
(define current-dec-01
  (let ([guess-dec-01 (date (->year the-today) 12 1)])
    (if (date<? the-today guess-dec-01)
        (date (sub1 (->year the-today)) 12 1)
        guess-dec-01)))

(section "mutable section that sets *year* and *day*"

  ;; the contest year
  (define *year* #f)

  ;; the contest day
  (define *day* #f)

  (define mode
    (command-line
     #:once-each
     [("--day") day "The day" (set! *day* (string->number day))]
     [("--year") year "The year" (set! *year* (string->number year))]
     #:args (mode) mode))

  (unless *year*
    (set! *year* (->year current-dec-01))
    (printf "inferred year: ~a\n" *year*))

  (unless *day*
    ;; dec-01 that the user wants
    (define dec-01 (date *year* 12 1))
    (set! *day* (add1 (period-ref
                       (date-period-between dec-01 the-today '(days))
                       'days)))
    (printf "inferred day: ~a\n" *day*)))

(unless (<= 1 *day* 25)
  (raise-user-error 'runner "invalid day: ~a" *day*))

(define path (build-path (~a *year*)
                         (~a "day" (~a *day*
                                       #:width 2
                                       #:align 'right
                                       #:pad-string "0"))))

(match mode
  ["new"
   (cond
     [(directory-exists? path)
      (raise-user-error 'new "already exists: ~a" path)]
     [else (make-directory* path)
           (copy-file "./template/task.rkt" (build-path path "task.rkt"))])]
  [_ (void)])

(current-directory path)

(define (read-cookie)
  (define cookie-file "../../.cookie")
  (cond
    [(file-exists? cookie-file)
     (file->string cookie-file)]
    [else
     (printf "You have not provided a session id.\n")
     (printf "> ")
     (define cookie (read-line))
     (with-output-to-file cookie-file
       (thunk (display cookie)))
     (printf "session saved\n")
     cookie]))

(match mode
  ["new" (void)]
  ["test" (dynamic-require '(submod (file "task.rkt") test) #f)]
  ["download"
   (define cookie (read-cookie))
   (unless (file-exists? "input.txt")
     (with-output-to-file "input.txt"
       (thunk
        ((compose1
          display
          port->bytes
          (curryr get-pure-port (list (format "Cookie: session=~a" cookie)))
          string->url)
         (format "https://adventofcode.com/~a/day/~a/input" *year* *day*)))))]
  [_
   ((dynamic-require '(file "task.rkt") (string->symbol mode))
    (file->string "input.txt"))])
