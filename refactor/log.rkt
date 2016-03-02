#lang racket/base

(require racket/date)

(provide log! run!)

(define (log! . args)
  (parameterize ([date-display-format 'iso-8601])
    (printf "~a: ~a\n" (date->string (current-date) #t)
            (apply format args))))

(define (run! f args)
  (log! "START ~a ~v" f args)
  (f args)
  (log! "END ~a ~v" f args))