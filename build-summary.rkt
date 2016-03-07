#lang racket/base

(require "config.rkt"
         "utils.rkt")

(provide build-summary)

(define (build-summary) (file->value* summary-path (hash)))
