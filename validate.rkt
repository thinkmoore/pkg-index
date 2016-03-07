#lang racket/base

(provide valid-name? valid-author? valid-tag?)

(define (valid-name? t)
  (not (regexp-match #rx"[^a-zA-Z0-9_\\-]" t)))

(define (valid-author? a)
  (not (regexp-match #rx"[ :]" a)))

(define valid-tag?
  valid-name?)