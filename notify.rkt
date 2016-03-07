#lang racket/base

(require racket/file
         racket/list)

(require "config.rkt"
         "log.rkt")

(provide notify! get-notices)

(define (get-notices)
  (file->string notice-path))

(define (upload-notice! m)
  (display-to-file m notice-path #:exists 'replace))

(define (do-notify! l)
  (upload-notice! (first l)))

(define (notify! m)
  (run! do-notify! (list m)))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "notify"
   #:args (message)
   (upload-notice! message)))
