#lang racket/base

(require racket/match
         racket/file
         racket/port
         racket/list
         net/http-client)

(require "config.rkt"
         "log.rkt"
         "notify.rkt"
         "utils.rkt")

(provide do-build-update!
         signal-build-update!)

(define SUMMARY-HOST "pkg-build.racket-lang.org")
(define SUMMARY-URL "/summary.rktd")
(define SUMMARY-ETAG-PATH (string->path (format "~a.etag" (path->string summary-path))))

(define (extract-tag hs)
  (or
   (for/or ([h (in-list hs)])
     (match h
       [(regexp
         #rx#"^ETag: (.*?)$"
         (list _ tag-bys))
        tag-bys]
       [_
        #f]))
   #""))

(define (file->bytes* p d)
  (if (file-exists? p)
    (file->bytes p)
    d))

(define (build-update!)
  (define cur-version
    (file->bytes* SUMMARY-ETAG-PATH #""))
  (log! "build-update: Current: ~v\n" cur-version)
  (printf "host: ~a~nurl: ~a~n" SUMMARY-HOST SUMMARY-URL)
  (define-values
    (_0 head-headers _1)
    (http-sendrecv
     SUMMARY-HOST SUMMARY-URL
     #:method #"HEAD"))
  (define head-version
    (extract-tag head-headers))
  (log! "build-update: Head: ~v\n" head-version)

  (unless (bytes=? cur-version head-version)
    (define-values
      (_2 get-headers get-ip)
      (http-sendrecv
       SUMMARY-HOST SUMMARY-URL
       #:method #"GET"))
    (define get-version
      (extract-tag get-headers))
    (log! "build-update: Get: ~v\n" get-version)

    (define new-file
      (make-temporary-file "summary-~a.rktd" #f cache-path))
    (call-with-output-file new-file
      #:exists 'truncate/replace
      (λ (new-op)
        (copy-port get-ip new-op)))

    (with-output-to-file SUMMARY-ETAG-PATH
      #:exists 'truncate/replace
      (λ ()
        (write-bytes get-version)))

    (rename-file-or-directory new-file summary-path #t)))

(define (do-build-update! l)
  (notify! "package build status being checked for updates")
  (build-update!)
  (notify! ""))
(define (run-build-update!)
  (run! do-build-update! empty))
(define run-sema (make-semaphore 1))
(define (signal-build-update!)
  (safe-run! run-sema (λ () (run-build-update!))))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "build-update"
   #:args ()
   (do-build-update! empty)))
