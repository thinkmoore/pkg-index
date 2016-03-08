#lang racket/base

(require racket/list
         racket/function
         racket/system
         racket/package
         pkg/private/stage
         (prefix-in pkg: pkg/lib))

(require "config.rkt"
         "log.rkt"
         "notify.rkt"
         "pkgs.rkt"
         "static.rkt"
         "utils.rkt")

(define (update-all)
  (update-checksums #f (package-list)))
(define (update-pkgs pkgs)
  (update-checksums #t pkgs))

(define (update-checksums force? pkgs)
  (filter (curry update-checksum force?) pkgs))

(define (update-checksum force? pkg-name)
  (log! "update-checksum ~v ~v" force? pkg-name)
  (with-handlers
      ([exn:fail?
        (λ (x)
          (with-handlers
              ([exn:fail?
                (λ (x2)
                  ((error-display-handler)
                   (format 
                    "second error (~v) while catching error (~v) while updating (~v)"
                    (exn-message x2)
                    (exn-message x)
                    pkg-name)
                   x2))])
            (define i (package-info pkg-name))
            (package-info-set!
             (hash-set i 'checksum-error
                       (regexp-replace*
                        (regexp (github-client_secret))

                        (let ([the-string-port (open-output-string)])
                          (parameterize ([current-error-port the-string-port])
                            ((error-display-handler)
                             (exn-message x)
                             x))
                          (get-output-string the-string-port))
                        
                        "REDACTED"))))
          #t)])
    (define i (package-info pkg-name))
    (define old-checksum (package-ref i 'checksum))
    (define now (current-seconds))
    (define last (hash-ref i 'last-checked -inf.0))
    (define changed? #f)
    (when (or force?
              (>= (- now last) (* 1 60 60))
              (not old-checksum))
      (log! "\tupdating ~a" pkg-name)
      (define new-checksum
        (package-url->checksum
         (package-ref i 'source)
         #:pkg-name pkg-name))
      (unless (equal? new-checksum old-checksum)
        (log! "\told: ~v" old-checksum)
        (log! "\tnew: ~v" new-checksum)
        (set! changed? #t))
      (package-begin
       (define* i
         (hash-set i 'checksum
                   (or new-checksum
                       old-checksum)))
       (define* i
         (hash-set i 'last-checked now))
       (define* i
         (hash-update i 'versions
                      (λ (v-ht)
                        (for/hash ([(v vi) (in-hash v-ht)])
                          (define old-checksum (hash-ref vi 'checksum ""))
                          (define new-checksum
                            (package-url->checksum
                             (hash-ref vi 'source "")
                             #:pkg-name pkg-name))
                          (unless (equal? new-checksum old-checksum)
                            (log! "\t~a old: ~v" vi old-checksum)
                            (log! "\t~a new: ~v" vi new-checksum)
                            (set! changed? #t))
                          (values v
                                  (hash-set vi 'checksum
                                            (or new-checksum
                                                old-checksum)))))
                      hash))
       (define* i
         (cond
           [(not new-checksum)
            i]
           [(and (equal? new-checksum old-checksum)
                 ;; update if 'modules was not present:
                 (hash-ref i 'modules #f))
            i]
           [else
            (hash-set (update-from-content i) 'last-updated now)]))
       (define* i
         (hash-set i 'checksum-error #f))
       (log! "\twriting with checksum ~v" (hash-ref i 'checksum))
       (package-info-set! i)))
    changed?))

(define (update-from-content i)
  (log! "\tgetting package content for ~v" (hash-ref i 'name))
  (define-values (checksum module-paths dependencies)
    (pkg:get-pkg-content (pkg:pkg-desc (hash-ref i 'source)
                                       #f
                                       (hash-ref i 'name)
                                       (hash-ref i 'checksum)
                                       #f)))
  (package-begin
   (define* i (hash-set i 'modules module-paths))
   (define* i (hash-set i 'dependencies dependencies))
   i))

(define (do-update! pkgs)
  (notify! "package sources being checked for updates")
  (log! "update: checking ~v" pkgs)
  (define changed
    (cond
     [(empty? pkgs)
      (update-all)]
     [else
      (update-pkgs pkgs)]))
  (log! "update: changes ~v" changed)
  (signal-static! changed))
(define (run-update! pkgs)
  (run! do-update! pkgs))
(define run-sema (make-semaphore 1))
(define (signal-update! pkgs)
  (safe-run! run-sema (λ () (run-update! pkgs))))

(provide do-update!
         (contract-out
          [signal-update! authority-closure/c]))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "update"
   #:args pkgs
   (do-update! pkgs)))
