#lang racket/base

(require racket/contract
         racket/file
         web-server/http/id-cookie
         pkg/private/stage)

(require "config.rkt"
         "monitor.rkt")

(provide
 (contract-out [initialize (apply and/c deprivilege/c (map grant-curator/c curation-administrators))]))

(define (initialize)
  (make-directory* root-path)

  (define secret-key
    (make-secret-salt/file
     (build-path root-path "secret.key")))

  (make-directory* users.new-path)

  (let ([client_id-path (build-path root-path "client_id")])
    (if (file-exists? client_id-path)
        (github-client_id (file->string client_id-path))
        (raise-user-error 'pkg-index "Cannot find file ~a" client_id-path)))
  (let ([client_secret-path (build-path root-path "client_secret")])
    (if (file-exists? client_secret-path)
        (github-client_secret (file->string client_secret-path))
        (raise-user-error 'pkg-index "Cannot find file ~a" client_secret-path)))

  (make-directory* cache-path)

  (make-directory* pkgs-path)

  (make-directory* static-path)

  (make-directory* (build-path static-path "pkg")))
