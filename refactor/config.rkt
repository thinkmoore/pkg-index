#lang racket/base

(require racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path src-path ".")
(define-runtime-path root-path "root")
(define users.new-path (build-path root-path "users.new"))
(define cache-path (build-path root-path "cache"))
(define summary-path (build-path cache-path "summary.rktd"))
(define pkgs-path (build-path root-path "pkgs"))
(define static.src-path (build-path src-path "static"))
(define static-path (build-path src-path "static-gen"))
(define notice-path (build-path static-path "notice.json"))

(define ssl-cert-path (build-path root-path "server-cert.pem"))
(define ssl-key-path (build-path root-path "private-key.pem"))

(define s3-config
  (build-path (find-system-path 'home-dir) ".s3cfg-plt"))
(define s3-bucket "pkgs.racket-lang.org")
(define s3cmd-path (find-executable-path "s3cmd"))

(define curation-adminstrators
  '("jay.mccarthy@gmail.com"
    "mflatt@cs.utah.edu"
    "samth@ccs.neu.edu"
    "stamourv@racket-lang.org"))
