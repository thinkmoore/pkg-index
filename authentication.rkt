#lang racket/base

(require racket/file
         racket/contract
         (prefix-in bcrypt: bcrypt))

(require "config.rkt"
         "log.rkt"
         "pkgs.rkt"
         "monitor.rkt"
         "utils.rkt")

(provide
 (contract-out
  [authenticate (and/c
                 authority-closure/c
                 (->i ([operation symbol?]
                       [email string?]
                       [password string?]
                       [success (email) (->a () #:auth () (as-user/c email) any)]
                       [failure (-> symbol? any)])
                      [result any/c]))]
  [update-password (-> string? string? void)])
 authenticated-as curation-administrator?)

(define current-user (make-parameter #f))

(define (authenticated-as)
  (current-user))

(define (curation-administrator? email)
  (member email curation-administrators))

(define (check-password email given-password)
  (define password-path (extend-path users.new-path email))
  (cond
    [(not (file-exists? password-path)) 'new-user]
    [(let ([expected-password (file->bytes password-path)])
       (bcrypt:check expected-password (string->bytes/utf-8 given-password))) #t]
    [else 'bad-password]))

(define (check-author email pkg)
  (cond
    [(not (package-exists? pkg)) 'no-package]
    [(member email (package-authors (package-info pkg))) #t]
    [else 'not-author]))

(define (authenticate operation email given-password onSuccess onFailure)
  (let ([password-okay? (check-password email given-password)])
    (cond
      [(symbol? password-okay?) (onFailure password-okay?)]
      [else
       (parameterize ([current-user email])
         (onSuccess))])))

(define (update-password email new-password)
  (display-to-file (bcrypt:encode (string->bytes/utf-8 new-password))
                   (extend-path users.new-path email)
                   #:exists 'replace))
