#lang racket/base

(require racket/file
         (prefix-in bcrypt: bcrypt))

(require "config.rkt"
         "log.rkt"
         "pkgs.rkt"
         "utils.rkt")

(provide authenticate authenticated-as update-password curation-administrator?)

(define current-user (make-parameter #f))

(define (authenticated-as)
  (current-user))

(define (curation-administrator? email)
  (member email curation-adminstrators))

(define (check-password email given-password)
  (define password-path (extend-path users.new-path email))
  (cond
    [(not (file-exists? password-path)) 'new-user]
    [(let ([expected-password (file->bytes password-path)])
       (bcrypt:check expected-password given-password)) #t]
    [else 'bad-password]))

(define (check-author email pkg)
  (cond
    [(not (package-exists? pkg)) 'no-package]
    [(member email (package-authors (package-info pkg))) #t]
    [else 'not-author]))

(define (authenticate operation
                      #:as-curator [as-curator #f]
                      #:as-author  [pkg #f]
                      #:email email
                      #:password given-password
                      #:on-success onSuccess
                      #:on-failure onFailure)
  (let ([password-okay? (check-password email given-password)]
        [curator-okay?  (or (not as-curator) (curation-administrator? email))]
        [author-okay?   (or (not pkg) (check-author email pkg))])
    (cond
      [(symbol? password-okay?) (onFailure password-okay?)]
      [(symbol? curator-okay?)  (onFailure curator-okay?)]
      [(symbol? author-okay?)   (onFailure author-okay?)]
      [else
       (parameterize ([current-user email])
         (onSuccess))])))

(define (update-password email new-password)
  (display-to-file (bcrypt:encode (string->bytes/utf-8 new-password))
                   (extend-path users.new-path email)
                   #:exists 'replace))