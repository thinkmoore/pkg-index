#lang racket/base

(require racket/contract
         racket/file
         racket/match
         racket/list
         racket/string
         version/utils)

(require "config.rkt"
         "utils.rkt"
         "validate.rkt"
         "monitor.rkt")

(provide
 (contract-out
  [package-list (-> (listof valid-name?))]
  [packages-of (-> valid-author? (listof valid-name?))]
  [package-info (-> valid-name? (or/c package-info/c #f))]
  [new-package-info (->a ([name valid-name?]
                          [source string?]
                          [author valid-author?]
                          [desc string?]
                          [last-edit number?])
                         #:auth (author) (is-author/c (hash 'author author))
                         [result package-info/c])]
  [package-info-set! (->a ([pkg package-info/c])
                          #:auth (pkg) (is-author/c pkg)
                          [result void])]
  [package-curate! (->a ([pkg package-info/c]
                         [ring number?])
                        #:auth () is-curator/c
                        [result void])]
  [package-ref (-> package-info/c package-info-key? any/c)]
  [package-author? (-> package-info/c valid-author? boolean?)]
  [package-authors (-> package-info/c (listof valid-author?))]
  [package-tags-normalize (-> (listof valid-tag?) (listof valid-tag?))]
  [package-remove! (->a ([pkg-name valid-name?])
                        #:auth (pkg-name) (is-author/c (package-info pkg-name))
                        [result void])]
  [package-exists? (-> valid-name? boolean?)]))

(define (package-list)
  (sort (map path->string (directory-list pkgs-path))
        string-ci<=?))

(define (packages-of u)
  (filter (λ (p) (package-author? p u)) (package-list)))

(define (package-remove! pkg-name)
  (delete-file (extend-path pkgs-path pkg-name)))

(define (package-exists? pkg-name)
  (file-exists? (extend-path pkgs-path pkg-name)))

(define package-info-key-contracts
  (hash 'name valid-name?
        'author valid-author?
        'authors (listof valid-author?)
        'source any/c
        'description (or/c string? #f)
        'dependencies any/c
        'modules any/c
        'conflicts any/c
        'build any/c
        'search-terms any/c
        'ring (integer-in 0 2)
        'checksum (or/c string? #f)
        'checksum-error (or/c string? #f)
        'tags (listof valid-tag?)
        'versions any/c
        'last-checked number?
        'last-edit number?
        'last-updated number?))

(define package-info-keys
  (hash-keys package-info-key-contracts))

(define package-info-required-keys
  (list 'author 'source 'name))

(define (package-info-key? v)
  (member v package-info-keys))

(define (apply-key-contract key value blame)
  (let ([ctc (hash-ref package-info-key-contracts key)])
    (((contract-late-neg-projection ctc) blame)
     value (blame-negative blame))))

(define package-info/c
  (make-chaperone-contract
   #:name "package-info/c"
   #:first-order
   (λ (v)
     (and (hash? v)
         (hash-has-key? v 'author)
         (hash-has-key? v 'source)))
   #:late-neg-projection
   (λ (blame)
     (λ (val neg)
       (let ([blame (blame-replace-negative blame neg)])
         (unless (hash? val)
           (raise-blame-error
            blame val
            '(expected: "~a" given: "~e")
            'package-info/c val))
            package-info-required-keys)
         (chaperone-hash
          val
          (λ (h k)
            (unless (package-info-key? k)
              (raise-blame-error
               (blame-swap blame) val
               '(expected: "~a" given: "~e")
               (format "a member of ~a" package-info-keys) k))
            (values k (λ (h k v) (apply-key-contract k v blame))))
          (λ (h k v)
            (unless (package-info-key? k)
              (raise-blame-error
               (blame-swap blame) val
               '(expected: "~a" given: "~e")
               (format "a member of ~a" package-info-keys) k))
            (values k (apply-key-contract k v (blame-swap blame))))
          (λ (h k)
            (when (member k package-info-required-keys)
              (raise-blame-error
               (blame-swap blame) val
               "~n  Cannot remove required field: ~e" k))
            k)
          (λ (h k) k))))))

(define (package-ref pkg-info key)
  (hash-ref pkg-info key
            (λ ()
              (match key
                ['checksum ""]
                ['ring 2]
                ['checksum-error #f]
                ['tags empty]
                ['versions (hash)]
                [(or 'last-checked 'last-edit 'last-updated) -inf.0]))))

(define (package-authors pkg-info)
  (let ([authors (hash-ref pkg-info 'author "")])
    (string-split authors)))

(define (package-author? pkg-info author)
  (if (member author (package-authors pkg-info))
      #t
      #f))

(define (package-tags-normalize ts)
  (remove-duplicates (sort ts string-ci<?)))

(define (read-package-info pkg-name)
  (with-handlers ([exn:fail?
                   (λ (x)
                     ((error-display-handler)
                      (exn-message x)
                      x)
                     (hasheq))])
    (define p
      (extend-path pkgs-path pkg-name))
    (define v
      (if (file-exists? p)
          (file->value p)
          (hasheq)))
    (define ht
      (if (hash? v)
          v
          (hasheq)))
    ht))

(define (package-info pkg-name #:version [version #f])
  (define ht (read-package-info pkg-name))
  (define no-version
    (hash-set ht 'name pkg-name))
  (cond
   [(and version
         (hash-has-key? no-version 'versions)
         (hash? (hash-ref no-version 'versions #f))
         (hash-has-key? (hash-ref no-version 'versions) version)
         (hash? (hash-ref (hash-ref no-version 'versions) version #f)))
    =>
    (λ (version-ht)
      (hash-merge version-ht no-version))]
   [else
    no-version]))

(define (new-package-info name source author desc last-edit)
  (let ([pkg-info (hash 'name name
                        'source source
                        'author author
                        'description desc
                        'last-edit last-edit)])
    (if (package-exists? name)
        #f
        (begin
          (package-info-set! pkg-info)
          pkg-info))))

(define (package-info-set! pkg-info)
  (write-to-file pkg-info (extend-path pkgs-path (package-ref pkg-info 'name))
                 #:exists 'replace))

(define (package-curate! pkg-info ring)
  (package-info-set! (hash-set pkg-info 'ring ring)))

(define (hash-merge from to)
  (for/fold ([to to])
            ([(k v) (in-hash from)])
    (hash-set to k v)))
