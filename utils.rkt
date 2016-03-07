#lang racket/base

(require racket/contract
         racket/match
         racket/list
         racket/file
         racket/date
         racket/path
         web-server/http
         net/url)

(provide
 forever
 safe-run!
 hash-ref-or
 file->value*
 format-time
 package-url->useful-url
 string-min
 string-max
 convert-to-json
 (contract-out
  [extend-path (-> (or/c path-for-some-system? path-string?)
                   (or/c path-element? path-element-string?)
                   path-for-some-system?)]
  [hash*-set (-> hash? list? any/c hash?)]
  [hash-deep-merge (-> hash? hash? hash?)]
  [response/sexpr (-> any/c response?)]))

(define (string-min x y)
  (if (string<=? x y)
      x
      y))

(define (string-max x y)
  (if (string<? x y)
      y
      x))

(define (path-element-string? val)
  (and (path-string? val) (path-element? (string->path-element val))))

(define (extend-path base path)
  (build-path base path))

(define-syntax-rule (forever . body)
  (let loop () (begin . body) (loop)))

(define (hash*-set ht p v)
  (match p
    [(list k)
     (hash-set ht k v)]
    [(list-rest f r)
     (hash-update ht f (λ (fht) (hash*-set fht r v)) (hasheq))]))

(define (hash-deep-merge ht more-ht)
  (for/fold ([ht ht])
            ([(k new-v) (in-hash more-ht)])
    (hash-update ht k
                 (λ (old-v)
                   (cond
                     [(not old-v) new-v]
                     [(hash? old-v) (hash-deep-merge old-v new-v)]
                     [else new-v]))
                 #f)))

(define (hash-ref-or ht ks)
      (or (for/or ([k (in-list ks)])
            (hash-ref ht k #f))
          (error 'hash-ref-or "Keys (~v) not found in ~e" ks ht)))

(define (response/sexpr v)
  (response 200 #"Okay" (current-seconds)
            #"text/s-expr" empty
            (λ (op) (write v op))))

(define (safe-run! run-sema t)
  (thread
   (λ ()
     (call-with-semaphore run-sema
       (λ ()
         (with-handlers ([exn:fail? (λ (x) ((error-display-handler)
                                            (exn-message x)
                                            x))])
           (t)))))))

(define (file->value* path default)
  (if (file-exists? path)
      (file->value path)
      default))

(define (format-time s)
  (if s
      (with-handlers ([exn:fail? (λ (x) "")])
        (parameterize ([date-display-format 'iso-8601])
          (date->string (seconds->date s #f) #t)))
      ""))

(define (package-url->useful-url pkg-url-str)
  (let ([pkg-url (string->url pkg-url-str)])
    (match (url-scheme pkg-url)
      ["github"
       (match (url-path pkg-url)
         [(list* user repo branch path)
          (url->string
           (struct-copy
            url pkg-url
            [scheme "http"]
            [path (list* user repo (path/param "tree" empty) branch path)]))]
         [_ pkg-url-str])]
      ["git"
       (match (url-path pkg-url)
         ;; xxx make this more robust
         [(list user repo)
          (url->string
           (struct-copy
            url pkg-url
            [scheme "http"]
            [path (list user repo (path/param "tree" empty)
                        (path/param "master" empty))]))]
         [_ pkg-url-str])]
      [_ pkg-url-str])))

(define convert-to-json
  (match-lambda
   [(? list? l)
    (map convert-to-json l)]
   [(? string? s)
    s]
   [(? hash? ht)
    (for/hash ([(k v) (in-hash ht)])
      (values (convert-to-json-key k)
              (convert-to-json v)))]
   [(? number? n)
    n]
   [(? boolean? b)
    b]
   [(? symbol? s)
    (symbol->string s)]
   [(? keyword? s)
    (hasheq 'kw (keyword->string s))]
   [x
    (error 'convert-to-json "~e" x)]))
(define convert-to-json-key
  (match-lambda
   [(? string? s)
    (string->symbol s)]
   [(? symbol? s)
    s]
   [x
    (error 'convert-to-json-key "~e" x)]))

(module* test racket/base
  (require rackunit)
  (require (submod ".."))
  (check-not-exn (λ () (extend-path "hello" "world")))
  (check-exn exn:fail:contract? (λ () (extend-path "hello" "../world")))
  (check-exn exn:fail:contract? (λ () (extend-path "hello" ".."))))