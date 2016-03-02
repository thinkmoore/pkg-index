#lang racket/base

(require racket/contract
         racket/match
         racket/list
         web-server/http)

(provide
 forever
 (contract-out
  [path-element? (-> any/c boolean?)]
  [extend-path (-> (or/c path-for-some-system? path-string?)
                   (or/c path-element? path-element-string?)
                   path-for-some-system?)]
  [hash*-set (-> hash? list? any/c hash?)]
  [hash-deep-merge (-> hash? hash? hash?)]
  [response/sexpr (-> any/c response?)]))

(define (path-element? val)
  (and (path-for-some-system? val)
       (let-values ([(base name must-be-dir) (split-path val)])
         (and (eq? base 'relative)
              (not (or (eq? name 'up) (eq? name 'same)))))))

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

(define (response/sexpr v)
  (response 200 #"Okay" (current-seconds)
            #"text/s-expr" empty
            (λ (op) (write v op))))

(module* test racket/base
  (require rackunit)
  (require (submod ".."))
  (check-not-exn (λ () (extend-path "hello" "world")))
  (check-exn exn:fail:contract? (λ () (extend-path "hello" "../world")))
  (check-exn exn:fail:contract? (λ () (extend-path "hello" ".."))))