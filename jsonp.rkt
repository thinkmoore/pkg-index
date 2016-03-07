#lang racket/base

(require racket/match
         web-server/http
         json)

(require (for-syntax racket/base))

(require "utils.rkt")

(provide define-jsonp
         make-jsonp-responder)

(define (response/jsonp callback o)
  (response/output
   (λ (op)
     (fprintf op "~a(" callback)
     (write-json o op)
     (fprintf op ");"))
   #:mime-type #"application/javascript"))

(define (request-jsonp-data req)
  (for/fold ([ht (hasheq)])
            ([b (in-list (request-bindings/raw req))])
    (match b
      [(binding:form id value)
       (define p (map string->symbol (parse-jsonp-path (bytes->string/utf-8 id))))
       (hash*-set ht p
                  (bytes->string/utf-8 value))]
      [_ ht])))

(define parse-jsonp-path
  (match-lambda
   [(regexp #rx"^([^[]+)\\[([^]]+)\\](.*)$"
            (list _ fst snd rst))
    (list* fst (parse-jsonp-path (format "~a~a" snd rst)))]
   [s
    (list s)]))

(define (make-jsonp-responder f)
  (λ (req)
    (define og (request-jsonp-data req))
    (response/jsonp
     (hash-ref og 'callback)
     (f (hash-remove (hash-remove og 'callback) '_)))))

(define-match-expander hash-entries
  (lambda (stx)
    (syntax-case stx ()
      [(_ [key val] ...)
       #'(and (app (λ (m) (hash-ref m key)) val)
              ...)])))

(define-syntax (define-jsonp stx)
  (syntax-case stx ()
    [(_ (f pat ...) body0 body ...)
     (quasisyntax/loc stx
       (define f
         (make-jsonp-responder
          (match-lambda [(hash-entries pat ...) (begin body0 body ...)]
                        [x (error 'f "ill formatted request: ~v~ntried to match: ~v" x
                                  '#,(syntax->datum #'(pat ...)))]))))]))