#lang racket/base

(require racket/list
         racket/match
         racket/file
         racket/string
         web-server/http
         web-server/servlet-env
         web-server/dispatch
         net/url)

(require (for-syntax racket/base))

(require "config.rkt"
         "log.rkt"
         "jsonp.rkt"
         "notify.rkt"
         "authentication.rkt"
         "pkgs.rkt"
         "static.rkt"
         "build-update.rkt"
         "update.rkt"
         "utils.rkt")

(define (send-mail-message from	 	 	 	 
                           subject	 	 	 	 
                           to	 	 	 	 
                           cc	 	 	 	 
                           bcc	 	 	 	 
                           body)
  (log! "Would have sent email:")
  (log! "from: ~a" from)
  (log! "subject: ~a" subject)
  (log! "to: ~a" to)
  (log! "cc: ~a" cc)
  (log! "bcc: ~a" cc)
  (for-each log! body)
  (log! "EOF"))

(define-syntax (define-jsonp/auth stx)
  (syntax-case stx ()
    [(_ (f pat ...) body0 body ...)
     (quasisyntax/loc stx
       (define-jsonp (f ['email email] ['passwd passwd] pat ...)
         (log! "~a handler" #,(syntax->datum #'f))
         (authenticate
          '#,(syntax->datum #'f)
          email
          passwd
          (λ () body0 body ...)
          (λ (reason) reason))))]))

(define email-codes (make-hash))
(define-jsonp (jsonp/authenticate ['email email] ['passwd passwd] ['code email-code])
  (log! "authenticate handler")
  (define (generate-a-code email)
    (define correct-email-code
      (number->string (random (expt 10 8))))
    (hash-set! email-codes email correct-email-code)
    correct-email-code)
  (define (check-code-or onSuccess onNoCode)
    (cond
      [(and (not (string=? "" email-code))
            (hash-ref email-codes email #f))
       => (λ (correct-email-code)
            (cond
              [(equal? correct-email-code email-code)
               (update-password email passwd)
               (hash-remove! email-codes email)
               (onSuccess)]
              [else 'wrong-code]))]
      [else (onNoCode)]))
  (authenticate
   'jsonp/authenticate
   email
   passwd
   (λ () (hasheq 'curation (curation-administrator? email))) ; XXX expose this?
   (λ (reason)
     (log! (format "login failed with reason ~a" reason))
     (match reason
       ['bad-password
        (check-code-or
         (λ () (hasheq 'curation (curation-administrator? email))) ; XXX expose this?
         (λ ()
           (send-mail-message
            "pkg@racket-lang.org"
            "Account password reset for Racket Package Catalog"
            (list email)
            empty empty
            (list
             "Someone tried to login with your email address for an account on the Racket Package Catalog, but failed."
             "If you this was you, please use this code to reset your password:"
             ""
             (generate-a-code email)
             ""
             "This code will expire, so if it is not available, you'll have to try to again."))
           #f))]
       ['new-user
        (check-code-or
         (λ () #t)
         (λ ()
           (send-mail-message
            "pkg@racket-lang.org"
            "Account confirmation for Racket Package Catalog"
            (list email)
            empty empty
            (list
             "Someone tried to register your email address for an account on the Racket Package Catalog."
             "If you want to proceed, use this code:"
             ""
             (generate-a-code email)
             ""
             "This code will expire, so if it is not available, you'll have to try to register again."))
           #f))]))))

(define-jsonp/auth (jsonp/update)
  (signal-update! (packages-of (authenticated-as)))
  #t)

(define-jsonp/auth (jsonp/package/del ['pkg pkg])
  (package-remove! pkg)
  (signal-static! empty)
  #t)

(define-jsonp/auth
  (jsonp/package/modify
   ['pkg pkg] ['name mn-name] ['description mn-desc] ['source mn-source])
  (cond
    [(equal? pkg "")
     (if (new-package-info mn-name mn-source (authenticated-as) mn-desc (current-seconds))
         (begin
           (signal-update! (list mn-name))
           #t)
         #f)]
    [else
     (let ([pkg-info (package-info pkg)])
       (cond
         [(equal? mn-name pkg)
          (package-info-set! (hash-set* pkg-info
                                        'source mn-source
                                        'description mn-desc
                                        'last-edit (current-seconds)))
          (signal-update! (list pkg))
          #t]
         [(not (package-exists? mn-name))
          (package-info-set! (hash-set* pkg-info
                                        'name mn-name
                                        'source mn-source
                                        'description mn-desc
                                        'last-edit (current-seconds)))
          (package-remove! pkg)
          (signal-update! (list mn-name))
          #t]
         [else #f]))]))

(define-jsonp/auth (jsonp/package/version/add ['pkg pkg] ['version version] ['source source])
  (let ([pkg-info (package-info pkg)])
    (package-info-set! (hash-update pkg-info 'versions
                                    (λ (v-ht)
                                      (hash-set v-ht version
                                                (hasheq 'source source
                                                        'checksum ""))
                                      hash)))
    (signal-update! (list pkg))
    #t))

(define-jsonp/auth (jsonp/package/version/del ['pkg pkg] ['version version])
  (let ([pkg-info (package-info pkg)])
    (package-info-set! (hash-update pkg-info 'versions
                                    (λ (v-ht)
                                      (hash-remove v-ht version))
                                    hash))
    (signal-update! (list pkg))
    #t))

(define-jsonp/auth (jsonp/package/tag/add ['pkg pkg] ['tag tag])
  (let* ([pkg-info (package-info pkg)]
         [new-tags (package-tags-normalize (cons tag (package-ref pkg-info 'tags)))])
    (package-info-set! (hash-set pkg-info 'tags new-tags))
    (signal-static! (list pkg))
    #t))

(define-jsonp/auth (jsonp/package/tag/del ['pkg pkg] ['tag tag])
  (let* ([pkg-info (package-info pkg)]
         [new-tags (remove tag (package-ref pkg-info 'tags))])
    (package-info-set! (hash-set pkg-info 'tags new-tags))
    (signal-static! (list pkg))
    #t))

(define-jsonp/auth (jsonp/package/author/add ['pkg pkg] ['author author])
  (let* ([pkg-info (package-info pkg)]
         [authors  (package-authors pkg-info)])
    (let ([new-authors (string-join (cons author authors))])
          (package-info-set! (hash-set pkg-info 'author new-authors))
          (signal-static! (list pkg))
          #t)))

(define-jsonp/auth (jsonp/package/author/del ['pkg pkg] ['author author])
  (let* ([pkg-info (package-info pkg)]
         [authors  (package-authors pkg-info)])
    (if (> (length authors) 1)
        (let ([new-authors (string-join (remove author authors))])
          (package-info-set! (hash-set pkg-info 'author new-authors))
          (signal-static! (list pkg))
          #t)
        #f)))

(define-jsonp/auth (jsonp/package/curate ['pkg pkg] ['ring ring-s])
  (let* ([info (package-info pkg)]
         [ring-n (string->number ring-s)])
    (package-curate! info (min 2 (max 0 ring-n)))
    (signal-static! (list pkg))
    #t))

(define (api/upload req)
  (define req-data (read (open-input-bytes (or (request-post-data/raw req) #""))))
  (match-define (list email given-password pis) req-data)
  (authenticate
   'api/upload
   email
   given-password
   (λ ()
     (log! "receiving api/upload!")
     (for ([(p more-pi) (in-hash pis)])
       (log! "received api/upload for ~a" p)
       (define pi
         (if (package-exists? p)
             (package-info p)
             #hash()))
       (define new-pi (hash-deep-merge pi more-pi))
       (define updated-pi
         (hash-remove
          (let ([now (current-seconds)])
            (for/fold ([pi new-pi])
                      ([k (in-list '(last-edit last-checked last-updated))])
              (hash-set pi k now)))
          'checksum))
       (log! "api/upload old ~v more ~v new ~v updated ~v"
             (hash-ref pi 'source #f)
             (hash-ref more-pi 'source #f)
             (hash-ref new-pi 'source #f)
             (hash-ref updated-pi 'source #f))
       (package-info-set! updated-pi))
     (signal-update! (hash-keys pis))
     (response/sexpr #t))
   (λ (reason)
     (response/sexpr #f))))

(define jsonp/notice
  (make-jsonp-responder (λ (args) (get-notices))))

(define-values (main-dispatch main-url)
  (dispatch-rules
   [("jsonp" "authenticate") jsonp/authenticate]
   [("jsonp" "update") jsonp/update]
   [("jsonp" "package" "del") jsonp/package/del]
   [("jsonp" "package" "modify") jsonp/package/modify]
   [("jsonp" "package" "version" "add") jsonp/package/version/add]
   [("jsonp" "package" "version" "del") jsonp/package/version/del]
   [("jsonp" "package" "tag" "add") jsonp/package/tag/add]
   [("jsonp" "package" "tag" "del") jsonp/package/tag/del]
   [("jsonp" "package" "author" "add") jsonp/package/author/add]
   [("jsonp" "package" "author" "del") jsonp/package/author/del]
   [("jsonp" "package" "curate") jsonp/package/curate]
   [("api" "upload") #:method "post" api/upload]
   [("jsonp" "notice") jsonp/notice]))

(define (go)
  (log! "launching on port ~v" port)
  (signal-static! empty)
  (thread
   (λ ()
     (forever
      (log! "update-t: Running scheduled build update.")
      (signal-build-update!)
      (log! "update-t: Running scheduled update.")
      (signal-update! empty)
      (log! "update-t: sleeping for 1 min")
      (sleep 60)))) ;(* 1 60 60)))))
  (serve/servlet
   main-dispatch
   #:command-line? #t
   ;; xxx I am getting strange behavior on some connections... maybe
   ;; this will help?
   #:connection-close? #t
   #:listen-ip #f
   #:ssl? (eq? scheme "https")
   #:ssl-cert (if (eq? scheme "https") ssl-cert-path #f)
   #:ssl-key (if (eq? scheme "https") ssl-key-path #f)
   #:extra-files-paths (list static-path)
   #:servlet-regexp #rx""
   #:port port))

(module+ main
  (require "init.rkt")
  (initialize)
  (go))
