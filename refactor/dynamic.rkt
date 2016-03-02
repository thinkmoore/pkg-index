#lang racket/base

(require racket/list
         racket/match
         racket/file
         racket/string
         web-server/http
         web-server/servlet-env
         web-server/dispatch
         net/url
         net/sendmail)

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

(define-syntax-rule (define-jsonp/auth (f . pat) . body)
  (define-jsonp (f ['email email] ['passwd passwd] . pat)
    (authenticate
     (syntax->datum f)
     #:email email
     #:password passwd
     #:on-success (λ () body)
     #:on-failure (λ (reason) reason))))

(define-syntax-rule (define-jsonp/auth/author (f . pat) . body)
  (define-jsonp (f ['email email] ['passwd passwd] ['pkg author-of] . pat)
    (authenticate
     (syntax->datum f)
     #:as-author author-of
     #:email email
     #:password passwd
     #:on-success (λ () body)
     #:on-failure (λ (reason) reason))))

(define-syntax-rule (define-jsonp/auth/curator (f . pat) . body)
  (define-jsonp (f ['email email] ['passwd passwd] . pat)
    (authenticate
     (syntax->datum f)
     #:as-curator #t
     #:email email
     #:password passwd
     #:on-success (λ () body)
     #:on-failure (λ (reason) reason))))

(define email-codes (make-hash))
(define-jsonp (jsonp/authenticate ['email email] ['passwd passwd] ['code email-code])
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
   #:email email
   #:password passwd
   #:on-success (λ () (hasheq 'curation (curation-administrator? email))) ; XXX expose this?
   #:on-failure
   (λ (reason)
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
             "This code will expire, so if it is not available, you'll have to try to again."))))]
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
             "This code will expire, so if it is not available, you'll have to try to register again."))))]))))

(define-jsonp/auth (jsonp/update)
  (signal-update! (packages-of (authenticated-as))))

(define-jsonp/auth/author (jsonp/package/del ['pkg pkg])
  (package-remove! pkg)
  (signal-static! empty))

(define-jsonp/auth
  (jsonp/package/modify
   ['pkg pkg] ['name mn-name] ['description mn-desc] ['source mn-source])
  (cond
    [(equal? pkg "")
     (unless (package-exists? mn-name)
       (package-info-set! (hasheq 'name mn-name
                                  'source mn-source
                                  'author (authenticated-as)
                                  'description mn-desc
                                  'last-edit (current-seconds)))
       (signal-update! (list mn-name)))]
    [else
     (let ([pkg-info (package-info pkg)])
       (cond
         [(not (package-author? pkg-info (authenticated-as))) #f]
         [(equal? mn-name pkg)
          (package-info-set! (hash-set* pkg-info
                                        'source mn-source
                                        'description mn-desc
                                        'last-edit (current-seconds)))
          (signal-update! (list pkg))]
         [(not (package-exists? mn-name))
          (package-info-set! (hash-set* pkg-info
                                        'name mn-name
                                        'source mn-source
                                        'description mn-desc
                                        'last-edit (current-seconds)))
          (package-remove! pkg)
          (signal-update! (list mn-name))]
         [else #f]))]))

(define-jsonp/auth/author (jsonp/package/version/add ['pkg pkg] ['version version] ['source source])
  (let ([pkg-info (package-info pkg)])
    (package-info-set! (hash-update pkg-info 'versions
                                    (λ (v-ht)
                                      (hash-set v-ht version
                                                (hasheq 'source source
                                                        'checksum ""))
                                      hash)))
    (signal-update! (list pkg))))

(define-jsonp/auth/author (jsonp/package/version/del ['pkg pkg] ['version version])
  (let ([pkg-info (package-info pkg)])
    (package-info-set! (hash-update pkg-info 'versions
                                    (λ (v-ht)
                                      (hash-remove v-ht version))
                                    hash))
    (signal-update! (list pkg))))

(define-jsonp/auth (jsonp/package/tag/add ['pkg pkg] ['tag tag])
  (let* ([pkg-info (package-info pkg)]
         [new-tags (package-tags-normalize (cons tag (package-ref pkg-info 'tags)))])
    (package-info-set! (hash-set pkg-info 'tags new-tags))
    (signal-static! (list pkg))))

(define-jsonp/auth/author (jsonp/package/tag/del ['pkg pkg] ['tag tag])
  (let* ([pkg-info (package-info pkg)]
         [new-tags (remove tag (package-ref pkg-info 'tags))])
    (package-info-set! (hash-set pkg-info 'tags new-tags))
    (signal-static! (list pkg))))

(define-jsonp/auth/author (jsonp/package/author/add ['pkg pkg] ['author author])
  (let* ([pkg-info (package-info pkg)]
         [authors  (package-authors pkg-info)])
    (let ([new-authors (string-join (cons author authors))])
          (package-info-set! (hash-set pkg-info 'author new-authors))
          (signal-static! (list pkg))
          #t)))

(define-jsonp/auth/author (jsonp/package/author/del ['pkg pkg] ['author author])
  (let* ([pkg-info (package-info pkg)]
         [authors  (package-authors pkg-info)])
    (if (> (length authors) 1)
        (let ([new-authors (string-join (remove author authors))])
          (package-info-set! (hash-set pkg-info 'author new-authors))
          (signal-static! (list pkg))
          #t)
        #f)))

(define-jsonp/auth/curator (jsonp/package/curate ['pkg pkg] ['ring ring-s])
  (let* ([info (package-info pkg)]
         [ring-n (string->number ring-s)])
    (package-info-set! (hash-set info 'ring (min 2 (max 0 ring-n))))
    (signal-static! (list pkg))))

(define (api/upload req)
  (define req-data (read (open-input-bytes (or (request-post-data/raw req) #""))))
  (match-define (list email given-password pis) req-data)
  (authenticate
   'api/upload
   #:as-curator #t
   #:email email
   #:password given-password
   #:on-success
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
   #:on-failure
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
   [("jsonp" "notice") jsonp/notice]
   [else redirect-to-static]))

(define (redirect-to-static req)
  (redirect-to
   (url->string
    (struct-copy url (request-uri req)
                 [scheme "http"]
                 [host "pkgs.racket-lang.org"]
                 [port 80]))))

(define (go port)
  (log! "launching on port ~v" port)
  (signal-static! empty)
  (thread
   (λ ()
     (forever
      (log! "update-t: Running scheduled build update.")
      (signal-build-update!)
      (log! "update-t: Running scheduled update.")
      (signal-update!/beat empty)
      (log! "update-t: sleeping for 1 hour")
      (sleep (* 1 60 60)))))
  (serve/servlet
   main-dispatch
   #:command-line? #t
   ;; xxx I am getting strange behavior on some connections... maybe
   ;; this will help?
   #:connection-close? #t
   #:listen-ip #f
   #:ssl? #t
   #:ssl-cert ssl-cert-path
   #:ssl-key ssl-key-path
   #:extra-files-paths empty
   #:servlet-regexp #rx""
   #:port port))

(module+ main
  (go 9904))