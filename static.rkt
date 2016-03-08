#lang racket/base

(require racket/contract
         racket/list
         racket/set
         racket/match
         racket/path
         racket/promise
         racket/file
         racket/port
         web-server/http
         web-server/dispatch
         net/url
         xml
         json)

(require "config.rkt"
         "log.rkt"
         "notify.rkt"
         "pkgs.rkt"
         "build-summary.rkt"
         "basic.rkt"
         "utils.rkt"
         "monitor.rkt")

(provide (contract-out [signal-static! authority-closure/c])
         do-static)

(define (update-package-info pkg-name)
  (log! "static: building ht for ~v" pkg-name)
  (let* ([pkg-info (package-info pkg-name)]
         [versions (hash-set (hash-ref pkg-info 'versions (hash))
                             'default
                             (hasheq 'source (hash-ref pkg-info 'source "")
                                     'checksum (hash-ref pkg-info 'checksum "")))]
         [versions-5.3.6 (hash-ref-or versions '("5.3.6" default))]
         [source-5.3.6 (hash-ref versions-5.3.6 'source)]
         [checksum-5.3.6 (hash-ref versions-5.3.6 'checksum)]
         [conflicts empty])
    (values (hash-set* pkg-info
                       'name pkg-name
                       'source source-5.3.6
                       'checksum checksum-5.3.6
                       'last-updated (hash-ref pkg-info 'last-updated (current-seconds))
                       'last-checked (hash-ref pkg-info 'last-checked (current-seconds))
                       'last-edit (hash-ref pkg-info 'last-edit (current-seconds))
                       'versions versions
                       'ring (hash-ref pkg-info 'ring 2)
                       'dependencies (hash-ref pkg-info 'dependencies empty)
                       'modules (hash-ref pkg-info 'modules empty)
                       'conflicts
                       (hash-ref pkg-info 'conflicts
                                 (λ ()
                                   (set! conflicts (cons pkg-name conflicts))
                                   empty))
                       'tags (hash-ref pkg-info 'tags empty)
                       'author (hash-ref pkg-info 'author "")
                       'authors (package-authors pkg-info))
            conflicts)))

(define (module-lists-conflict? left right)
  (define seen? (make-hash))
  (for ([l (in-list left)])
    (hash-set! seen? l #t))
  (for/or ([r (in-list right)])
    (hash-ref seen? r #f)))

(define (packages-conflict? left right)
  (log! "static: computing conflict between ~v and ~v" left right)
  (define left-i (package-info left))
  (define right-i (package-info right))
  (define left-m (and left-i (hash-ref left-i 'modules #f)))
  (define right-m (and right-i (hash-ref right-i 'modules #f)))
  (if (and left-m right-m)
      (module-lists-conflict? left-m right-m)
      ;; We have to say #t here because otherwise things with no
      ;; information won't be conflicting.
      #t))

;; From pkg-build/summary
(struct doc/main (name path) #:prefab)
(struct doc/extract (name path) #:prefab)
(struct doc/salvage (name path) #:prefab)
(struct doc/none (name) #:prefab)
(struct conflicts/indirect (path) #:prefab)

(define (generate-static pkgs)
  (let ([all-pkg-list (package-list)]
        [these-pkg-list pkgs]
        [conflicts (mutable-set)]
        [packages (make-hash)]
        [build-summary (build-summary)]
        [conflict-cache (make-hash)])
    (for ([pkg-name (in-list all-pkg-list)])
      (let-values ([(updated-info conflict-list) (update-package-info pkg-name)])
        (set-union! conflicts (list->set conflict-list))
        (hash-set! packages pkg-name updated-info)))
    (define (packages-conflict?/cache left right)
      (define smin (string-min left right))
      (define smax (string-max left right))
      (hash-ref! conflict-cache
                 (cons smin smax)
                 (λ ()
                   (packages-conflict? smin smax))))
    (log! "static: computing ring-01")
    (define ring-01
      (filter (λ (p) (member (hash-ref (package-info p) 'ring 2) '(0 1))) all-pkg-list))
    (define (package-aconflicts? pkg)
      (filter (λ (other-pkg)
                (if (equal? pkg other-pkg)
                    #f
                    (packages-conflict?/cache pkg other-pkg)))
              ring-01))
    (define changed-pkg-set (set-union (list->set these-pkg-list) conflicts))
    (log! "static: computing conflicts")
    (define (package-conflicts? pkg)
      (filter (λ (other-pkg)
                (if (equal? pkg other-pkg)
                    #f
                    (packages-conflict?/cache pkg other-pkg)))
              ring-01))
    (define (compute-conflicts! some-pkgs)
      (log! "static: computing conflicts: new round")
      (define more-set (list->set '()))
      (for ([pkg (in-list some-pkgs)])
        (log! "static: computing conflicts for ~v" pkg)
        (hash-update!
         packages pkg
         (λ (ht)
           (define conflicts (package-conflicts? pkg))
           (set! more-set (set-union more-set (list->set conflicts)))
           (package-info-set! (hash-set (package-info pkg)
                                        'conflicts conflicts))
           (hash-set ht 'conflicts conflicts))))
      (set! more-set (set-subtract more-set changed-pkg-set))
      (unless (set-empty? more-set)
        (set! changed-pkg-set (set-union changed-pkg-set more-set))
        (compute-conflicts! (set->list more-set))))
    (compute-conflicts! these-pkg-list)
    (define changed-pkg-list (set->list changed-pkg-set))
    (for ([pkg (in-list all-pkg-list)])
      (log! "static: computing detailed ht for ~v" pkg)
      (define pb (hash-ref build-summary pkg #f))
      (define (pbl k)
        (and pb (hash-ref pb k #f)))
      
      (hash-update!
       packages pkg
       (λ (ht)
         (define conflicts (hash-ref ht 'conflicts))
         (hash-set*
          ht
          'build
          (hash 'min-failure-log (pbl 'min-failure-log)
                'success-log (pbl 'success-log)
                'test-failure-log (pbl 'test-failure-log)
                'test-success-log (pbl 'test-success-log)
                'conflicts-log
                (match (pbl 'conflicts-log)
                  [#f #f]
                  [(? path-string? f) f]
                  [(conflicts/indirect file)
                   (list "indirect" file)])
                'dep-failure-log (pbl 'dep-failure-log)
                'docs
                (for/list ([d (in-list (or (pbl 'docs) empty))])
                  (match d
                    [(doc/main n p) (list "main" n p)]
                    [(doc/extract n p) (list "extract" n p)]
                    [(doc/salvage n p) (list "salvage" n p)]
                    [(doc/none n) (list "none" n)]))
                'failure-log (pbl 'failure-log))
          'versions
          (for/hash ([(v vht) (in-hash (hash-ref ht 'versions))])
            (values v
                    (with-handlers ([exn:fail? (λ (x) vht)])
                      (hash-set vht 'source_url
                                (package-url->useful-url (hash-ref vht 'source))))))
          'search-terms
          (let* ([st (hasheq)]
                 [st (for/fold ([st st])
                               ([t (in-list (hash-ref ht 'tags))])
                       (hash-set st (string->symbol t) #t))]
                 [st (hash-set
                      st
                      (string->symbol
                       (format "ring:~a" (hash-ref ht 'ring))) #t)]
                 [st (for/fold ([st st])
                               ([a (in-list (package-authors ht))])
                       (hash-set
                        st (string->symbol (format "author:~a" a)) #t))]
                 [st (if (empty? (hash-ref ht 'tags))
                         (hash-set st ':no-tag: #t)
                         st)]
                 [st (if (hash-ref ht 'checksum-error #f)
                         (hash-set st ':error: #t)
                         st)]
                 [st (if (equal? "" (hash-ref ht 'description ""))
                         (hash-set st ':no-desc: #t)
                         st)]
                 [st (if (empty? conflicts)
                         st
                         (hash-set st ':conflicts: #t))]
                 [st (if (pbl 'success-log)
                         (hash-set st ':build-success: #t)
                         st)]
                 [st (if (pbl 'failure-log)
                         (hash-set st ':build-fail: #t)
                         st)]
                 [st (if (pbl 'dep-failure-log)
                         (hash-set st ':build-dep-fail: #t)
                         st)]
                 [st (if (pbl 'conflicts-log)
                         (hash-set st ':build-conflicts: #t)
                         st)]
                 [pb-docs (pbl 'docs)]
                 [st (if (and pb-docs (cons? pb-docs)
                              (andmap (λ (d)
                                        (or (doc/main? d)
                                            (doc/extract? d)
                                            (doc/salvage? d)))
                                      pb-docs))
                         (hash-set st ':docs: #t)
                         st)]
                 [st (if (and pb-docs (cons? pb-docs)
                              (andmap (λ (d)
                                        (or (doc/extract? d)
                                            (doc/salvage? d)
                                            (doc/none? d)))
                                      pb-docs))
                         (hash-set st ':docs-error: #t)
                         st)])
            st)))))
    
    (define basic-dispatch
      (pkg-index/basic
       (λ () all-pkg-list)
       (λ (pkg-name) (hash-ref packages pkg-name))))
    
    (define (page/atom.xml req)
      (define ps
        (sort (map package-info all-pkg-list)
              >
              #:key (λ (i) (hash-ref i 'last-updated -inf.0))))
      (define top
        (if (empty? ps)
            0
            (hash-ref (first ps) 'last-updated -inf.0)))
      (define (atom-format-time t)
        (format "~aZ" (format-time t)))
      (response/xexpr
       #:mime-type #"application/atom+xml"
       `(feed
         ([xmlns "http://www.w3.org/2005/Atom"])
         (title ,(cdata #f #f (format "<![CDATA[~a]]>"
                                      "Racket Package Updates")))
         (link ([href ,(url->string (combine-url/relative pkg-url "rss"))]
                [rel "self"]))
         (link ([href ,(url->string pkg-url)]))
         (updated ,(atom-format-time top))
         (id pkg-url)
         ,@(for/list ([i (in-list ps)])
             (define p (hash-ref i 'name))
             (define this-url
               (url->string (combine-url/relative pkg-url (format "#[~a]" p))))
             (define lu (atom-format-time (hash-ref i 'last-updated -inf.0)))
             (define a 
               (match (package-authors i)
                 [(cons a _) a]
                 ['() "nobody"]))
             (match-define (regexp #rx"^([^@]+)" (list _ n)) a)
             `(entry
               (title ([type "html"])
                      ,(cdata #f #f (format "<![CDATA[~a]]>" p)))
               (link ([href ,this-url]))
               (updated ,lu)
               (author (name ,n) (email ,a))
               (id ,this-url)
               (content
                ([type "html"])
                ,(cdata #f #f
                        (format "<![CDATA[~a]]>"
                                (xexpr->string
                                 `(div
                                   (p ,(format "~a package updated on ~a."
                                               p lu))
                                   (p ,(format
                                        "Checksum: ~a"
                                        (hash-ref (hash-ref (hash-ref i 'versions (hash))
                                                            'default (hasheq))
                                                  'checksum "")))
                                   (p ,(hash-ref i 'description ""))))))))))))
    
    (define-values (main-dispatch main-url)
      (dispatch-rules
       [("atom.xml") page/atom.xml]
       [else basic-dispatch]))
    
    (define (url->request u)
      (make-request #"GET" (string->url u) empty
                    (delay empty) #f "1.2.3.4" 80 "4.3.2.1"))
    
    (define (cache url file)
      (define p (build-path static-path file))
      (make-directory* (path-only p))
      
      (define bs
        (with-output-to-bytes
            (λ ()
              ((response-output (main-dispatch (url->request url)))
               (current-output-port)))))
      (unless (and (file-exists? p)
                   (bytes=? bs (file->bytes p)))
        (log! "static: caching ~v" p)
        (with-output-to-file p
          #:exists 'replace
          (λ () (display bs)))
        (with-output-to-file (path-add-suffix p #".json")
          #:exists 'replace
          (λ () (write-json (convert-to-json (file->value p))))))
      (void))
    
    (define (copy-directory/files+ src dest)
      (cond
        [(directory-exists? src)
         (cond [(directory-exists? dest)
                (void)]
               [(file-exists? dest)
                (error 'copy-directory/files+ "Can't copy dir ~v to file ~v" src dest)]
               [else
                (make-directory dest)])
         (for ([p (in-list (directory-list src))])
           (copy-directory/files+ (build-path src p) (build-path dest p)))]
        [(file-exists? src)
         (cond [(directory-exists? dest)
                (error 'copy-directory/files+ "Can't copy file ~v to dir ~v" src dest)]
               [(file-exists? dest)
                (copy-file src dest #t)]
               [else
                (copy-file src dest)])
         (file-or-directory-modify-seconds
          dest
          (file-or-directory-modify-seconds src))]
        [else
         (error 'copy-directory/files+ "Unknown kind of source ~v" src)]))
    
    (log! "static: copying ~v to ~v" static.src-path static-path)
    (copy-directory/files+
     static.src-path
     static-path)
    
    ;; xxx
    (set! changed-pkg-list all-pkg-list)
    
    (log! "static: caching database")
    (cache "/atom.xml" "atom.xml")
    (cache "/pkgs" "pkgs")
    (cache "/pkgs-all" "pkgs-all")
    (for ([p (in-list changed-pkg-list)])
      (cache (format "/pkg/~a" p) (format "pkg/~a" p)))
    
    (let ()
      (log! "static: removing deleted files")
      (define pkg-path (build-path static-path "pkg"))
      (for ([f (in-list (directory-list pkg-path))]
            #:unless (regexp-match #"json$" (path->string f))
            #:unless (member (path->string f) all-pkg-list))
        (log! "static: removing ~v" f)
        (with-handlers ([exn:fail:filesystem? void])
          (delete-file (build-path pkg-path f))
          (delete-file (build-path pkg-path (path-add-suffix f #".json"))))))
    
    changed-pkg-list))

(define (do-static pkgs)
  (notify! "update upload being computed: the information below may not represent all recent changes and updates")
  (define changed-pkgs (generate-static pkgs))
  changed-pkgs)

(define run-sema (make-semaphore 1))
(define (signal-static! pkgs)
  (safe-run! run-sema (λ () (run! do-static pkgs))))

(module+ main
  (require racket/cmdline)
  
  (command-line
   #:program "static"
   #:args pkgs
   (do-static pkgs)))
