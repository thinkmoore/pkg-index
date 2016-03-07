#lang racket

(require authorization-contracts)

(provide is-author/c is-curator/c as-user/c
         grant-curator/c revoke-curator/c
         deprivilege/c ->a)

(define-monitor pkg-monitor
  (monitor-interface is-author/c is-curator/c as-user/c grant-curator/c revoke-curator/c deprivilege/c)
  (action
   [is-author/c (pkg)
    #:on-create (do-create)
    #:on-apply
    (let* ([authors (string-split (hash-ref pkg 'author))]
           [authorpcpl (disj (list->set (map author->pcpl authors)))])
      (do-apply #:check (≽@ current-principal authorpcpl authorpcpl)))]
   [is-curator/c
    #:on-create (do-create)
    #:on-apply (do-apply #:check (≽@ current-principal curator curator))]  
   [as-user/c (user)
    #:on-create (do-create)
    #:on-apply  (do-apply #:set-principal (author->pcpl user))]
   [grant-curator/c (user)
    #:on-create (do-create)
    #:on-apply  (do-apply #:add (list (≽@ (author->pcpl user) curator curator)))]
   [revoke-curator/c (user)
    #:on-create (do-create)
    #:on-apply  (do-apply #:remove (list (≽@ (author->pcpl user) curator curator)))]
   [deprivilege/c
    #:on-create (do-create)
    #:on-apply  (do-apply #:set!-principal unpriv)])
  (extra
   (define curator (pcpl 'curator))
   (define unpriv (pcpl 'unprivileged))
   (define pcpls (make-weak-hash))
   (define (author->pcpl author)
     (hash-ref! pcpls author (λ () (pcpl (string->symbol author)))))))

(run pkg-monitor)
