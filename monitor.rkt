#lang racket

(require authorization-contracts)

(provide is-author/c is-curator/c as-user/c
         grant-curator/c revoke-curator/c
         ->a)

(define-monitor pkg-monitor
  (monitor-interface is-author/c is-curator/c as-user/c grant-curator/c revoke-curator/c)
  (action
   [is-author/c (pkg)
    #:on-create (do-create)
    #:on-apply
    (let* ([authors (string-split (hash-ref pkg 'author))]
           [authorpcpl (disj (list->set authors))])
      (do-apply #:check (≽@ current-principal authorpcpl authorpcpl)))]
   [is-curator/c
    #:on-create (do-create)
    #:on-apply (do-apply #:check (≽@ current-principal curator curator))]  
   [as-user/c (user)
    #:on-create (do-create)
    #:on-apply  (do-apply #:set-principal user)]
   [grant-curator/c (user)
    #:on-create (do-create #:add (list (≽@ user curator curator)))
    #:on-apply  (do-apply)]
   [revoke-curator/c (user)
    #:on-create (do-create #:remove (list (≽@ user curator curator)))
    #:on-apply  (do-apply)])
  (extra
   (define curator (pcpl 'curator))))

(run pkg-monitor)
