#lang scheme/base

(require srfi/26/cut
         "cookie.ss"
         "test-base.ss")

(define rfc-date?
  (cut regexp-match
       "[A-Z][a-z][a-z], [0-9][0-9]-[A-Z][a-z][a-z]-[0-9][0-9][0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] GMT"
       <>))

(define cookie-tests
  (test-suite "All tests for cookie"
    
    (test-case "rfc822 string generated correctly"
      (for-each (cut check-pred rfc-date? <>)
                (map expires->rfc822-string
                     `(0 1 2 100 123456 ,(current-seconds)))))
    
    (test-case "print-cookie includes expires attribute"
      (check-pred
       (cut regexp-match "expires=" <>)
       (print-cookie
        (cookie:add-expires
         (set-cookie "foo" "bar")
         10))))
    
    (test-case "print-cookie formats cookie correctly"
      (check string=?
             (print-cookie (cookie:add-expires
                            (cookie:add-domain
                             (cookie:add-path
                              (set-cookie "foo" "bar")
                              "/")
                             ".untyped.com")
                            10))
             (string-append "foo=bar; expires="
                            (expires->rfc822-string 10)
                            "; path=/; domain=.untyped.com")))))

(provide cookie-tests)
