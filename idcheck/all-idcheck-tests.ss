(module all-idcheck-tests mzscheme

  (require (file "idcheck-test.ss")
           (file "idcheck-util-test.ss")
           (file "idcheck-db-test.ss")
           (file "cookie-test.ss")
           (file "test-base.ss"))

  (provide all-idcheck-tests)

  (define all-idcheck-tests
    (test-suite
     "all-idcheck-tests"
     idcheck-tests
     idcheck-util-tests
     idcheck-db-tests
     cookie-tests
     ))

  )
