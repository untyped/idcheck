(module run-tests mzscheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
           (file "all-idcheck-tests.ss")
           (file "idcheck.ss")
           (file "test-configuration.ss"))
  
  (parameterize ([idcheck-url           test-idcheck-url]
                 [idcheck-redirect-url  test-idcheck-redirect-url]
                 [base-url              test-base-url]
                 [idcheck-cookie-domain test-idcheck-cookie-domain])
    (test/text-ui all-idcheck-tests))
  
  )

