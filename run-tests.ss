#lang scheme/base

(require (planet schematics/schemeunit:3/test)
         (planet schematics/schemeunit:3/text-ui)
         "all-idcheck-tests.ss"
         "idcheck.ss"
         "test-configuration.ss")

(print-struct #t)

(parameterize ([idcheck-url           test-idcheck-url]
               [idcheck-redirect-url  test-idcheck-redirect-url]
               [base-url              test-base-url]
               [idcheck-cookie-domain test-idcheck-cookie-domain])
  (run-tests all-idcheck-tests))
