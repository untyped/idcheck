#lang mzscheme

(require "idcheck-test.ss"
         "idcheck-util-test.ss"
         "idcheck-db-test.ss"
         "cookie-test.ss"
         "test-base.ss")

(provide all-idcheck-tests)

(define all-idcheck-tests
  (test-suite
   "all-idcheck-tests"
   idcheck-tests
   idcheck-util-tests
   idcheck-db-tests
   cookie-tests
   ))
