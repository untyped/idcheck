#lang mzscheme

(require "idcheck.ss"
         "test-base.ss")

; test-suite
(define idcheck-tests
  (test-suite "All tests for idcheck"
    
    (test-case "Preregister returns a valid key"
      (let ((key (preregister "http://www.untyped.com/")))
        (check-pred string? key)
        (check = (string-length key) 32)
        (check-equal? (string-ref key 0) #\R)
        (check-not-false (string->number (substring key 1 32)))))
    
    (test-case "validate-key raises exception when key is not valid"
      (check-false (validate-key "THIS Will not work")))))

(provide idcheck-tests)
