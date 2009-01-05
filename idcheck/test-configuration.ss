#lang scheme/base

(provide test-idcheck-url
         test-idcheck-redirect-url
         test-base-url
         test-idcheck-cookie-domain)

; ***** ALTER THESE VALUES TO CONFIGURE YOUR TEST ENVIRONMENT *****
(define test-idcheck-url           #f) ; example: "http://idcheck.example.com/idcheck"
(define test-idcheck-redirect-url  #f) ; example: "https://idcheck.example.com/idcheck"
(define test-base-url              #f) ; example: "http://www.example.com:5678"
(define test-idcheck-cookie-domain #f) ; example: ".example.com"
; *****************************************************************
