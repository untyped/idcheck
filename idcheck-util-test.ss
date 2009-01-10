(module idcheck-util-test mzscheme
  
  (require (lib "request-structs.ss" "web-server" "private")
           (lib "response-structs.ss" "web-server" "private")
           (file "cookie.ss")
           (file "idcheck-util.ss")
           (file "test-base.ss")
           (file "test-configuration.ss"))
  
  (provide idcheck-util-tests)
  
  ;; test-host : bytes
  ;;
  ;; This is used in a number of tests below...
  (define test-host #"www.untyped.com")
  
  (define idcheck-util-tests
    (test-suite
     "All tests for idcheck-util"
     
     (test-case
      "preregistration-key? correct"
      (check-true
       (preregistration-key? "R1234567890123456789012345678901"))
      (check-false (preregistration-key? "R1234"))
      (check-false
       (preregistration-key? "r1111111111111111111111111111111"))
      (check-false (preregistration-key? "abcd"))
      (check-false
       (preregistration-key? "12345678901234567890123456789012")))
     
     (test-case
      "registered-key? correct"
      (check-true
       (registered-key? "12345678901234567890123456789012"))
      (check-false (registered-key? "1234"))
      (check-false
       (registered-key? "R1234567890123456789012345678901"))
      (check-false (registered-key? "abcd")))
     
     (test-case
      "unregistered? correct"
      (check-not-false (unregistered?
                        `((host . ,test-host)
                          (accept . #"text/xml"))))
      (check-not-false (unregistered?
                        `((host . ,test-host)
                          (accept . #"text/xml")
                          (cookie . #"idcheck.request=R0847913617386740866043033607243"))))
      (check-false (unregistered?
                    `((host . ,test-host)
                      (accept . #"text/xml")
                      (cookie . #"idcheck.request=70847913617386740866043033607243"))))
      (check-false (unregistered?
                    `((host . ,test-host)
                      (accept . #"text/xml")
                      (cookie . #"idcheck=70847913617386740866043033607243")))))
     
     (test-case
      "validated? correct"
      (check-false (validated?
                    `((host . ,test-host)
                      (accept . #"text/xml"))))
      (check-false (validated?
                    `((host . ,test-host)
                      (accept . #"text/xml")
                      (cookie . #"idcheck.request=R0847913617386740866043033607243"))))
      (check-false (validated?
                    `((host . ,test-host)
                      (accept . #"text/xml")
                      (cookie . #"idcheck.request=70847913617386740866043033607243"))))
      (check-not-false (validated?
                        `((host . ,test-host)
                          (accept . #"text/xml")
                          (cookie . #"idcheck=70847913617386740866043033607243")))))
     
     (test-case
      "unvalidated? correct"
      (check-false (unvalidated?
                    `((host . ,test-host)
                      (accept . #"text/xml"))))
      (check-false (unvalidated?
                    `((host . ,test-host)
                      (accept . #"text/xml")
                      (cookie . #"idcheck.request=R0847913617386740866043033607243"))))
      (check-not-false (unvalidated?
                        `((host . ,test-host)
                          (accept . #"text/xml")
                          (cookie . #"idcheck.request=70847913617386740866043033607243"))))
      (check-false (unvalidated?
                    `((host . ,test-host)
                      (accept . #"text/xml")
                      (cookie . #"idcheck=70847913617386740866043033607243")))))
     
     (test-case
      "my-redirect-to generates correct response"
      (let ((response (my-redirect-to "url" '((set-cookie . "foo=bar")))))
        (check = (response/basic-code response) 302)
        (check-equal? (response/basic-mime response) #"text/html")
        (let* ([headers (response/basic-headers response)]
               [location-header (ormap (lambda (header)
                                         (if (equal? (header-field header) #"Location")
                                             header
                                             #f))
                                       headers)]
               [set-cookie-header (ormap (lambda (header)
                                         (if (equal? (header-field header) #"set-cookie")
                                             header
                                             #f))
                                         headers)])
          (check-not-false location-header "check 1")
          (check-not-false set-cookie-header "check 2")
          (check-equal? (header-value location-header) #"url" "check 3")
          (check-equal? (header-value set-cookie-header) #"foo=bar" "check 4"))))
     
     (test-case
      "set-idcheck-cookie sets correct attributes"
      (check
       string=?
       (print-cookie (set-idcheck-cookie "bar"))
       (format "idcheck.request=bar; expires=~a; path=/; domain=~a"
               (expires->rfc822-string (+ (current-seconds) 480))
               test-idcheck-cookie-domain)))
     
     (test-case
      "clear-idcheck-cookie sets correct attributes"
      (check
       string=?
       (print-cookie (clear-idcheck-cookie))
       (format "idcheck.request=null; expires=~a; path=/; domain=~a"
               (expires->rfc822-string 0)
               test-idcheck-cookie-domain)))
     
     (test-case
      "set-private-cookie sets correct attributes"
      (check
       string=?
       (print-cookie (set-private-cookie "bar"))
       "idcheck=bar; path=/"))
     
     (test-case
      "clear-private-cookie sets correct attributes"
      (check
       string=?
       (print-cookie (clear-private-cookie))
       (string-append "idcheck=null; expires="
                      (expires->rfc822-string 0)
                      "; path=/")))
     
     ))
  )
 
