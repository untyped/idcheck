(module example-servlet mzscheme

  (require (lib "servlet.ss" "web-server")
           (lib "url.ss" "net")
           (file "../../idcheck.ss")
           (file "../../test-configuration.ss"))

  (provide interface-version timeout start)

  (define interface-version 'v1)

  (define timeout +inf.0)

  ;; start : request -> response
  (define (start initial-request)
    (parameterize ([idcheck-url           test-idcheck-url]
                   [idcheck-redirect-url  test-idcheck-redirect-url]
                   [base-url              test-base-url]
                   [idcheck-cookie-domain test-idcheck-cookie-domain])
      (with-idcheck-authenticated-request
       initial-request
       (lambda (request)                                   
         (send/finish
          `(html (head (title "Successfully Logged In"))
                 (body (h1 "Successfully Logged In")
                       ,@(let ((info (lookup-user (get-key request))))
                           `((p "Your details returned by IDCheck are: " ,info)
                             (p "Your username is: " ,(get-username info)))))))))))
  
  )
