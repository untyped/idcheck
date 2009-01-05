#lang scheme/base

(require (for-syntax scheme/base)
         scheme/runtime-path
         web-server/servlet
         (planet untyped/instaservlet:2)
         "base.ss"
         "idcheck.ss"
         "test-configuration.ss")

; request -> void
(define (menu request)
  ; (listof binding)
  (define bindings
    (request-bindings request))
  ; (U string #f)
  (define action
    (and (exists-binding? 'do bindings)
         (extract-binding/single 'do bindings)))
  (show (cond [(equal? action "login")  (login request)]
              [(equal? action "logout") (logout request)]
              [else                     request])))

; request -> request
(define (login request)
  (idcheck-login request))

; request -> request
(define (logout request)
  (idcheck-logout request))

; request -> void
(define (show request)
  (send/finish
   `(html (head (title "Successfully Logged In"))
          (body (h1 "Successfully Logged In")
                ,@(let* ([key      (get-key request)]
                         [info     (and key (lookup-user key))]
                         [username (and info (get-username info))])
                    `((p "Action " (a ([href "?do=login"]) "Login")
                         " " (a ([href "?do=logout"]) "Logout"))
                      (p "Your IDCheck key is: ")
                      (blockquote (pre ,(or key "[none]")))
                      (p "Your details returned by IDCheck are: ")
                      (blockquote (pre ,(or info "[none]")))
                      (p "Your username is: ")
                      (blockquote (pre ,(or username "[none]")))))))))

; Main program body ------------------------------

(print-struct #t)

(go! (lambda (request)
       (parameterize ([idcheck-url           test-idcheck-url]
                      [idcheck-redirect-url  test-idcheck-redirect-url]
                      [base-url              test-base-url]
                      [idcheck-cookie-domain test-idcheck-cookie-domain])
         (menu request)))
     #:port 5678)
