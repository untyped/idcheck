#lang scheme/base

(require net/url
         scheme/contract
         scheme/port
         scheme/pretty
         srfi/13
         srfi/26
         (prefix-in ws: web-server/servlet)
         (planet schematics/macro:1/aif)
         "base.ss"
         "cookie.ss"
         "idcheck-util.ss"
         "idcheck-db.ss")

; Parameters -----------------------------------

; (parameter ((string -> response) -> request))
(define idcheck-send/forward
  (make-parameter ws:send/forward))

; (string -> response) -> request
(define (send/forward response-generator)
  ((idcheck-send/forward) response-generator))

; (parameter string)
;
; The URL of the IDCheck CGI we communicate with.
;
; Example:
;
;     "http://idcheck.untyped.com/idcheck"
(define idcheck-url
  (make-parameter #f))

; -> string
;
; Retrieves the current value of the idcheck-url parameter,
; and checks to make sure the parameter has been set.
(define (get-idcheck-url)
  (let ([url (idcheck-url)])
    (or url (raise-exn:idcheck
             #<<ENDSTR
The idcheck-url parameter has not been set.
Initialize the parameter with a call to parameterize as follows:

   (parameterize ([idcheck-url "http://idcheck.untyped.com/idcheck"])
     ; Insert code here...
     )
ENDSTR
             ))))

; (parameter string)
;
; The URL we redirect people to so they can log in to IDCheck.
; This will normally be the same as idcheck-url, but may be
; different (for example, it may be https instead of http).
;
; Example:
;
;     "https://idcheck.untyped.com/idcheck"
(define idcheck-redirect-url
  (make-parameter #f))

; -> string
;
; Retrieves the current value of the idcheck-redirect-url parameter,
; and checks to make sure the parameter has been set.
(define (get-idcheck-redirect-url)
  (let ([url (idcheck-redirect-url)])
    (if url
        url
        (raise-exn:idcheck
         #<<ENDSTR
The idcheck-redirect-url parameter has not been set.
Initialize the parameter with a call to parameterize as follows:

   (parameterize ([idcheck-redirect-url "https://idcheck.untyped.com/idcheck"])
     ; Insert code here...
     )
ENDSTR
         ))))

; (parameter string)
;
; The base URL, without a trailing slash, of the server
; running this IDCheck client.
;
; Example:
;
;     "http://www.untyped.com" ; note: no trailing slash
(define base-url
  (make-parameter #f))

; -> string
;
; Retrieves the current value of the base-url parameter,
; and checks to make sure the parameter has been set.
(define (get-base-url)
  (let ([url (base-url)])
    (if url
        url
        (raise-exn:idcheck
         #<<ENDSTR
The base-url parameter has not been set.
Initialize the parameter with a call to parameterize as follows:

   (parameterize ([base-url "http://www.untyped.com"])
     ; Insert code here...
     )

Note: do not include a trailing slash.
ENDSTR
         ))))

; Functions ------------------------------------

; request (request -> any) -> any
(define (with-idcheck-authenticated-request request controller)
  (let loop ((request request))
    (aif key (get-key request)
         (if (lookup-user key)
             (controller request)
             (loop (idcheck-login (clear-cookies request))))
         (loop (idcheck-login request)))))

; request -> (U string #f)
(define (get-key request)
  (let ((headers (ws:request-headers request)))
    (if (validated? headers)
        (let ((key (headers-registered-key headers)))
          (if (validate-key key)
              key
              (begin (printf "Key not validated: ~s.~n" key)
                     #f)))
        (begin (printf "No headers-registered-key: idcheck.request=[~s] idcheck=[~s].~n"
                       (get-cookie/single "idcheck.request" (headers-cookies headers))
                       (get-cookie/single "idcheck" (headers-cookies headers)))
               #f))))

; string -> (U string #f)
;
; Validate key and return personal data, if any
(define (validate-key key)
  (let* ([port
          (get-impure-port
           (string->url
            (string-append (get-idcheck-url)
                           "?version=2.0.9&check_cookie="
                           key)))]
         [headers (purify-port port)]
         [status (parse-status (read-line
                                (open-input-string headers)))])
    (unless (successful? status)
      (close-input-port port)
      (raise-exn:idcheck
       (format "Validation of idcheck key failed with code ~a and reason ~a\n"
               (status-code status)
               (status-reason status))))
    (begin0
      (aif result (cut string=? <> "BAD") (port->string port)
           (begin (printf "IDCheck returned BAD.~n")
                  #f)
           result)
      (close-input-port port))))

; request -> request
(define (idcheck-login request)
  (with-handlers ([exn:fail:network?
                   (lambda (exn) 
                     (raise-exn:idcheck 
                      (format "Could not connect to IDCheck service. Reason: ~a"
                              (exn-message exn))))])
    (let ([headers (ws:request-headers request)])
      (cond [(validated? headers)
             (let ((key (headers-registered-key headers)))
               (if (validate-key key)
                   (if (lookup-user key)
                       request
                       (idcheck-login (clear-cookies request)))
                   (preregister+login)))]
            [(unregistered? headers)
             (preregister+login)]
            [(unvalidated? headers)
             (let ((prereg-key (headers-preregistered-key headers)))
               (if (validate-key prereg-key)
                   (set-cookie+redirect request)
                   (preregister+login)))]
            [else (pretty-print (list "IDCheck error: request was:"
                                      (cons 'method    (request-method    request))
                                      (cons 'URL       (url->string (request-uri request)))
                                      (cons 'headers   (request-headers   request))
                                      (cons 'bindings  (request-bindings  request))
                                      (cons 'post-data (request-post-data request))
                                      (cons 'host-ip   (request-host-ip   request))
                                      (cons 'host-port (request-host-port request))
                                      (cons 'client-ip (request-client-ip request))))
                  (raise-exn:idcheck "Should not get here.")]))))

; request -> request
(define (idcheck-logout request)
  (send/forward
   (lambda (url)
     (aif key (get-key request)
          (remove-user! key)
          (void))
     (my-redirect-to (string-append (get-idcheck-redirect-url) "?do=logout")
                     `((set-cookie . ,(print-cookie (clear-private-cookie)))
                       (set-cookie . ,(print-cookie (clear-idcheck-cookie))))))))

; request -> request
(define (clear-cookies request)
  (send/forward
   (lambda (url)
     (aif key (get-key request)
          (remove-user! key)
          (void))
     (my-redirect-to url `((set-cookie . ,(print-cookie (clear-private-cookie)))
                           (set-cookie . ,(print-cookie (clear-idcheck-cookie))))))))

; -> request
(define (preregister+login)
  (let ((request
         (send/forward
          (lambda (url)
            (let* ((key (preregister (string-append (get-base-url) url)))
                   (cookie (set-idcheck-cookie key)))
              (my-redirect-to
               (get-idcheck-redirect-url)
               `((set-cookie . ,(print-cookie cookie)))))))))
    (set-cookie+redirect request)))

; request -> request
(define (set-cookie+redirect request)
  (send/forward
   (lambda (url)
     (let ((prereg-key
            (headers-preregistered-key (ws:request-headers request))))
       (aif personal-data (validate-key prereg-key)
            (begin
              (add-user! prereg-key personal-data)
              (my-redirect-to
               url
               `((set-cookie
                  .
                  ,(print-cookie
                    (set-private-cookie prereg-key)))
                 (set-cookie
                  .
                  ,(print-cookie
                    (clear-idcheck-cookie))))))
            (preregister+login))))))

; string -> string
;
; Generate a preregistration key from IDCheck
(define (preregister url)
  (define (url->hexurl url)
    (apply string-append
           (string-fold-right
            (lambda (char seed)
              (cons (number->string (char->integer char) 16)
                    seed))
            null
            url)))
  (let* ([port
          (get-impure-port
           (string->url
            (string-append (get-idcheck-url)
                           "?preregister=idcheck:2.0.9:"
                           (url->hexurl url))))]
         [headers (purify-port port)]
         [status  (parse-status (read-line (open-input-string headers)))])
    (unless (successful? status)
      (close-input-port port)
      (raise-exn:idcheck
       (format "Preregistration request to idcheck server failed with code ~a and reason ~a\n"
               (status-code status)
               (status-reason status))))
    (let ([response (read-line port)])
      (close-input-port port)
      (if (preregistration-key? response)
          response
          (raise-exn:idcheck
           (format "Preregistration response ~a is not the correct format\n" response))))))

; Provide statements -----------------------------

(provide exn:idcheck?
         exn:idcheck
         lookup-user
         add-user!
         remove-user!
         get-username)

(provide/contract
 [idcheck-url                        (parameter/c (or/c string? false/c))]
 [idcheck-redirect-url               (parameter/c (or/c string? false/c))]
 [base-url                           (parameter/c (or/c string? false/c))]
 [idcheck-cookie-domain              (parameter/c (or/c string? false/c))]
 [idcheck-send/forward               (parameter/c procedure?)]
 [idcheck-login                      (-> ws:request? ws:request?)]
 [idcheck-logout                     (-> ws:request? ws:request?)]
 [get-key                            (-> ws:request? (or/c string? false/c))]
 [validate-key                       (-> string? (or/c string? false/c))]
 [preregister                        (-> string? string?)]
 [with-idcheck-authenticated-request (-> ws:request? (-> ws:request? any) any)])
