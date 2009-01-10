(module idcheck mzscheme
  
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "string.ss" "srfi" "13")
           (lib "servlet.ss" "web-server")
           (lib "cut.ss" "srfi" "26")
           (planet "port.ss" ("schematics" "port.plt" 1))
           (planet "aif.ss" ("schematics" "macro.plt" 1))
           (file "base.ss")
           (file "cookie.ss")
           (file "idcheck-util.ss")
           (file "idcheck-db.ss"))
  
  (provide idcheck-url
           idcheck-redirect-url
           base-url
           idcheck-cookie-domain
           
           exn:idcheck?
           exn:idcheck
           
           lookup-user
           add-user!
           remove-user!
           get-username)
  
  (provide/contract
   [idcheck-login                      (-> request? request?)]
   [idcheck-logout                     (-> request? request?)]
   [get-key                            (-> request? (or/c string? false/c))]
   [validate-key                       (-> string? (or/c string? false/c))]
   [preregister                        (-> string? string?)]
   [with-idcheck-authenticated-request (-> request? (-> request? any) any)])
  
  ; Parameters -----------------------------------
  
  ;; parameter idcheck-url : string
  ;;
  ;; The URL of the IDCheck CGI we communicate with.
  ;;
  ;; Example:
  ;;
  ;;     "http://idcheck.untyped.com/idcheck"
  (define idcheck-url
    (make-parameter #f))
  
  ;; get-idcheck-url : -> string
  ;;
  ;; Retrieves the current value of the idcheck-url parameter,
  ;; and checks to make sure the parameter has been set.
  (define (get-idcheck-url)
    (let ([url (idcheck-url)])
      (if url
          url
          (raise-exn:idcheck
           #<<ENDSTR
The idcheck-url parameter has not been set.
Initialize the parameter with a call to parameterize as follows:

     (parameterize ([idcheck-url "http://idcheck.untyped.com/idcheck"])
       ; Insert code here...
       )
ENDSTR
           ))))
  
  ;; parameter idcheck-redirect-url : string
  ;;
  ;; The URL we redirect people to so they can log in to IDCheck.
  ;; This will normally be the same as idcheck-url, but may be
  ;; different (for example, it may be https instead of http).
  ;;
  ;; Example:
  ;;
  ;;     "https://idcheck.untyped.com/idcheck"
  (define idcheck-redirect-url
    (make-parameter #f))
  
  ;; get-idcheck-redirect-url : -> string
  ;;
  ;; Retrieves the current value of the idcheck-redirect-url parameter,
  ;; and checks to make sure the parameter has been set.
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
  
  ;; parameter base-url : string
  ;;
  ;; The base URL, without a trailing slash, of the server
  ;; running this IDCheck client.
  ;;
  ;; Example:
  ;;
  ;;     "http://www.untyped.com" ; note: no trailing slash
  (define base-url
    (make-parameter #f))
  
  ;; get-base-url : -> string
  ;;
  ;; Retrieves the current value of the base-url parameter,
  ;; and checks to make sure the parameter has been set.
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
  
  ;; with-idcheck-authenticated-request : request (request -> any) -> any
  (define (with-idcheck-authenticated-request request controller)
    (let loop ((request request))
      (aif key (get-key request)
           (if (lookup-user key)
               (controller request)
               (loop (idcheck-login (idcheck-logout request))))
           (loop (idcheck-login request)))))
  
  ;; get-key : request -> (U string #f)
  (define (get-key request)
    (let ((headers (request-headers request)))
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
  
  ;; validate-key : string -> (U string #f)
  ;;
  ;; Validate key and return personal data, if any
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
  
  ;; idcheck-login : request -> request
  (define (idcheck-login request)
    (with-handlers 
        ([exn:fail:network?
          (lambda (exn) 
            (raise-exn:idcheck 
             (format "Could not connect to IDCheck service. Reason: ~a"
                     (exn-message exn))))])
      (let ((headers (request-headers request)))
        (cond [(validated? headers)
               (let ((key (headers-registered-key headers)))
                 (if (validate-key key)
                     (if (lookup-user key)
                         request
                         (idcheck-login (idcheck-logout request)))
                     (preregister+login)))]
              [(unregistered? headers)
               (preregister+login)]
              [(unvalidated? headers)
               (let ((prereg-key (headers-preregistered-key headers)))
                 (if (validate-key prereg-key)
                     (set-cookie+redirect request)
                     (preregister+login)))]
              [else (raise-exn:idcheck "Should not get here.")]))))
  
  ;; idcheck-logout : request -> request
  (define (idcheck-logout request)
    (send/forward
     (lambda (url)
       (aif key (get-key request)
            (remove-user! key)
            (void))
       (my-redirect-to
        url
        `((set-cookie . ,(print-cookie (clear-private-cookie)))
          (set-cookie . ,(print-cookie (clear-idcheck-cookie))))))))
  
  ;; preregister+login : -> request
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
  
  ;; set-cookie+redirect : request -> request
  (define (set-cookie+redirect request)
    (send/forward
     (lambda (url)
       (let ((prereg-key
              (headers-preregistered-key (request-headers request))))
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
  
  ;;- preregister : string -> string
  ;;
  ;; Generate a preregistration key from IDCheck
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
           [status (parse-status (read-line
                                  (open-input-string headers)))])
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
  
  )
 