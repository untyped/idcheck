(module idcheck-util mzscheme

  (require (lib "plt-match.ss")
           (lib "request-structs.ss" "web-server" "private")
           (lib "response-structs.ss" "web-server" "private")
           (lib "xml.ss" "xml")
           (file "base.ss")
           (file "cookie.ss"))

  (provide idcheck-cookie-domain
           
           preregistration-key?
           registered-key?
           unregistered?
           unvalidated?
           validated?
           successful?
           
           headers-registered-key
           headers-preregistered-key
           my-redirect-to
           
           parse-status
           (struct status (major-version minor-version code reason))
           
           set-idcheck-cookie
           clear-idcheck-cookie
           set-private-cookie
           clear-private-cookie)
  
  ;; idcheck-cookie-domain : (parameter string)
  ;;
  ;; This is the "domain" attribute set on IDCheck cookies. It must be
  ;; a partial domain name, starting with a full stop. For example:
  ;;
  ;;     ".untyped.com"
  ;;
  ;; The parameter must be set to a common suffix of the domain name
  ;; of the local web server and the IDCheck server. For example:
  ;;
  ;;     IDCheck server:    "idcheck.untyped.com"
  ;;     Web server:        "www.untyped.com"
  ;;     Parameter setting: ".untyped.com"
  ;; 
  ;; This parameter must be set before the library is used.
  (define idcheck-cookie-domain (make-parameter #f))
  
  ;; get-idcheck-cookie-domain : -> string
  ;;
  ;; Retrieves the current value of the idcheck-cookie-domain parameter,
  ;; and checks to make sure the parameter has been set.
  (define (get-idcheck-cookie-domain)
    (let ([cookie-domain (idcheck-cookie-domain)])
      (if cookie-domain
          cookie-domain
          (raise-exn:idcheck
           #<<ENDSTR
The idcheck-cookie-domain parameter has not been set.
Initialize the parameter with a call to parameterize as follows:

     (parameterize ([idcheck-cookie-domain ".untyped.com"])
       ; Insert code here...
       )
ENDSTR
           ))))

  ;; preregistration-key? : string -> string
  (define (preregistration-key? key)
    (if (regexp-match #rx"R[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]" key)
        #t
        #f))

  ;; registered-key? : string -> string
  (define (registered-key? key)
    (if (regexp-match #rx"[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]" key)
        #t
        #f))

  ;; headers-cookies : headers -> string
  (define (headers-cookies headers)
    (let ([cookies (assoc-value/default 'cookie headers #"")])
      (if (bytes? cookies)
          (bytes->string/utf-8 cookies)
          cookies)))

  ;; headers-keys : headers -> (values (U string #f) (U string #f))
  (define (headers-keys headers)
    (let* ((cookies (headers-cookies headers))
           (prereg-key
            (get-cookie/single "idcheck.request" cookies))
           (reg-key (get-cookie/single "idcheck" cookies)))
      (values prereg-key reg-key)))

  ;; headers-registered-key : headers -> (U string #f)
  (define (headers-registered-key headers)
    (let-values (((prereg-key reg-key) (headers-keys headers)))
      reg-key))

  ;; headers-preregistered-key : headers -> (U string #f)
  (define (headers-preregistered-key headers)
    (let-values (((prereg-key reg-key) (headers-keys headers)))
      prereg-key))

  ;; unregistered? : headers -> (U #t #f)
  ;;
  ;; True if the user has not registered with IDCheck
  (define (unregistered? headers)
    (let-values (((prereg reg) (headers-keys headers)))
      (and (or (not prereg) (preregistration-key? prereg))
           (not reg))))

  ;; unvalidated? : headers -> (U #t #f)
  ;;
  ;; True if the user has registered with IDCheck, but has
  ;; not been validated
  (define (unvalidated? headers)
    (let-values (((prereg reg) (headers-keys headers)))
      (and prereg (registered-key? prereg) (not reg))))

  ;; validated? : headers -> (U #t #f)
  ;;
  ;; True if the user has been validated with IDCheck
  (define (validated? headers)
    (let-values (((prereg reg) (headers-keys headers)))
      (and (not prereg) reg)))

  ;; Copied from http-client

  ;; Regexp to extract information from an HTTP status line
  ;; Stolen from Neil Van Dyke's HTTPer package
  (define status-regexp
    (regexp "^HTTP/([0-9]+)\\.([0-9]+) +([1-5][0-9][0-9]) +(.*)"))

  ;; successful? : status -> (U #t #f)
  (define (successful? status)
    (let ((code (status-code status)))
      (and (>= code 200) (<= code 299))))

  ;; struct status: Number Number Number String
  (define-struct status
    (major-version minor-version code reason) (make-inspector))

  ;; parse-status : string -> status
  (define (parse-status string)
    (match (regexp-match status-regexp string)
           [(list whole major minor status reason)
            (make-status
             (string->number major)
             (string->number minor)
             (string->number status)
             reason)]
           [_ (raise-exn:idcheck
               (format "Invalid status line: ~a" string))]))

  ;; my-redirect-to : string (alist-of symbol string) -> response
  ;;
  ;; redirect-to in the servlets library doesn't accept
  ;; headers, so we duplicate here
  (define (my-redirect-to url headers)
    (make-response/full
     302
     "Moved temporarily"
     (current-seconds)
     #"text/html"
     (cons (make-header #"Location" (string->bytes/utf-8 url))
           (map (lambda (kvp)
                  (make-header (string->bytes/utf-8 (symbol->string (car kvp)))
                               (string->bytes/utf-8 (cdr kvp))))
                headers))
     (list (xexpr->string
            `(html (head (meta ((http-equiv "refresh") (url ,url)))
                         (title "Redirect to " ,url))
                   (body (p "Redirecting to " (a ([href ,url]) ,url))))))))

  ;; Name of the cookie used to communicate with the IDCheck
  ;; server
  (define idcheck-cookie-name "idcheck.request")

  ;; Name of the cookie visible only to us, used to store
  ;; the IDCheck key
  (define private-cookie-name "idcheck")

  ;; set-idcheck-cookie : string -> cookie
  (define (set-idcheck-cookie value)
    (cookie:add-path
     (cookie:add-expires
      (cookie:add-domain
       (set-cookie idcheck-cookie-name value)
       (get-idcheck-cookie-domain))
      (+ (current-seconds) 480))
     "/"))
  
  ;; clear-idcheck-cookie : () -> cookie
  (define (clear-idcheck-cookie)
    (cookie:add-path
     (cookie:add-expires
      (cookie:add-domain
       (set-cookie idcheck-cookie-name "null")
       (get-idcheck-cookie-domain))
      0)
     "/"))

  ;; set-private-cookie : string -> cookie
  (define (set-private-cookie value)
    (cookie:add-path
     (set-cookie private-cookie-name value)
     "/"))

  ;; clear-private-cookie : () -> cookie
  (define (clear-private-cookie)
    (cookie:add-path
     (cookie:add-expires
      (set-cookie private-cookie-name "null")
      0)
     "/"))
  )
