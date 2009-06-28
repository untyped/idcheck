#lang scheme/base

(require scheme/match
         web-server/http
         xml/xml
         (planet untyped/mirrors:2/plain/util)
         "base.ss"
         "cookie.ss")

; (parameter string)
;
; This is the "domain" attribute set on IDCheck cookies. It must be
; a partial domain name, starting with a full stop. For example:
;
;     ".untyped.com"
;
; The parameter must be set to a common suffix of the domain name
; of the local web server and the IDCheck server. For example:
;
;     IDCheck server:    "idcheck.untyped.com"
;     Web server:        "www.untyped.com"
;     Parameter setting: ".untyped.com"
; 
; This parameter must be set before the library is used.
(define idcheck-cookie-domain (make-parameter #f))

; -> string
;
; Retrieves the current value of the idcheck-cookie-domain parameter,
; and checks to make sure the parameter has been set.
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

; string -> string
(define (preregistration-key? key)
  (and (regexp-match #px"R[0-9]{31}" key) #t))

; string -> string
(define (registered-key? key)
  (and (regexp-match #px"[0-9]{32}" key) #t))

; headers -> string
(define (headers-cookies headers)
  (let ([cookies (assoc-value/default 'cookie headers #"")])
    (if (bytes? cookies)
        (bytes->string/utf-8 cookies)
        cookies)))

; headers -> (values (U string #f) (U string #f))
(define (headers-keys headers)
  (let* ([cookies (headers-cookies headers)]
         [prereg-key (get-cookie/single "idcheck.request" cookies)]
         [reg-key (get-cookie/single "idcheck" cookies)])
    (values prereg-key reg-key)))

; headers -> (U string #f)
(define (headers-registered-key headers)
  (let-values ([(prereg-key reg-key) (headers-keys headers)])
    reg-key))

; headers -> (U string #f)
(define (headers-preregistered-key headers)
  (let-values ([(prereg-key reg-key) (headers-keys headers)])
    prereg-key))

; headers -> (U #t #f)
;
; True if the user has not registered with IDCheck
(define (unregistered? headers)
  (let-values (((prereg reg) (headers-keys headers)))
    (and (or (not prereg) (preregistration-key? prereg))
         (not reg))))

; headers -> (U #t #f)
;
; True if the user has registered with IDCheck, but has
; not been validated
(define (unvalidated? headers)
  (let-values (((prereg reg) (headers-keys headers)))
    (and prereg (registered-key? prereg) (not reg))))

; headers -> (U #t #f)
;
; True if the user has been validated with IDCheck
(define (validated? headers)
  (let-values (((prereg reg) (headers-keys headers)))
    reg))

; Copied from http-client

; Regexp to extract information from an HTTP status line
; Stolen from Neil Van Dyke's HTTPer package
(define status-regexp
  (regexp "^HTTP/([0-9]+)\\.([0-9]+) +([1-5][0-9][0-9]) +(.*)"))

; status -> (U #t #f)
(define (successful? status)
  (let ((code (status-code status)))
    (and (>= code 200) (<= code 299))))

; (struct Number Number Number String)
(define-struct status (major-version minor-version code reason) #:transparent #:mutable)

; string -> status
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

; string (alistof symbol string) -> response
;
; redirect-to in the servlets library doesn't accept
; headers, so we duplicate here
(define (my-redirect-to url headers)
  (make-response/full
   302
   (string+bytes->message #"Moved temporarily")
   (current-seconds)
   (string+bytes->mime-type #"text/html")
   (cons (make-header #"Location" (string->bytes/utf-8 url))
         (map (lambda (kvp)
                (make-header (string->bytes/utf-8 (symbol->string (car kvp)))
                             (string->bytes/utf-8 (cdr kvp))))
              headers))
   (map string+bytes->content
        (list (xexpr->string
               `(html (head (meta ((http-equiv "refresh") (url ,url)))
                            (title "Redirect to " ,url))
                      (body (p "Redirecting to " (a ([href ,url]) ,url)))))))))

; Name of the cookie used to communicate with the IDCheck server
(define idcheck-cookie-name "idcheck.request")

; Name of the cookie visible only to us, used to store the IDCheck key
(define private-cookie-name "idcheck")

; string -> cookie
(define (set-idcheck-cookie value)
  (cookie:add-path
   (cookie:add-expires
    (cookie:add-domain
     (set-cookie idcheck-cookie-name value)
     (get-idcheck-cookie-domain))
    (+ (current-seconds) 480))
   "/"))

; -> cookie
(define (clear-idcheck-cookie)
  (cookie:add-path
   (cookie:add-expires
    (cookie:add-domain
     (set-cookie idcheck-cookie-name "null")
     (get-idcheck-cookie-domain))
    0)
   "/"))

; string -> cookie
(define (set-private-cookie value)
  (cookie:add-path
   (set-cookie private-cookie-name value)
   "/"))

; -> cookie
(define (clear-private-cookie)
  (cookie:add-path
   (cookie:add-expires
    (set-cookie private-cookie-name "null")
    0)
   "/"))

; Provide statements -----------------------------

(provide idcheck-cookie-domain
         
         preregistration-key?
         registered-key?
         unregistered?
         unvalidated?
         validated?
         successful?
         
         headers-cookies
         headers-registered-key
         headers-preregistered-key
         my-redirect-to
         
         parse-status
         (struct-out status)
         
         set-idcheck-cookie
         clear-idcheck-cookie
         set-private-cookie
         clear-private-cookie)
