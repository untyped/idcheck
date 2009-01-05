;; This library implements Cookies as per the Netscape spec.
;; Later specs are not supported by popular but retarded
;; browsers such as Internet Explorer
(module cookie mzscheme

  (require (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "string.ss" "srfi" "13")
           (lib "char-set.ss" "srfi" "14")
           (lib "time.ss" "srfi" "19"))

  (provide
   set-cookie
   cookie:add-domain
   cookie:add-expires
   cookie:add-path
   cookie:secure
   ;; To actually return a cookie (string formated as a cookie):
   print-cookie
   ;; To parse the Cookies header:
   get-cookie
   get-cookie/single
   ;; exceptions
   (struct cookie-error ())
   expires->rfc822-string)

  (define-struct cookie (name value expires path domain secure))
  (define-struct (cookie-error exn) ())

  ;; Netscape Spec:
  ;; Set-Cookie: NAME=VALUE; expires=DATE; path=PATH; domain=DOMAIN_NAME; secure
  (define set-cookie
    (lambda (name value)
      (unless (and (cookie-string? name #f)
                   (cookie-string? value))
        (raise (make-cookie-error (format "Invalid NAME/VALUE pair: ~a / ~a" name value) (current-continuation-marks))))
      (make-cookie name value
                   #f ;; session cookie
                   #f ;; current path
                   #f ;; current domain
                   #f;; normal (non SSL)
                   )))

  ;;!
  ;;
  ;; (function (print-cookie cookie))
  ;;
  ;; (param cookie Cookie-structure "The cookie to return as a string")
  ;;
  ;; Formats the cookie contents in a string ready to be appended to a
  ;; "Set-Cookie: " header, and sent to a client (browser).
  (define print-cookie
    (lambda (cookie)
      (unless (cookie? cookie)
        (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
      (string-join
       (filter (lambda (s)
                 (not (string-null? s)))
               (list (format "~a=~a" (cookie-name cookie) (cookie-value cookie))
                     (let ((e (cookie-expires cookie)))
                       (if e
                           (format "expires=~a" (expires->rfc822-string e))
                           ""))
                     (let ((p (cookie-path cookie))) (if p (format "path=~a" p) ""))
                     (let ((d (cookie-domain cookie))) (if d (format "domain=~a" d) ""))
                     (let ((s (cookie-secure cookie))) (if s "secure" ""))))
       "; ")))


  (define cookie:add-domain
    (lambda (cookie domain)
      (unless (valid-domain? domain)
        (raise (make-cookie-error (format "Invalid domain: ~a" domain) (current-continuation-marks))))
      (unless (cookie? cookie)
        (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
      (set-cookie-domain! cookie domain)
      cookie))

  (define cookie:add-expires
    (lambda (cookie seconds)
      (unless (and (integer? seconds) (not (negative? seconds)))
        (raise (make-cookie-error (format "Invalid Expires for cookie: ~a" seconds) (current-continuation-marks))))
      (unless (cookie? cookie)
        (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
      (set-cookie-expires! cookie seconds)
      cookie))

  (define cookie:add-path
    (lambda (cookie path)
      (unless (string? path)
        (raise (make-cookie-error (format "Invalid path: ~a" path) (current-continuation-marks))))
      (unless (cookie? cookie)
        (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
      (set-cookie-path! cookie path)
      cookie))

  (define cookie:secure
    (lambda (cookie secure?)
      (unless (boolean? secure?)
        (raise (make-cookie-error (format "Invalid argument (boolean expected), received: ~a" secure?) (current-continuation-marks))))
      (unless (cookie? cookie)
        (raise (make-cookie-error (format "Cookie expected, received: ~a" cookie) (current-continuation-marks))))
      (set-cookie-secure! cookie secure?)
      cookie))

  ;; Parsing the Cookie header:

  (define char-set:all-but=
    (char-set-difference char-set:full (string->char-set "=")))

  (define char-set:all-but-semicolon
    (char-set-difference char-set:full (string->char-set ";")))

  ;;!
  ;;
  ;; (function (get-all-results name cookies))
  ;;
  ;; Auxiliar procedure that returns all values associated with
  ;; `name' in the association list (cookies).
  (define get-all-results
    (lambda (name cookies)
      (let loop ((c cookies))
        (cond ((null? c) ())
              (else
               (let ((pair (car c)))
                 (if (string=? name (car pair))
                     ;; found an instance of cookie named `name'
                     (cons (cadr pair) (loop (cdr c)))
                     (loop (cdr c)))))))))

  ;; which tipically looks like: (cookie . "test5=\"5\"; test1=\"1\"; test0=\"0\"; test1=\"20\"")
  ;; note that it can be multi-valued: `test1' has values: "1", and "20".
  ;; Of course, in the same spirit, we only receive the "string content".
  (define get-cookie
    (lambda (name cookies)
      (let ((cookies (map (lambda (p)
                            (map string-trim-both
                                 (string-tokenize p char-set:all-but=)))
                          (string-tokenize cookies char-set:all-but-semicolon))))
        (get-all-results name cookies))))

  ;;!
  ;;
  ;; (function (get-cookie/single name cookies))
  ;;
  ;; (param name String "The name of the cookie we are looking for")
  ;; (param cookies String "The string (from the environment) with the content of the cookie header.")
  ;;
  ;; Returns the first name associated with the cookie named `name', if any, or #f.
  (define get-cookie/single
    (lambda (name cookies)
      (let ((cookies (get-cookie name cookies)))
        (and (not (null? cookies))
             (car cookies)))))

   ;;;;;
  ;; Auxiliar procedures
   ;;;;;

  ;; token          = 1*<any CHAR except CTLs or tspecials>
  ;;
  ;; tspecials      = "(" | ")" | "<" | ">" | "@"
  ;;                | "," | ";" | ":" | "\" | <">
  ;;                | "/" | "[" | "]" | "?" | "="
  ;;                | "{" | "}" | SP | HT
  (define char-set:tspecials
    (char-set-union
     (char-set-difference char-set:punctuation (string->char-set "_."))
     char-set:whitespace))
  (define char-set:control (char-set-union char-set:iso-control
                                           (char-set (integer->char 127))));; DEL
  (define char-set:token (char-set-difference char-set:ascii char-set:tspecials char-set:control))

  ;;!
  ;;
  ;; (function (quoted-string? s))
  ;;
  ;; (param s String "The string to check")
  ;;
  ;; Returns #t only if the string is surrounded by double quotes.  As in:
  ;; quoted-string  = ( <"> *(qdtext) <"> )
  ;; qdtext         = <any TEXT except <">>
  (define quoted-string?
    (lambda (s)
      (and (string=? (string-take s 1) "\"")
           (string=? (string-take-right s 1) "\""))))

  ;;!
  ;;
  ;; (function (cookie-string? s))
  ;;
  ;; (param s String "String to check")
  ;;
  ;; Returns whether this is a valid string to use as the value or the
  ;; name (depending on value?) of an HTTP cookie.
  (define cookie-string?
    (opt-lambda (s (value? #t))
      (unless (string? s)
        (raise (make-cookie-error (format "String expected, received: ~a" s) (current-continuation-marks))))
      (if value?
          ;; value: token | quoted-string
          (or (string-every char-set:token s)
              (quoted-string? s))
          ;; name:  token
          (string-every char-set:token s))))

  ;; Host names as per RFC 1123 and RFC952, more or less, anyway. :-)
  (define char-set:hostname
    (let ((a-z-lowercase (ucs-range->char-set #x61 #x7B))
          (a-z-uppercase (ucs-range->char-set #x41 #x5B)))
      (char-set-adjoin!
       (char-set-union char-set:digit a-z-lowercase a-z-uppercase)
       #\. )))

  (define valid-domain?
    (lambda (dom)
      (and
       ;; Domain must start with a dot (.)
       (string=? (string-take dom 1) ".")
       ;; The rest are tokens-like strings separated by dots
       (string-every char-set:hostname dom)
       (<= (string-length dom) 76))))


  ;;; Utility

  (define (expires->rfc822-string seconds)
    (date->string (time-utc->date
                    (make-time time-utc 0 seconds))
                  "~a, ~d-~b-~Y ~H:~M:~S GMT"))
       

  )
;;; cookie-unit.ss ends here