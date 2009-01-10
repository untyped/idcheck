#lang scheme/base

(require scheme/contract
         "base.ss"
         "idcheck-util.ss")

; Variables ------------------------------------

(define db (make-hash))

; Public procedures ----------------------------

; string string -> void
(define (add-user! key data)
  (hash-set! db (key->number/fail key) data))

; string -> void
(define (remove-user! key)
  (hash-remove! db (key->number/fail key)))

; string -> (U string #f)
(define (lookup-user key)
  (hash-ref db (key->number/fail key) #f))

; string -> string
(define (get-username str)
  (define port (open-input-string str))
  (read-line port 'any)
  (read-line port 'any))

; Helpers --------------------------------------

; string -> number
(define (key->number/fail str)
  (if (registered-key? str)
      (string->number str)
      (raise-exn:idcheck (format "The string ~a is not a valid IDCheck key" str))))

; Provide statements -----------------------------

(provide/contract
 [add-user!    (-> string? string? any)]
 [remove-user! (-> string? any)]
 [lookup-user  (-> string? (or/c string? false/c))]
 [get-username (-> string? string?)])
