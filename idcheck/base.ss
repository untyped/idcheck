#lang scheme/base

(require (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/list))

; (struct string continuation-marks)
(define-struct (exn:idcheck exn) ())

; -> void
(define-syntax raise-exn:idcheck
  (syntax-rules ()
    ((raise-exn-idcheck msg)
     (raise
      (make-exn:idcheck
       (string->immutable-string msg)
       (current-continuation-marks))))))

; Provide statements -----------------------------

(provide (all-from-out (planet untyped/unlib:3/debug)
                       (planet untyped/unlib:3/list))
         (all-defined-out))

