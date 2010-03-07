#lang scheme/base

(require (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/exn)
         (planet untyped/unlib:3/list))

; (struct string continuation-marks)
(define-struct (exn:idcheck exn) ())

; (struct string continuation-marks)
(define-struct (exn:idcheck:bad-request exn:idcheck) ())

; Provide statements -----------------------------

(provide (all-from-out (planet untyped/unlib:3/debug)
                       (planet untyped/unlib:3/exn)
                       (planet untyped/unlib:3/list))
         (all-defined-out))

