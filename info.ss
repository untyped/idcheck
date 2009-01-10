#lang setup/infotab

(define name "idcheck")

(define blurb
  '((p "Client for the IDCheck distributed authentication system, available from "
       (a ([href "http://idcheck.sourceforge.net"]) "http://idcheck.sourceforge.net") ".")))
  
(define release-notes
  '((p "Changes:")
    (ul (li "updated to SchemeUnit 3;")
        (li "updated to PLT 4.1.3;")
        (li "added more verbose error messages."))))
  
(define url "http://www.untyped.com/")

(define categories '(devtools net))

(define doc.txt "doc.txt")
  
(define primary-file "idcheck.ss")
  
(define required-core-version "4.0")

(define repositories '("4.x"))
