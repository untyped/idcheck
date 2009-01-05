(module base mzscheme

  (provide (all-defined))
  
  (define-struct (exn:idcheck exn) ())

  (define-syntax raise-exn:idcheck
    (syntax-rules ()
      ((raise-exn-idcheck msg)
       (raise
        (make-exn:idcheck
         (string->immutable-string msg)
         (current-continuation-marks))))))


  ;; --- Copied from Unlib ---
  
  ;; assoc-value/default : any1 (list-of (cons any1 any2)) any2 -> any2 
  ;; 
  ;; Searches for a value by key in a list of key/value pairs
  ;; (an association list). If the key is not found, the default
  ;; value is returned instead.
  (define (assoc-value/default key alist default)
    (let ([kvp (assoc key alist)])
      (if kvp
          (cdr kvp)
          default)))

  )