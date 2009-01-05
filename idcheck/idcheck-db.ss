(module idcheck-db mzscheme

  (require (lib "contract.ss")
           (file "base.ss")
           (file "idcheck-util.ss"))
  
  (provide/contract
   [add-user!    (-> string? string? any)]
   [remove-user! (-> string? any)]
   [lookup-user  (-> string? (or/c string? false/c))]
   [get-username (-> string? string?)])
  
  ; Variables ------------------------------------
  
  (define db (make-hash-table 'equal))

  ; Public procedures ----------------------------
  
  ;; add-user! : string string -> void
  (define (add-user! key data)
    (hash-table-put!
     db
     (key->number/fail key)
     data))

  ;; remove-user : string -> void
  (define (remove-user! key)
    (hash-table-remove!
     db
     (key->number/fail key)))

  ;; lookup-user : string -> (U string #f)
  (define (lookup-user key)
    (hash-table-get
     db
     (key->number/fail key)
     (lambda ()
       #f)))

  ;; get-username : string -> string
  (define (get-username str)
    (let ([port (open-input-string str)])
      (read-line port 'any)
      (read-line port 'any)))
  
  ; Helpers --------------------------------------

  ;; key->number/fail : string -> number
  (define (key->number/fail str)
    (if (registered-key? str)
        (string->number str)
        (raise-exn:idcheck
         (format "The string ~a is not a valid IDCheck key" str))))
  
  )