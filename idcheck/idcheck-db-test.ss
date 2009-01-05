(module idcheck-db-test mzscheme
  
  (require (file "idcheck-db.ss")
           (file "test-base.ss"))
  
  (provide idcheck-db-tests)

  (define dummy-key "12345678901234567890123456789012")
  
  (define idcheck-db-tests
    (test-suite
     "All tests for idcheck-db"

     (test-case
      "Added users are found"
      (let ((data "foo"))
        (after
         (add-user! dummy-key data)
         (check-equal? (lookup-user dummy-key)
                       data)
         (remove-user! dummy-key))))

     (test-case
      "Removed users are removed"
      (let ((data "foo"))
        (add-user! dummy-key data)
        (check-equal? (lookup-user dummy-key)
                      data)
        (remove-user! dummy-key)
        (check-false (lookup-user dummy-key))))

     (test-case
      "Non-numeric keys raise an exception"
      (check-exn exn:idcheck?
                 (lambda ()
                   (add-user! "foobar" "foo"))))
     
     (test-case
      "get-username returns username"
      (check-equal?
       (get-username "OK\nusername\nsome stuff\nmorestuff")
       "username"))
     ))
  )
