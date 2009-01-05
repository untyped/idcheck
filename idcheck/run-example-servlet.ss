(module run-example-servlet mzscheme
  
  (require (planet "instaweb.ss" ("schematics" "instaweb.plt" 1))
           (lib "etc.ss"))
  
  (print-struct #t)
  
  (instaweb "example-servlet.ss" 5678)
  
  )