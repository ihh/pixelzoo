;; Reaction-diffusion models allowing for Turing morphogen patterns, among other things
(begin
  (import (srfi 69))

  (define turing-hash (make-hash-table))

  (define (turing-key x)
    (if
     (string? x)
     x
     (symbol->string x)))

  (define (turing-rule abcdr)
    (let ((astr (turing-key (car abcdr)))
	  (bstr (turing-key (cadr abcdr)))
	  (cstr (turing-key (caddr abcdr)))
	  (dstr (turing-key (cadddr abcdr)))
	  (rate (caddddr abcdr))
	  (cdstr (string-append cstr " " dstr)))
      (hash-table-update!
       turing-hash
       astr
       (lambda (ahash)
	 (hash-table-update!
	  ahash
	  bstr
	  (lambda (bhash)
	    (hash-table-update!/default
	     bhash
	     cdstr
	     (lambda (cdrate)
	       (let* ((oldrate (caddr cdrate))
		      (newrate (+ oldrate rate)))
		 (list cstr dstr newrate)))
	     (list cstr dstr 0)))
	  make-hash-table))
       make-hash-table)))

  (define (turing-rate a)
    (let ((astr (turing-key a)))
      (hash-table-fold
       turing-hash
       (lambda (bstr bhash amax)
	 (max
	  amax
	  (hash-table-fold
	   bhash
	   (lambda (cdstr cdrate abtotal)
	     (+ abtotal (caddr cdrate)))
	   0)))
       0)))
  
  )
