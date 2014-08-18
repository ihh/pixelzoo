;; Reaction-diffusion models allowing for Turing morphogen patterns, among other things
(begin
  (import (srfi 69))

  (define turing-reaction-hash (make-hash-table))

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
       turing-reaction-hash
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

  (define (turing-grammar abcdr-list)
    (map turing-rule abcdr-list))

  (define (turing-max-rate a)
    (let ((astr (turing-key a)))
      (hash-table-fold
       turing-reaction-hash
       (lambda (bstr bhash amax)
	 (max
	  amax
	  (hash-table-fold
	   bhash
	   (lambda (cdstr cdrate abtotal)
	     (+ abtotal (caddr cdrate)))
	   0)))
       0)))

  (define (turing-update-rule a)
    (let* ((astr (turing-key a))
	   (max-rate (turing-max-rate astr)))
      `(adjacent
	(dir 0)
	(next
	 (rule
	  (vector
	   (index 0)
	   (x 1)
	   (y 2)
	   (next
	    (rule
	     ,(indirect-switch-type
	       '(1 2)
	       `(,(hash-table-fold
		   (hash-table-ref turing-reaction-hash astr)
		   (lambda (bstr bhash case-list)
		     (cons
		      `(,bstr
			,(apply-random-switch
			  ,(let* ((rhs-list-and-total-prob
				  (hash-table-fold
				   bhash
				   (lambda (cdstr cdrate accum)
				     (let* ((cstr (car cdrate))
					    (dstr (cadr cdrate))
					    (rate (caddr cdrate))
					    (rhs-list (car accum))
					    (total-prob (cdr accum))
					    (prob (exact->inexact (/ cdrate max-rate))))
				       `(,(cons
					   (list
					    prob
					    (let ((drule
						   (if
						    (equal? dstr bstr)
						    nop-rule
						    (indirect-set-rule
						     '(1 2)
						     dstr))))
					      (if
					       (equal? cstr astr)
					       drule
					       (set-rule
						'(0 0)
						cstr
						drule))))
					   rhs-list) ,(+ total-prob prob))))
				   '(() 1)))
				  (rhs-list (car rhs-list-and-total-prob))
				  (total-prob (cdr rhs-list-and-total-prob)))
			     (cons (list (- 1 total-prob) nop-rule) rhs-list))))
		      case-list))
		   '())))))))))))


  )
