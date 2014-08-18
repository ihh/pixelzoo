;; Reaction-diffusion models allowing for Turing morphogen patterns, among other things
(begin
  (import (srfi 69))

  (define (turing-reset)
    (set! turing-reaction-hash (make-hash-table))
    (set! turing-hsb-hash (make-hash-table))
    (set! turing-neighborhood-hash (make-hash-table)))

  (turing-reset)

  (define (turing-delete-key)
    (begin
      (hash-table-delete! turing-reaction-hash key)
      (hash-table-delete! turing-hsb-hash key)
      (hash-table-delete! turing-neighborhood-hash key)))

  (define (turing-key x)
    (if
     (string? x)
     x
     (symbol->string x)))

  (define (turing-hsb a . rest)
    (let ((astr (turing-key (car abcdr))))
      (hash-table-set! turing-hsb-hash astr rest)))

  (define (turing-neighborhood a hood)
    (let ((astr (turing-key (car abcdr))))
      (hash-table-set! turing-neighborhood-hash astr hood)))

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
	     (lambda () (list cstr dstr 0))))
	  make-hash-table))
       make-hash-table)))

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

  (define (turing-particle a . rest)
    (let* ((hsb (opt-arg rest 0 0))
	   (neighborhood (opt-arg rest 1 neumann-neighborhood))
	   (abcdr-list (opt-arg rest 2 '()))
	   (astr (turing-key a))
	   (max-rate (turing-max-rate astr)))
      (begin
	(map turing-rule abcdr-list)
	`(particle
	  (name ,astr)
	  ,(apply
	    hsb
	    (hash-table-ref
	     turing-hsb-hash
	     astr
	     (lambda ()
	       (list
		(modulo
		 (foldr + 0 (map char->integer (string->list astr))) ;; hash hue from name
		 256)))))
	  ,(make-particle-neighborhood
	    (hash-table-ref/default turing-neighborhood-hash astr neumann-neighborhood))
	  (rate ,max-rate)
	  (rule ,(turing-update-rule a))))))

  (define (turing-grammar abcdr-list)
    (begin
      (map turing-rule abcdr-list)
      (let* ((turing-keys (hash-table-keys turing-reaction-hash))
	     ((turing-particles (map turing-particle turing-keys))))
	(begin
	  (map turing-delete-key turing-keys)
	  turing-particles))))

  )
