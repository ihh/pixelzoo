;; Reaction-diffusion models allowing for Turing morphogen patterns, among other things
(begin
  (import (srfi 69))

  (define (turing-reset)
    (set! turing-reaction-hash (make-hash-table))
    (set! turing-hsb-hash (make-hash-table))
    (set! turing-neighborhood-hash (make-hash-table))
    (set! turing-vars-hash (make-hash-table)))

  (turing-reset)

  (define (turing-delete-key)
    (begin
      (hash-table-delete! turing-reaction-hash key)
      (hash-table-delete! turing-hsb-hash key)
      (hash-table-delete! turing-neighborhood-hash key)
      (hash-table-delete! turing-vars-hash key)))

  (define (turing-key x)
    (let ((xstr (if
		 (string? x)
		 x
		 (symbol->string x))))
      (if (equal xstr "_") empty-type xstr)))

  (define (turing-hsb a . rest)
    (let ((astr (turing-key a)))
      (hash-table-set! turing-hsb-hash astr rest)))

  (define (turing-neighborhood a hood)
    (let ((astr (turing-key a)))
      (hash-table-set! turing-neighborhood-hash astr hood)))

  (define (turing-var a var size)
    (let ((astr (turing-key a)))
      (hash-table-update!
       turing-vars-hash
       astr
       (lambda (ahash)
	 (hash-table-set!
	  ahash
	  var
	  size))
       make-hash-table)))

  (define (turing-var-exists? a var)
    (let ((astr (turing-key a)))
      (and
       (hash-table-exists? turing-vars-hash astr)
       (hash-table-exists? (hash-table-ref turing-vars-hash astr) var))))

  (define turing-dir-var "dir")
  (define (turing-self-turn delta . rest)
    (apply modify-self-var (append (list turing-dir-var delta turing-dir-var) rest)))
  (define (turing-has-dir? a) (turing-var-exists? a turing-dir-var))

  (define (turing-directed-neighborhood a hood)
    (begin
      (turing-neighborhood a hood)
      (turing-var a turing-dir-var (ceiling-lg2 (length hood)))))

  (define (turing-rule abcdr)
    (let* ((astr (turing-key (car abcdr)))
	   (bstr (turing-key (cadr abcdr)))
	   (cdrate-rest (cddr abcdr))
	   (cstr (turing-key (car cdrate-rest)))
	   (dstr (turing-key (cadr cdrate-rest)))
	   (rate (caddr cdrate-rest))
	   (rest (cdddr cdrate-rest))
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
	     (lambda (old-cdrate-rest)
	       (let* ((oldrate (caddr old-cdrate-rest))
		      (newrate (+ oldrate rate))
		      (oldrest (cdddr old-cdrate-rest)))
		 (append (list cstr dstr newrate) oldrest rest)))
	     cdrate-rest)))
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

  (define (turing-adjacent-selector next)
    `(adjacent
      (dir 0)
      (next
       (rule
	,next)))

  (define (turing-dir-selector next)
    (get-register-from-var origin self-type turing-dir-var 0 next))

  (define (turing-update-rule a . rest)
    (let* ((astr (turing-key a))
	   (neighbor-selector
	    (opt-arg
	     rest 0
	     (if
	      (turing-has-dir? a)
	      turing-dir-selector
	      turing-adjacent-selector)))
	   (max-rate (turing-max-rate astr)))
      (neighbor-selector
       `(vector
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
				  (lambda (cdstr cdrate-rest accum)
				    (let* ((cstr (car cdrate-rest))
					   (dstr (cadr cdrate-rest))
					   (rate (caddr cdrate-rest))
					   (rest (cdddr cdrate-rest))
					   (post-rule (opt-arg rest 0 nop-rule))
					   (rhs-list (car accum))
					   (total-prob (cdr accum))
					   (prob (exact->inexact (/ rate max-rate))))
				      `(,(cons
					  (list
					   prob
					   (let ((drule
						  (cond
						   ((equal? dstr bstr) post-rule)
						   ((equal? dstr ".") post-rule)
						   (else (indirect-set-rule
							  '(1 2)
							  dstr
							  '()
							  (if
							   (turing-has-dir? dstr)
							   (indirect-set-var-from-register '(1 2) dstr turing-dir-var 0 post-rule)
							   post-rule))))))
					     (cond
					      ((equal? cstr astr) drule)
					      ((equal? cstr ".") drule)
					      ((equal? cstr "<") (turing-self-turn -1 drule))
					      ((equal? cstr "<<") (turing-self-turn -2 drule))
					      ((equal? cstr "<<<") (turing-self-turn -3 drule))
					      ((equal? cstr "<<<<") (turing-self-turn -4 drule))
					      ((equal? cstr ">") (turing-self-turn +1 drule))
					      ((equal? cstr ">>") (turing-self-turn +2 drule))
					      ((equal? cstr ">>>") (turing-self-turn +3 drule))
					      ((equal? cstr ">>>>") (turing-self-turn +4 drule))
					      (else
					       (set-rule
						origin
						cstr
						(if
						 (turing-has-dir? dstr)
						 (set-var-from-register origin cstr turing-dir-var 0 drule)
						 drule))))))
					  rhs-list) ,(+ total-prob prob))))
				  '(() 1)))
				(rhs-list (car rhs-list-and-total-prob))
				(total-prob (cdr rhs-list-and-total-prob)))
			   (cons (list (- 1 total-prob) nop-rule) rhs-list))))
		    case-list))
		 '()))))))))))

  (define (turing-particle a . rest)
    (let ((astr (turing-key a)))
      `(particle
	(name ,astr)
	(vars
	 ,@(hash-table-fold
	    (hash-table-ref turing-vars-hash astr make-hash-table)
	    (lambda (varname varbits vars-list)
		(cons
		 `(varsize (name ,varname) (size ,varbits))
		 vars-list))
	    '()))
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
	  (hash-table-ref/default turing-neighborhood-hash astr neighborhood))
	(rate ,(turing-max-rate astr))
	(rule ,(turing-update-rule (cons astr rest))))))

  (define (turing-grammar abcdr-list)
    (begin
      (map turing-rule abcdr-list)
      (let* ((turing-keys (hash-table-keys turing-reaction-hash))
	     ((turing-particles (map turing-particle turing-keys))))
	(begin
	  (map turing-delete-key turing-keys)
	  `(turing-grammar ,@turing-particles)))))

  )
