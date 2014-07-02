(begin

  ;; RNA
  ;; Vars:
  ;;  has-fwd-sense-bond (1 bit), fwd-sense-bond-dir (3 bits), has-rev-sense-bond (1 bit), rev-sense-bond-dir (3 bits),
  ;;  has-fwd-anti-bond (1 bit), fwd-anti-bond-dir (3 bits), has-rev-anti-bond (1 bit), rev-anti-bond-dir (3 bits)
  ;;  sense-base (2 bits), anti-base (2 bits), has-anti (1 bit)
  ;; Registers:
  ;;  (0,1)  location of fwd-sense-bond cell
  ;;  (2,3)  direction & inverse-direction from origin to fwd-sense-bond cell
  ;;  (4,5)  location of rev-sense-bond cell
  ;;  (6,7)  direction & inverse-direction from origin to rev-sense-bond cell
  ;;  (8,9)  location of fwd-anti-bond cell
  ;; (10,11) direction & inverse-direction from origin to fwd-anti-bond cell
  ;; (12,13) location of rev-anti-bond cell
  ;; (14,15) direction & inverse-direction from origin to rev-anti-bond cell
  ;; (16,17) location of target cell for move
  ;; (18,19) direction & inverse-direction from target cell to fwd-sense-bond cell
  ;; (20,21) direction & inverse-direction from target cell to rev-sense-bond cell
  ;; (22,23) direction & inverse-direction from target cell to fwd-anti-bond cell
  ;; (24,25) direction & inverse-direction from target cell to rev-anti-bond cell

  (define rna-has-fwd-sense-bond-var "has-fwd-sense-bond")
  (define rna-has-rev-sense-bond-var "has-rev-sense-bond")
  (define rna-fwd-sense-bond-dir-var "fwd-sense-bond-dir")
  (define rna-rev-sense-bond-dir-var "rev-sense-bond-dir")

  (define rna-has-fwd-anti-bond-var "has-fwd-anti-bond")
  (define rna-has-rev-anti-bond-var "has-rev-anti-bond")
  (define rna-fwd-anti-bond-dir-var "fwd-anti-bond-dir")
  (define rna-rev-anti-bond-dir-var "rev-anti-bond-dir")

  (define rna-sense-base-var "sense-base")
  (define rna-anti-base-var "anti-base")
  (define rna-has-anti-var "has-anti")

  (define (rna-particle name)
    `(particle
      (name ,name)
      (vars
       (varsize (name ,rna-has-fwd-sense-bond-var) (size 1))
       (varsize (name ,rna-fwd-sense-bond-dir-var) (size 3))
       (varsize (name ,rna-has-rev-sense-bond-var) (size 1))
       (varsize (name ,rna-rev-sense-bond-dir-var) (size 3))
       (varsize (name ,rna-has-fwd-anti-bond-var) (size 1))
       (varsize (name ,rna-fwd-anti-bond-dir-var) (size 3))
       (varsize (name ,rna-has-rev-anti-bond-var) (size 1))
       (varsize (name ,rna-rev-anti-bond-dir-var) (size 3))
       (varsize (name ,rna-sense-base-var) (size 2))
       (varsize (name ,rna-anti-base-var) (size 2))
       (varsize (name ,rna-has-anti-var) (size 1)))

      (colrule (var ,rna-sense-base-var) (hexmul "20000"))
      (colrule (var ,rna-anti-base-var) (hexmul "80000") (hexinc "14ffff"))
;      (rule (scheme "(rna-move-rule)"))
))

  (define (rna-move-subrule)
    (subrule
     "rna-move-subrule"
     (apply
      (rna-bond-cascade
       (rna-has-rev-anti-bond-var
	rna-rev-anti-bond-dir-var
	rna-has-fwd-anti-bond-var
	rna-fwd-anti-bond-dir-var
	12
	(rna-bond-cascade
	 (rna-has-fwd-anti-bond-var
	  rna-fwd-anti-bond-dir-var
	  rna-has-rev-anti-bond-var
	  rna-rev-anti-bond-dir-var
	  8
	  (rna-bond-cascade
	   (rna-has-rev-sense-bond-var
	    rna-rev-sense-bond-dir-var
	    rna-has-fwd-sense-bond-var
	    rna-fwd-sense-bond-dir-var
	    4
	    (rna-bond-cascade
	     (rna-has-fwd-sense-bond-var
	      rna-fwd-sense-bond-dir-var
	      rna-has-rev-sense-bond-var
	      rna-rev-sense-bond-dir-var
	      0
	      rna-drift-rule))))))))
      '() moore-dirs)))

  (define (rna-bond-cascade
	   has-bond-var bond-dir-var partner-has-bond-var partner-bond-dir-var bond-base-reg next-in-cascade)
    (lambda (confirmed-bond-list candidate-nbr-dirs)
      (switch-var
       origin self-type bond-var
       `((0 ,(next-in-cascade confirmed-bond-list candidate-nbr-dirs))
	 (1 ,(bind-moore-dir
	      bond-dir-var
	      (lambda (loc dir)
		(let* ((inv-dir (moore-back dir)))
		  ;; bond verification goes here
		  (next-in-cascade
		   (cons (list bond-dir-var loc dir inv-dir) confirmed-bond-list)
		   (grep
		    (lambda (nbr-dir)
		      (moore-neighbor? (loc-minus loc (moore-loc nbr-dir)))) candidate-nbr-dirs))))))))))

  ;; rna-drift-rule(confirmed-bond-list,candidate-nbr-dirs):
  ;;  select random neighbor, load registers, jump to appropriate random-step-XX subrule


  ;; random-step-XX: (where XX is a name derived from which bonds are present, e.g. random-step-fs-rs)
  ;;   Switch on move target:
  ;;    (empty) move into it
  ;;    (contains RNA with no anti) merge with probability (complementary ? p_attach_match : p_attach_mismatch)

  ;; We also need a top-level test of the form:
  ;; If (has-anti)
  ;;  ...with probability (complementary ? p_detach_match : p_detach_mismatch)...
  ;;    ...select sense/antisense at random...
  ;;    ...do an rna-bond-cascade *exclusively for sense or antisense*, creating a candidate-nbr-dirs for that side...
  ;;    ...attempt detach, moving into target cell if it is empty.
  ;;  else (regular sense+antisense rna-bond-cascade)
  ;; else (sense-only rna-bond-cascade)


  ;; Once this is working, need to add phosphorylation state & allow formation/destruction of bonds......




)
