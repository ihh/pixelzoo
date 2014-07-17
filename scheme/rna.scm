(begin

  ;; RNA
  ;; Vars:
  ;;  has-fwd-sense-bond (1 bit), fwd-sense-bond-dir (3 bits), has-rev-sense-bond (1 bit), rev-sense-bond-dir (3 bits),
  ;;  has-fwd-anti-bond (1 bit), fwd-anti-bond-dir (3 bits), has-rev-anti-bond (1 bit), rev-anti-bond-dir (3 bits)
  ;;  sense-base (2 bits), anti-base (2 bits), has-anti (1 bit)
  ;; Registers:
  ;;  (0,1)  location of fwd-sense-bond cell
  ;;  (2,3)  direction & inverse-direction from origin to fwd-sense-bond cell
  ;;  (4,5)  direction & inverse-direction from target cell to fwd-sense-bond cell
  ;;  (6,7)  location of rev-sense-bond cell
  ;;  (8,9)  direction & inverse-direction from origin to rev-sense-bond cell
  ;; (10,11) direction & inverse-direction from target cell to rev-sense-bond cell
  ;; (12,13) location of fwd-anti-bond cell
  ;; (14,15) direction & inverse-direction from origin to fwd-anti-bond cell
  ;; (16,17) direction & inverse-direction from target cell to fwd-anti-bond cell
  ;; (18,19) location of rev-anti-bond cell
  ;; (20,21) direction & inverse-direction from origin to rev-anti-bond cell
  ;; (22,23) direction & inverse-direction from target cell to rev-anti-bond cell
  ;; (24,25) location of target cell for move
  ;;  26     complement of sense base

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

  (define rna-merge-mismatch-prob .01)

  (define (rna-particle name)
    `(particle
      (name ,name)
      (vars
       (varsize (name ,rna-has-fwd-sense-bond-var) (size 2))
       (varsize (name ,rna-fwd-sense-bond-dir-var) (size 3))
       (varsize (name ,rna-has-rev-sense-bond-var) (size 2))
       (varsize (name ,rna-rev-sense-bond-dir-var) (size 3))
       (varsize (name ,rna-has-fwd-anti-bond-var) (size 2))
       (varsize (name ,rna-fwd-anti-bond-dir-var) (size 3))
       (varsize (name ,rna-has-rev-anti-bond-var) (size 2))
       (varsize (name ,rna-rev-anti-bond-dir-var) (size 3))
       (varsize (name ,rna-sense-base-var) (size 2))
       (varsize (name ,rna-anti-base-var) (size 2))
       (varsize (name ,rna-has-anti-var) (size 1)))

      (colrule (var ,rna-sense-base-var) (hexmul "20000"))
      (colrule (var ,rna-anti-base-var) (hexmul "80000") (hexinc "14ffff"))
;      (rule (scheme "(rna-move-rule)"))
))


  ;; Top level:
  ;; Load register 26 with complement of sense base
  ;; Goto attempt-move

  ;; attempt-move:
  ;; If (has-anti)
  ;;  ...with probability (complementary ? p_detach_match : p_detach_mismatch)...
  ;;    ...select sense/antisense at random...
  ;;    ...call attempt-detach with appropriate var labels
  ;;  else (regular sense+antisense rna-bond-cascade)
  ;; else (sense-only rna-bond-cascade)


  ;; (attempt-detach has-fwd fwd-dir has-rev rev-dir base has-fwd-other fwd-dir-other has-rev-other rev-dir-other base-other)
  ;; ...do an rna-bond-cascade *exclusively for sense or antisense*, creating a candidate-nbr-dirs for that side...
  ;; ...attempt detach, moving into target cell if it is empty.


  ;; helper function for iterating over all possible combinations of bonds (a "cascade")
  ;; argument list of cascade-func is as follows:
  ;;  (cascade-func tag has-bond-var bond-dir-var partner-has-bond-var partner-bond-dir-var bond-base-reg next-func)
  ;; returns a function that takes arg list similar to init-args
  ;; final-func also takes arg list similar to init-args
  (define (do-cascade cascade-func final-func init-args)
    (apply
     (cascade-func
      "-ra"
      rna-has-rev-anti-bond-var
      rna-rev-anti-bond-dir-var
      rna-has-fwd-anti-bond-var
      rna-fwd-anti-bond-dir-var
      18
      (cascade-func
       "-fa"
       rna-has-fwd-anti-bond-var
       rna-fwd-anti-bond-dir-var
       rna-has-rev-anti-bond-var
       rna-rev-anti-bond-dir-var
       12
       (cascade-func
	"-rs"
	rna-has-rev-sense-bond-var
	rna-rev-sense-bond-dir-var
	rna-has-fwd-sense-bond-var
	rna-fwd-sense-bond-dir-var
	6
	(cascade-func
	 "-fs"
	 rna-has-fwd-sense-bond-var
	 rna-fwd-sense-bond-dir-var
	 rna-has-rev-sense-bond-var
	 rna-rev-sense-bond-dir-var
	 0
	 final-func))))
     init-args))

  ;; create the cascade for the top-level switch that tests existence & integrity of bonds
  (define (rna-move-subrule)
    (subrule
     "rna-move-subrule"
     (do-cascade rna-bond-cascade rna-drift-rule `("try-random-step-with-anti" "" () ,moore-dirs))))

  ;; rna-bond-cascade: the function for creating the bond verification cascade
  ;; if has-bond-var is TRUE, verify that the bond is mutual, and add to confirmed-bond-list
  ;; then pass control to next-in-cascade
  (define (rna-bond-cascade
	   tag has-bond-var bond-dir-var partner-has-bond-var partner-bond-dir-var bond-base-reg next-in-cascade)
    (lambda (subrule-prefix subrule-suffix confirmed-bond-list candidate-nbr-dirs)
      (switch-var
       origin self-type bond-var
       `((0 ,(next-in-cascade subrule-prefix subrule-suffix confirmed-bond-list candidate-nbr-dirs))
	 (1 ,(bind-moore-dir
	      bond-dir-var
	      (lambda (loc dir)
		(let* ((inv-dir (moore-back dir)))
		  ;; bond verification goes here
		  (next-in-cascade
		   subrule-prefix
		   (string-append subrule-suffix tag)
		   (cons (list bond-dir-var loc dir inv-dir bond-base-reg) confirmed-bond-list)
		   (grep
		    (lambda (nbr-dir)
		      (moore-neighbor? (loc-minus loc (moore-loc nbr-dir)))) candidate-nbr-dirs))))))))))


  ;; rna-drift-rule(confirmed-bond-list,candidate-nbr-dirs): the rule at the bottom of the bond verification cascade
  ;;  select random neighbor, load registers, jump to appropriate subrule
  (define (rna-drift-rule subrule-prefix subrule-suffix confirmed-bond-list candidate-nbr-dirs)
    (apply-random-switch
     (map
      (lambda (move-dir)
	(let* ((move-loc (moore-loc move-dir)))
	  ((list 1 (lambda ()
		     (load-rule
		      (append
		       `(24 ,(car move-loc))
		       `(25 ,(cadr move-loc))
		       (map (lambda (dirvar-loc-dir-invdir-reg)
			      (let* ((bond-dir-var (car dirvar-loc-dir-invdir))
				     (loc (cadr dirvar-loc-dir-invdir))
				     (dir (caddr dirvar-loc-dir-invdir))
				     (inv-dir (cadddr dirvar-loc-dir-invdir))
				     (bond-base-reg (caddddr dirvar-loc-dir-invdir))
				     (new-dir (moore-dir (loc-minus move-loc loc)))
				     (new-inv-dir (moore-back new-dir)))
				`((,bond-base-reg ,(car loc))
				  (,(+ bond-base-reg 1) ,(cadr loc))
				  (,(+ bond-base-reg 2) ,dir)
				  (,(+ bond-base-reg 3) ,inv-dir)
				  (,(+ bond-base-reg 4) ,new-dir)
				  (,(+ bond-base-reg 5) ,new-inv-dir))))
			    candidate-nbr-dirs))
		      `(goto
			,(string-concatenate
			  subrule-prefix
			  subrule-suffix)))))))))))


  ;; random-step-XX: (where XX is concatenation of all dir-vars for which bonds are present)
  ;;   Switch on move target:
  ;;    (empty) move into it
  ;;    (contains RNA with no anti) merge with probability (complementary ? p_attach_match : p_attach_mismatch)

  ;; random-step-subrule-cascade: the function for creating the various drift rules
  (define (random-step-subrule-cascade
	   tag has-bond-var bond-dir-var partner-has-bond-var partner-bond-dir-var bond-base-reg next-in-cascade)
    (lambda (subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list)
      (begin
	(next-in-cascade
	 subrule-prefix
	 (string-append subrule-suffix tag)
	 self-has-anti
	 (cons (list has-bond-var bond-dir-var partner-has-bond-var partner-bond-dir-var bond-base-reg)
	       confirmed-bond-reg-list))
	(next-in-cascade subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list))))

  ;; cascade to define drift rules & associated subrules
  (do-cascade random-step-subrule-cascade make-random-step-rule `("try-random-step-ds" "" 1 ()))  ;; single-stranded
  (do-cascade random-step-subrule-cascade make-random-step-rule `("try-random-step-ss" "" 0 ()))  ;; double-stranded

  ;; the function at the bottom of the drift rule cascade
  (define (make-random-step-rule subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list)
    (let* ((subrule-name (string-append subrule-prefix subrule-suffix)))
      (begin
	(subrule
	 subrule-name
	 (indirect-switch-type  ;; Switch on move target
	  '(24 25)
	  `((,empty-type   ;; (empty) move into it
	     (update-registers confirmed-bond-reg-list (,indirect-move-self '(24 25))))
	    (,self-type    ;; (contains RNA)
	     ,(if
	       ,self-has-anti
	       nop-rule
	       (indirect-switch-var
		'(24 25) self-type rna-has-anti-var
		`(0   ;; (no antisense base in source or target cell)
		  ,(indirect-compare-var-to-register
		    '(24 25) self-type rna-sense-base 26
		    `(eq "rna-merge-subrule")  ;; complementary: merge
		    `(neq  ;; non-complementary: merge with probability rna-merge-mismatch-prob
		      ,(random-rule
			rna-merge-mismatch-prob
			"rna-merge-subrule")))))))))))))

  ;; merge subrule
  (subrule
   "rna-merge-subrule"
   (copy-self-var-to-indirect
    self-type rna-sense-base-var '(24 25) self-type rna-anti-base-var
    (copy-self-var-to-indirect
     self-type rna-has-fwd-sense-bond-var '(24 25) self-type rna-has-fwd-anti-bond-var
     (copy-self-var-to-indirect
      self-type rna-has-rev-sense-bond-var '(24 25) self-type rna-has-rev-anti-bond-var
      (switch-var
       origin self-type rna-has-fwd-sense-bond-var
       `((0 (goto "rna-merge-subrule-test-rev"))
	 (1
	  ,(indirect-set-var-from-register
	    '(24 25) self-type rna-fwd-anti-bond-dir-var 4
	    (indirect-set-var-from-register
	     '(0 1) self-type
;; uh oh. does our fwd-sense bond go to neighbor's rev-sense or neighbor's rev-anti?
;; do we need more vars to track this?
;; that will mean combinatorially more cases to test...
;; i think we in fact need to expand has-fwd-sense, has-rev-sense, has-fwd-anti & has-rev-anti to 2-bit vars
;;  0 for no bond, 1 for bond-to-sense, 2 for bond-to-anti
;; need to update do-cascade to pass in vars for both possibilities

  ;; function to update bond vars in a drift move
  (define (update-registers confirmed-bond-reg-list next-rule)
    (if
     (null? confirmed-bond-reg-list)
     next-rule
     (let* ((confirmed-bond (car confirmed-bond-reg-list))
	    (has-bond-var (car confirmed-bond))
	    (bond-dir-var (cadr confirmed-bond))
	    (partner-has-bond-var (caddr confirmed-bond))
	    (partner-bond-dir-var (cadddr confirmed-bond))
	    (bond-base-reg (caddddr confirmed-bond))
	    (rest-of-confirmed-bond-reg-list (cdr confirmed-bond-reg-list)))
       (update-registers
	rest-of-confirmed-bond-reg-list
	(set-var-from-register
	 origin self-type bond-dir-var (+ bond-base-reg 4)
	 (indirect-set-var-from-register
	  (list bond-base-reg (+ bond-base-reg 1))
	  self-type partner-bond-dir-var (+ bond-base-reg 5)
	  next-rule))))))

  ;; Once this is working, need to add phosphorylation state & allow formation/destruction of bonds......
  



)
