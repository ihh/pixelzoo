(begin

  ;; RNA
  ;; Vars:
  ;;  has-fs-bond (1 bit), fs-bond-dir (3 bits), has-rs-bond (1 bit), rs-bond-dir (3 bits),
  ;;  has-fa-bond (1 bit), fa-bond-dir (3 bits), has-ra-bond (1 bit), ra-bond-dir (3 bits)
  ;;  sense-base (2 bits), anti-base (2 bits), has-anti (1 bit)
  ;; Registers:
  ;;  (0,1)  location of fs-bond cell
  ;;  (2,3)  direction & inverse-direction from origin to fs-bond cell
  ;;  (4,5)  direction & inverse-direction from target cell to fs-bond cell
  ;;  (6,7)  location of rs-bond cell
  ;;  (8,9)  direction & inverse-direction from origin to rs-bond cell
  ;; (10,11) direction & inverse-direction from target cell to rs-bond cell
  ;; (12,13) location of fa-bond cell
  ;; (14,15) direction & inverse-direction from origin to fa-bond cell
  ;; (16,17) direction & inverse-direction from target cell to fa-bond cell
  ;; (18,19) location of ra-bond cell
  ;; (20,21) direction & inverse-direction from origin to ra-bond cell
  ;; (22,23) direction & inverse-direction from target cell to ra-bond cell
  ;; (24,25) location of target cell for move
  ;;  26     complement of sense base
  ;;  27     direction from origin to target cell

  (define rna-has-fs-bond-var "has-fs-bond")
  (define rna-has-rs-bond-var "has-rs-bond")
  (define rna-fs-bond-dir-var "fs-bond-dir")
  (define rna-rs-bond-dir-var "rs-bond-dir")

  (define rna-has-fa-bond-var "has-fa-bond")
  (define rna-has-ra-bond-var "has-ra-bond")
  (define rna-fa-bond-dir-var "fa-bond-dir")
  (define rna-ra-bond-dir-var "ra-bond-dir")

  (define rna-sense-base-var "sense-base")
  (define rna-anti-base-var "anti-base")
  (define rna-has-anti-var "has-anti")

  (define rna-merge-mismatch-prob .01)
  (define rna-split-prob .01)

  (define rna-merge-subrule-name "rna-merge-subrule")
  (define rna-move-subrule-name "rna-move-subrule")
  (define rna-ds-move-subrule-name "rna-ds-move-subrule")
  (define rna-ss-move-subrule-name "rna-ss-move-subrule")

  (define rna-step-ss-subrule-prefix "rna-step-ss")
  (define rna-step-ds-subrule-prefix "rna-step-ds")
  (define rna-split-subrule-prefix "rna-split")

  (define (rna-particle name)
    `(particle
      (name ,name)
      (vars
       (varsize (name ,rna-has-fs-bond-var) (size 2))
       (varsize (name ,rna-fs-bond-dir-var) (size 3))
       (varsize (name ,rna-has-rs-bond-var) (size 2))
       (varsize (name ,rna-rs-bond-dir-var) (size 3))
       (varsize (name ,rna-has-fa-bond-var) (size 2))
       (varsize (name ,rna-fa-bond-dir-var) (size 3))
       (varsize (name ,rna-has-ra-bond-var) (size 2))
       (varsize (name ,rna-ra-bond-dir-var) (size 3))
       (varsize (name ,rna-sense-base-var) (size 2))
       (varsize (name ,rna-anti-base-var) (size 2))
       (varsize (name ,rna-has-anti-var) (size 1)))

      ,moore-particle-neighborhood

      (colrule (var ,rna-sense-base-var) (hexmul "20000"))
      (colrule (var ,rna-anti-base-var) (hexmul "80000") (hexinc "14ffff"))

      (rule (scheme "(rna-move-rule)"))))

  ;; helper functions for iterating over all possible combinations of bonds (a "cascade")
  ;; argument list of cascade-func is as follows:
  ;;  (cascade-func tag has-bond-var bond-dir-var partner-has-bond-var partner-bond-dir-var bond-base-reg next-func)
  ;; returns a function that takes arg list similar to init-args
  ;; final-func also takes arg list similar to init-args
  ;; antisense-func-maker takes a function of type cascade-func for the antisense part of the cascade,
  ;; and returns a similar function that can optionally stop at antisense (e.g. to do a split move)
  (define (rna-ds-or-as-cascade cascade-func final-func antisense-func-maker init-args)
    (apply
     (rna-as-cascade-func
      cascade-func
      (antisense-func-maker
       (rna-ss-cascade-func cascade-func final-func)))
     init-args))

  (define (rna-ds-cascade cascade-func final-func init-args)
    (rna-ds-or-as-cascade cascade-func final-func (lambda (ss-cascade) ss-cascade) init-args))

  (define (rna-diverted-ds-cascade cascade-func final-func divert-prob divert-args init-args)
    (rna-ds-or-as-cascade
     cascade-func final-func
     (lambda (ss-cascade-func)
       (lambda args
	 (apply-random-switch
	  `((,divert-prob ,(apply final-func divert-args))
	    (,(- 1 divert-prob) ,(apply ss-cascade-func args))))))
     init-args))

  (define (rna-sense-cascade cascade-func final-func init-args)
    (apply
     (rna-ss-cascade-func cascade-func final-func)
     init-args))

  (define (rna-antisense-cascade cascade-func final-func init-args)
    (apply
     (rna-as-cascade-func cascade-func final-func)
     init-args))

  (define (rna-as-cascade-func cascade-func final-func)
    (cascade-func
     "-ra"
     rna-has-ra-bond-var
     rna-ra-bond-dir-var
     rna-has-fs-bond-var
     rna-fs-bond-dir-var
     rna-has-fa-bond-var
     rna-fa-bond-dir-var
     rna-rs-bond-dir-var
     2
     18
     (cascade-func
      "-fa"
      rna-has-fa-bond-var
      rna-fa-bond-dir-var
      rna-has-rs-bond-var
      rna-rs-bond-dir-var
      rna-has-ra-bond-var
      rna-ra-bond-dir-var
      rna-fs-bond-dir-var
      1
      12
      final-func)))

  (define (rna-ss-cascade-func cascade-func final-func)
    (cascade-func
     "-rs"
     rna-has-rs-bond-var
     rna-rs-bond-dir-var
     rna-has-fs-bond-var
     rna-fs-bond-dir-var
     rna-has-fa-bond-var
     rna-fa-bond-dir-var
     rna-ra-bond-dir-var
     2
     6
     (cascade-func
      "-fs"
      rna-has-fs-bond-var
      rna-fs-bond-dir-var
      rna-has-rs-bond-var
      rna-rs-bond-dir-var
      rna-has-ra-bond-var
      rna-ra-bond-dir-var
      rna-fa-bond-dir-var
      1
      0
      final-func)))

  ;; create the cascade for the top-level rule
  (define (rna-move-rule)

    `(rule

      ;; cascades to define drift rules & associated subrules
      ,(rna-ds-cascade
	rna-random-step-subrule-cascade
	rna-make-random-step-rule
	`(,rna-step-ds-subrule-prefix "" 1 ()))  ;; double-stranded drift

      ,(rna-sense-cascade
	rna-random-step-subrule-cascade
	rna-make-random-step-rule
	`(,rna-step-ss-subrule-prefix "" 0 ()))  ;; single-stranded drift or merge

      ,(rna-antisense-cascade
	rna-random-step-subrule-cascade
	rna-make-split-rule
	`(,rna-split-subrule-prefix "" 1 ()))  ;; double-stranded split

      ;; main rule
      ,(subrule
	rna-move-subrule-name
	(switch-var
	 origin self-type rna-has-anti-var
	 `((0 
	    ,(rna-sense-cascade
	      rna-bond-cascade rna-drift-rule `(,rna-step-ss-subrule-prefix "" ())))
	   (1
	    ,(switch-var
	      origin self-type rna-sense-base-var
	      (map
	       (lambda (sense-base)
		 (load-rule
		  `((26 ,(- 3 sense-base)))
		  `(goto ,rna-ds-move-subrule-name)))
	       (iota 4)))))))

      ;; latter part of main rule
      ,(subrule
	rna-ds-move-subrule-name
	(rna-diverted-ds-cascade
	 rna-bond-cascade rna-drift-rule rna-split-prob
	 `(,rna-split-subrule-prefix "" ())
	 `(,rna-step-ds-subrule-prefix "" ())))

      ;; main goto
      (goto ,rna-move-subrule-name)))

  ;; rna-bond-cascade: the function for creating the bond verification cascade
  ;; if has-bond-var is TRUE, verify that the bond is mutual, and add to confirmed-bond-list
  ;; then pass control to next-in-cascade
  (define (rna-bond-cascade
	   tag
	   has-bond-var bond-dir-var
	   sense-partner-has-bond-var sense-partner-bond-dir-var
	   anti-partner-has-bond-var anti-partner-bond-dir-var
	   merged-bond-dir-var
	   expected-partner-has bond-base-reg next-in-cascade)
    (lambda (subrule-prefix subrule-suffix confirmed-bond-list)
      (switch-var
       origin self-type has-bond-var
       `((0 ,(next-in-cascade subrule-prefix subrule-suffix confirmed-bond-list))
	 (1 ,(rna-bind-and-verify tag 0 has-bond-var bond-dir-var
				  sense-partner-has-bond-var sense-partner-bond-dir-var
				  expected-partner-has bond-base-reg next-in-cascade
				  subrule-prefix subrule-suffix confirmed-bond-list))
	 (2 ,(rna-bind-and-verify tag 1 has-bond-var bond-dir-var
				  anti-partner-has-bond-var anti-partner-bond-dir-var
				  expected-partner-has bond-base-reg next-in-cascade
				  subrule-prefix subrule-suffix confirmed-bond-list))))))

  (define (rna-make-connect-tag goes-to-anti)
    (if (= goes-to-anti 1) "2a" "2s"))

  (define (rna-bind-and-verify tag goes-to-anti has-bond-var bond-dir-var partner-has-bond-var partner-bond-dir-var
			       expected-partner-has-bond-var bond-base-reg next-in-cascade
			       subrule-prefix subrule-suffix confirmed-bond-list)
    (let* ((goes-to-anti-tag (rna-make-connect-tag goes-to-anti))
	   (x-reg bond-base-reg)
	   (y-reg (+ bond-base-reg 1))
	   (dir-reg (+ bond-base-reg 2))
	   (invdir-reg (+ bond-base-reg 3))
	   (loc-reg (list x-reg y-reg))
	   (erase-bond (set-self-var has-bond-var 0))
	   (add-bond-and-proceed
	    (next-in-cascade
	     subrule-prefix
	     (string-append subrule-suffix tag goes-to-anti-tag)
	     (cons bond-base-reg confirmed-bond-list))))
      (get-register-from-var
       origin self-type bond-dir-var dir-reg
       `(vector
	 (index ,dir-reg)
	 (x ,x-reg)
	 (y ,y-reg)
	 (inv ,invdir-reg)
	 (next
	  ,(indirect-switch-type
	    loc-reg
	    `((,self-type
	       ,(indirect-switch-var
		 loc-reg self-type
		 partner-has-bond-var
		 `((,expected-partner-has-bond-var
		    ,(indirect-compare-var-to-register
		      loc-reg self-type partner-bond-dir-var invdir-reg
		      `(eq
			,(if
			  goes-to-anti
			  (indirect-switch-var
			   loc-reg self-type
			   rna-has-anti-var
			   `((1 ,add-bond-and-proceed))
			   erase-bond)  ;; called if partner has-anti-var is 0 and we're an anti slot
			  add-bond-and-proceed))
		      `(neq ,erase-bond))))  ;; called if partner bond direction doesn't point back to us
		 erase-bond)))  ;; called if partner has-bond-var doesn't point to our s/a slot
	    erase-bond))))))  ;; called if partner is not a polymer


  ;; rna-drift-rule(confirmed-bond-list): the rule at the bottom of the bond verification cascade
  ;;  select random neighbor, load registers, jump to appropriate subrule
  (define (rna-drift-rule subrule-prefix subrule-suffix confirmed-bond-list)
    (let* ((rule-name (string-append
		       subrule-prefix
		       subrule-suffix)))

      `(adjacent
	,@(map
	   (lambda (bond-base-reg)
	     (let* ((bond-dir-reg (+ bond-base-reg 2)))
	       `(index ,bond-dir-reg)))
	   confirmed-bond-list)
	(dir 27)
	(next
	 (vector
	  (index 27)
	  (x 24)
	  (y 25)
	  (next
	   ,(rna-compute-angles
	     confirmed-bond-list
	     `(goto ,rule-name))))))))

  (define (rna-compute-angles confirmed-bond-list next-rule)
    (if
     (null? confirmed-bond-list)
     next-rule
     (let* ((bond-base-reg (car confirmed-bond-list))
	    (neighbor-to-origin (+ 3 bond-base-reg))
	    (target-to-neighbor (+ 4 bond-base-reg))
	    (neighbor-to-target (+ 5 bond-base-reg))
	    (rest-of-confirmed-bond-list (cdr confirmed-bond-list)))
       `(vector
	 (index ,neighbor-to-origin)
	 (index 27)  ;; origin-to-target
	 (dir ,neighbor-to-target)
	 (inv ,target-to-neighbor)
	 (next ,(rna-compute-angles rest-of-confirmed-bond-list next-rule))))))

  ;; random-step-XX: (where XX is concatenation of all dir-vars for which bonds are present)
  ;;   Switch on move target:
  ;;    (empty) move into it
  ;;    (contains RNA with no anti) merge with probability (complementary ? 1 : p_merge_mismatch)

  ;; rna-random-step-subrule-cascade: the function for creating the various drift rules
  (define (rna-random-step-subrule-cascade
	   tag
	   has-bond-var bond-dir-var
	   sense-partner-has-bond-var sense-partner-bond-dir-var
	   anti-partner-has-bond-var anti-partner-bond-dir-var
	   merged-bond-dir-var
	   expected-partner-has-bond-var bond-base-reg next-in-cascade)
    (let ((goes-to-anti-tag (rna-make-connect-tag 1))
	  (goes-to-sense-tag (rna-make-connect-tag 0)))
      (lambda (subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list)
	(begin
	  (next-in-cascade
	   subrule-prefix
	   (string-append subrule-suffix tag goes-to-sense-tag)
	   self-has-anti
	   (cons (list has-bond-var bond-dir-var sense-partner-has-bond-var sense-partner-bond-dir-var merged-bond-dir-var bond-base-reg)
		 confirmed-bond-reg-list))
	  (next-in-cascade
	   subrule-prefix
	   (string-append subrule-suffix tag goes-to-anti-tag)
	   self-has-anti
	   (cons (list has-bond-var bond-dir-var anti-partner-has-bond-var anti-partner-bond-dir-var merged-bond-dir-var bond-base-reg)
		 confirmed-bond-reg-list))
	  (next-in-cascade subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list)))))

  ;; the function at the bottom of the drift rule cascade
  (define (rna-make-random-step-rule subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list)
    (let* ((subrule-name (string-append subrule-prefix subrule-suffix))
	   (qualified-merge-subrule-name (string-append rna-merge-subrule-name subrule-suffix)))
      (begin
	(subrule
	 subrule-name
	 (indirect-switch-type  ;; Switch on move target
	  '(24 25)
	  `((,empty-type   ;; (empty) move into it
	     (rna-reorient-bonds confirmed-bond-reg-list (,indirect-move-self '(24 25))))
	    (,self-type    ;; (contains RNA)
	     ,(if
	       self-has-anti
	       nop-rule
	       ;; TODO: allow unbonded particles to form new bonds
	       (indirect-switch-var
		'(24 25) self-type rna-has-anti-var
		`((0   ;; (no antisense base in source or target cell)
		   ,(indirect-compare-var-to-register
		     '(24 25) self-type rna-sense-base-var 26
		     `(eq ,qualified-merge-subrule-name)  ;; complementary: merge
		     `(neq  ;; non-complementary: merge with probability rna-merge-mismatch-prob
		       ,(random-rule
			 rna-merge-mismatch-prob
			 qualified-merge-subrule-name)))))))))))
	;; merge subrule
	(subrule
	 qualified-merge-subrule-name
	 (copy-self-var-to-indirect
	  self-type rna-sense-base-var '(24 25) self-type rna-anti-base-var
	  (copy-self-var-to-indirect
	   self-type rna-has-fs-bond-var '(24 25) self-type rna-has-fa-bond-var
	   (copy-self-var-to-indirect
	    self-type rna-has-rs-bond-var '(24 25) self-type rna-has-ra-bond-var
	    (rna-merge-or-split-bonds confirmed-bond-reg-list nop-rule))))))))

  ;; the function at the antisense diversion point. Generates a split rule
  (define (rna-make-split-rule subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list)
    (indirect-switch-type  ;; Switch on move target
     '(24 25)
     `((,empty-type   ;; (empty) split into it
	(indirect-set-rule
	 '(24 25) self-type
	 '((,rna-has-anti-var 0) (,rna-has-fa-bond-var 0) (,rna-has-ra-bond-var 0))
	 (copy-self-var-to-indirect
	  self-type rna-anti-base-var '(24 25) self-type rna-sense-base-var
	  (copy-self-var-to-indirect
	   self-type rna-has-fa-bond-var '(24 25) self-type rna-has-fs-bond-var
	   (copy-self-var-to-indirect
	    self-type rna-has-ra-bond-var '(24 25) self-type rna-has-rs-bond-var
	    (rna-merge-or-split-bonds confirmed-bond-reg-list nop-rule)))))))))

  ;; helper to update bond vars in a drift move
  (define (rna-reorient-bonds confirmed-bond-reg-list next-rule)
    (if
     (null? confirmed-bond-reg-list)
     next-rule
     (let* ((confirmed-bond (car confirmed-bond-reg-list))
	    (has-bond-var (car confirmed-bond))
	    (bond-dir-var (cadr confirmed-bond))
	    (partner-has-bond-var (caddr confirmed-bond))
	    (partner-bond-dir-var (cadddr confirmed-bond))
	    (merged-bond-dir-var (caddddr confirmed-bond))
	    (bond-base-reg (cadddddr confirmed-bond))
	    (rest-of-confirmed-bond-reg-list (cdr confirmed-bond-reg-list)))
       (rna-reorient-bonds
	rest-of-confirmed-bond-reg-list
	(set-var-from-register
	 origin self-type bond-dir-var (+ bond-base-reg 4)
	 (indirect-set-var-from-register
	  (list bond-base-reg (+ bond-base-reg 1))
	  self-type partner-bond-dir-var (+ bond-base-reg 5)
	  next-rule))))))

  ;; helper to merge/split bonds in a merge/split move
  (define (rna-merge-or-split-bonds confirmed-bond-reg-list next-rule)
    (if
     (null? confirmed-bond-reg-list)
     next-rule
     (let* ((confirmed-bond-reg (car confirmed-bond-reg-list))
	    (has-bond-var (car confirmed-bond-reg))
	    (bond-dir-var (cadr confirmed-bond-reg))
	    (partner-has-bond-var (caddr confirmed-bond-reg))
	    (partner-bond-dir-var (cadddr confirmed-bond-reg))
	    (merged-or-split-bond-dir-var (caddddr confirmed-bond-reg))
	    (bond-base-reg (cadddddr confirmed-bond-reg))
	    (rest-of-confirmed-bond-reg-list (cdr confirmed-bond-reg-list))
	    (merge-remaining-bonds (rna-merge-or-split-bonds rest-of-confirmed-bond-reg-list next-rule)))
       (indirect-set-var-from-register
	'(24 25)
	self-type merged-or-split-bond-dir-var (+ bond-base-reg 4)
	(indirect-set-var-from-register
	 (list bond-base-reg (+ bond-base-reg 1))
	 self-type partner-bond-dir-var (+ bond-base-reg 5))
	merge-remaining-bonds))))

; Uncomment to test memory limitations of RNA program...
;  (define dummy (rna-move-rule))

  ) ;; end
