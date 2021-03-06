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

  (define rna-fwd-antiparallel-suffix "-fap")
  (define rna-rev-antiparallel-suffix "-rap")
  (define rna-min-loop-suffix "-loop")

  (define rna-merge-mismatch-prob .01)
  (define rna-split-prob .1)

  (define rna-got-complement-subrule-name "rna-got-complement")
  (define rna-fwd-antiparallel-suffix "-fap")
  (define rna-rev-antiparallel-suffix "-rap")
  (define rna-min-loop-suffix "-loop")
  (define rna-merge-subrule-name "rna-merge")
  (define rna-split-subrule-prefix "rna-split")
  (define rna-step-ss-subrule-prefix "rna-ss-step")
  (define rna-step-ds-subrule-prefix "rna-ds-step")
  (define rna-move-subrule-name "rna-move")

  (define (rna-particle name)
    (rna-param-particle name .01 .1))

  (define (rna-param-particle name merge-mismatch-prob split-prob)
    (begin
      (set! rna-got-complement-subrule-name (string-append name "-got-complement"))
      (set! rna-merge-subrule-name (string-append name "-merge"))
      (set! rna-split-subrule-prefix (string-append name "-split"))
      (set! rna-step-ss-subrule-prefix (string-append name "-ss-step"))
      (set! rna-step-ds-subrule-prefix (string-append name "-ds-step"))
      (set! rna-move-subrule-name (string-append name "-move"))

      (set! rna-merge-mismatch-prob merge-mismatch-prob)
      (set! rna-split-prob split-prob)

      `(particle
	(name ,name)
	(vars
	 (varsize (name ,rna-has-fs-bond-var) (size 2))  ;; offset: 0
	 (varsize (name ,rna-fs-bond-dir-var) (size 3))  ;; offset: 2
	 (varsize (name ,rna-has-rs-bond-var) (size 2))  ;; offset: 5
	 (varsize (name ,rna-rs-bond-dir-var) (size 3))  ;; offset: 7
	 (varsize (name ,rna-has-fa-bond-var) (size 2))  ;; offset: 10
	 (varsize (name ,rna-fa-bond-dir-var) (size 3))  ;; offset: 12
	 (varsize (name ,rna-has-ra-bond-var) (size 2))  ;; offset: 15
	 (varsize (name ,rna-ra-bond-dir-var) (size 3))  ;; offset: 17
	 (varsize (name ,rna-sense-base-var) (size 2))   ;; offset: 20
	 (varsize (name ,rna-anti-base-var) (size 2))    ;; offset: 22
	 (varsize (name ,rna-has-anti-var) (size 1)))    ;; offset: 24

	(colrule (var ,rna-anti-base-var) (hexmul "e0000"))
	(colrule (var ,rna-sense-base-var) (hexmul "500000"))
	(colrule (var ,rna-has-anti-var) (hexmul "ff7fffff") (hexinc "14bfff"))

	,moore-particle-neighborhood

	(rule (scheme "(rna-move-rule)")))))

  ;; helper functions for iterating over all possible combinations of bonds (a "cascade")
  (define (rna-ds-or-as-cascade cascade-func final-func antisense-func-maker init-args)
    (apply
     (rna-as-cascade-func
      cascade-func
      (antisense-func-maker
       (rna-ss-cascade-func cascade-func final-func)))
     init-args))

  (define (rna-ds-cascade cascade-func final-func init-args)
    (rna-ds-or-as-cascade cascade-func final-func (lambda (ss-cascade) ss-cascade) init-args))

  (define (rna-diverted-ds-cascade cascade-func final-func divert-prob divert-arg0 init-args)
    (rna-ds-or-as-cascade
     cascade-func final-func
     (lambda (ss-cascade-func)
       (lambda args
	 (apply-random-switch
	  `((,divert-prob ,(apply final-func (cons divert-arg0 (cdr args))))
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
      2
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
     1
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
      ,@(rna-ds-cascade
	rna-random-step-subrule-cascade
	rna-make-random-step-rule
	`(,rna-step-ds-subrule-prefix "" 1 ()))  ;; double-stranded drift

      ,@(rna-sense-cascade
	rna-random-step-subrule-cascade
	rna-make-step-and-merge-rules
	`(,rna-step-ss-subrule-prefix "" 0 ()))  ;; single-stranded drift or merge

      ,@(rna-antisense-cascade
	rna-random-step-subrule-cascade
	rna-make-split-rule
	`(,rna-split-subrule-prefix "" 1 ()))  ;; double-stranded split

      ;; latter part of main rule
      ,(subrule
	rna-got-complement-subrule-name
	(switch-var
	 origin self-type rna-has-anti-var
	 `((0 
	    ,(rna-sense-cascade
	      rna-bond-cascade rna-drift-rule `(,rna-step-ss-subrule-prefix "" ())))
	   (1
	    ,(rna-diverted-ds-cascade
	      rna-bond-cascade rna-drift-rule
	      rna-split-prob rna-split-subrule-prefix
	      `(,rna-step-ds-subrule-prefix "" ()))))))

      ;; main rule
      ,(subrule
	rna-move-subrule-name
	(switch-var
	 origin self-type rna-sense-base-var
	 (map
	  (lambda (sense-base)
	    `(,sense-base
	      ,(load-rule
		`((26 ,(- 3 sense-base)))  ;; complement of BASE is 3-BASE
		`(goto ,rna-got-complement-subrule-name))))
	  (iota 4))))

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
       `(rule
	 (vector
	  (index ,dir-reg)
	  (x ,x-reg)
	  (y ,y-reg)
	  (inv ,invdir-reg)
	  (next
	   (rule
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
			  (rule
			   ,(if
			     (= goes-to-anti 1)
			     (indirect-switch-var
			      loc-reg self-type
			      rna-has-anti-var
			      `((1 ,add-bond-and-proceed))
			      erase-bond)  ;; called if partner has-anti-var is 0 and we're an anti slot
			     add-bond-and-proceed)))
			`(neq (rule ,erase-bond)))))  ;; called if partner bond direction doesn't point back to us
		   erase-bond)))  ;; called if partner has-bond-var doesn't point to our s/a slot
	      erase-bond))))))))  ;; called if partner is not a polymer


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
	 (rule
	  (vector
	   (index 27)
	   (x 24)
	   (y 25)
	   (next
	    (rule
	     ,(rna-compute-angles
	       confirmed-bond-list
	       `(goto ,rule-name))))))))))

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
	 (next (rule ,(rna-compute-angles rest-of-confirmed-bond-list next-rule)))))))

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
	`(,@(next-in-cascade
	     subrule-prefix
	     (string-append subrule-suffix tag goes-to-sense-tag)
	     self-has-anti
	     (cons (list has-bond-var bond-dir-var sense-partner-has-bond-var sense-partner-bond-dir-var merged-bond-dir-var bond-base-reg)
		   confirmed-bond-reg-list))
	  ,@(next-in-cascade
	     subrule-prefix
	     (string-append subrule-suffix tag goes-to-anti-tag)
	     self-has-anti
	     (cons (list has-bond-var bond-dir-var anti-partner-has-bond-var anti-partner-bond-dir-var merged-bond-dir-var bond-base-reg)
		   confirmed-bond-reg-list))
	  ,@(next-in-cascade subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list)))))

  ;; the function at the bottom of the drift rule cascade
  (define (rna-make-random-step-rule subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list)
    (let* ((subrule-name (string-append subrule-prefix subrule-suffix))
	   (qualified-merge-subrule-name (string-append rna-merge-subrule-name subrule-suffix)))
      (list
	(subrule
	 subrule-name
	 (indirect-switch-type  ;; Switch on move target
	  '(24 25)
	  `((,empty-type   ;; (empty) move into it
	     ,(rna-reorient-bonds confirmed-bond-reg-list (indirect-move-self '(24 25))))
	    (,self-type    ;; (contains RNA)
	     ,(if
	       (= self-has-anti 1)
	       nop-rule
	       ;; TODO: allow unbonded particles to form new bonds
	       (indirect-switch-var
		'(24 25) self-type rna-has-anti-var
		`((0   ;; (no antisense base in source or target cell)
		   ,(indirect-compare-var-to-register
		     '(24 25) self-type rna-sense-base-var 26
		     `(eq (rule (goto ,qualified-merge-subrule-name)))  ;; complementary: merge
		     `(neq  ;; non-complementary: merge with probability rna-merge-mismatch-prob
		       (rule
			,(random-rule
			  rna-merge-mismatch-prob
			  `(goto ,qualified-merge-subrule-name))))))))))))))))

  (define (rna-make-step-and-merge-rules subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list)
    (let* ((merge-subrule (string-append rna-merge-subrule-name subrule-suffix))
	   (merge-subrule-fwd-antiparallel (string-append merge-subrule rna-fwd-antiparallel-suffix))
	   (merge-subrule-rev-antiparallel (string-append merge-subrule rna-rev-antiparallel-suffix))
	   (has-bond
	    (lambda (has-bond-var)
	      (not (null? (grep (lambda (bond) (string=? (car bond) has-bond-var)) confirmed-bond-reg-list)))))
	   (self-has-rs (has-bond rna-has-rs-bond-var))
	   (self-has-fs (has-bond rna-has-fs-bond-var)))
      (append
       (list
       	;; final merge subrule
	(subrule
	 merge-subrule-rev-antiparallel  ;; we get here after checking that rev bonds are antiparallel
	 (copy-self-var-to-indirect
	  self-type rna-sense-base-var '(24 25) self-type rna-anti-base-var
	  (copy-self-var-to-indirect
	   self-type rna-has-fs-bond-var '(24 25) self-type rna-has-fa-bond-var
	   (copy-self-var-to-indirect
	    self-type rna-has-rs-bond-var '(24 25) self-type rna-has-ra-bond-var
	    (indirect-set-var
	     '(24 25) self-type rna-has-anti-var 1
	     (rna-merge-or-split-bonds confirmed-bond-reg-list 2 kill-self))))))

	;; subrule to check rev bonds are antiparallel
	(subrule
	 merge-subrule-fwd-antiparallel  ;; we get here after checking that fwd bonds are antiparallel
	 (if
	  self-has-rs
	  (indirect-switch-var
	   '(24 25) self-type
	   rna-has-rs-bond-var
	   `((0 (goto ,merge-subrule-rev-antiparallel)))
	   (indirect-compare-var-to-register
	    '(24 25) self-type rna-rs-bond-dir-var 10 ;; don't merge if target-rs-dir == new-target-ra-dir
	    `(neq (rule (goto ,merge-subrule-rev-antiparallel)))))
	  `(goto ,merge-subrule-rev-antiparallel)))

	;; subrule to check fwd bonds are antiparallel
	(subrule
	 merge-subrule  ;; this is the main entry point
	 (if
	  self-has-fs
	  (indirect-switch-var
	   '(24 25) self-type
	   rna-has-fs-bond-var
	   `((0 (goto ,merge-subrule-fwd-antiparallel)))
	   (indirect-compare-var-to-register
	    '(24 25) self-type rna-fs-bond-dir-var 4 ;; don't merge if target-fs-dir == new-target-fa-dir
	    `(neq (rule (goto ,merge-subrule-fwd-antiparallel)))))
	  `(goto ,merge-subrule-fwd-antiparallel))))

       ;; random step subrule
       (rna-make-random-step-rule subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list))))
    
  ;; the function at the antisense diversion point. Generates a split rule
  (define (rna-make-split-rule subrule-prefix subrule-suffix self-has-anti confirmed-bond-reg-list)
    (let* ((subrule-name (string-append subrule-prefix subrule-suffix)))
      (list
       (subrule
	subrule-name
	(indirect-switch-type  ;; Switch on move target
	 '(24 25)
	 `((,empty-type   ;; (empty) split into it
	    ,(indirect-set-rule
	      '(24 25) self-type
	      `((,rna-has-anti-var 0) (,rna-has-fa-bond-var 0) (,rna-has-ra-bond-var 0))
	      (copy-self-var-to-indirect
	       self-type rna-anti-base-var '(24 25) self-type rna-sense-base-var
	       (copy-self-var-to-indirect
		self-type rna-has-fa-bond-var '(24 25) self-type rna-has-fs-bond-var
		(copy-self-var-to-indirect
		 self-type rna-has-ra-bond-var '(24 25) self-type rna-has-rs-bond-var
		 (rna-merge-or-split-bonds
		  confirmed-bond-reg-list
		  1
		  (set-self-var
		   rna-anti-base-var 0
		   (set-self-var
		    rna-has-anti-var 0
		    (set-self-var
		     rna-has-fa-bond-var 0
		     (set-self-var
		      rna-has-ra-bond-var 0))))))))))))))))

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
  (define (rna-merge-or-split-bonds confirmed-bond-reg-list partner-has-bond-val next-rule)
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
	    (partner-loc (list bond-base-reg (+ bond-base-reg 1)))
	    (merge-remaining-bonds
	     (rna-merge-or-split-bonds rest-of-confirmed-bond-reg-list partner-has-bond-val next-rule)))
       (indirect-set-var-from-register
	'(24 25)
	self-type merged-or-split-bond-dir-var (+ bond-base-reg 4)
	(indirect-set-var-from-register
	 partner-loc
	 self-type partner-bond-dir-var (+ bond-base-reg 5)
	 (indirect-set-var
	  partner-loc self-type
	  partner-has-bond-var partner-has-bond-val
	  merge-remaining-bonds))))))

  ;; RNA tool
  (define rna-alphabet (string->list "acgu"))
  (define (rna-alphabet-index c) (list-index (char-downcase c) rna-alphabet))
  (define (rna-string-to-list str)
    (map rna-alphabet-index (string->list str)))

  (define (rna-tool particle str)
    (let* ((seq (rna-string-to-list str))
	   (len (length seq))
	   (se-dir (moore-dir southeast))
	   (nw-dir (moore-dir northwest)))
    `(tool
      (name ,(string-append "RNA " str))
      (size ,(ceiling-power-of-2 len))
      (reserve ,len)
      (recharge ,len)
      (overwrite (tmask) (gstate "empty"))
      ,(if
	(> len 1)
	`(brush
	  (stamp 1)
	  (center
	   (x ,(floor (/ len 2)))
	   (y ,(floor (/ len 2))))
	  (pattern
	   ,@(map
	      (lambda (pos)
		`(pixel
		  (x ,pos)
		  (y ,pos)
		  ,(gvars-list
		    particle
		    `((,rna-sense-base-var ,(list-ref seq pos))
		      ,@(if
			 (< pos (- len 1))
			 `((,rna-has-fs-bond-var 1)
			   (,rna-fs-bond-dir-var ,se-dir))
			 '())
		      ,@(if
			 (> pos 0)
			 `((,rna-has-rs-bond-var 1)
			   (,rna-rs-bond-dir-var ,nw-dir))
			 '())))))
	      (iota len))))
	(gvars particle `(,rna-sense-base-var ,(car seq)))))))

  ;; For debugging: RNA folded back on itself, confined in a wall sandwich
  ;; Forces quick merging
  (define (rna-sandwich-tool particle str)  ;; str must have EVEN length
    (let* ((seq (rna-string-to-list str))
	   (len (length seq))
	   (e-dir (moore-dir east))
	   (w-dir (moore-dir west))
	   (n-dir (moore-dir north))
	   (s-dir (moore-dir south)))
      `(tool
	(name ,(string-append "Confined " str))
	(size ,(max 4 (ceiling-power-of-2 (/ len 2))))
	(reserve ,(* len 2))
	(recharge ,(* len 2))
	(overwrite (tmask) (gstate "empty"))
	(brush
	 (stamp 1)
	 (center
	  (x ,(/ len 4))
	  (y ,(/ len 4)))
	 (pattern
	  ,@(map
	     (lambda (pos)
	       `(pixel (x ,pos) (y 0) ,(gvars "wall")))
	     (iota (/ len 2)))
	  ,@(map
	     (lambda (pos)
	       `(pixel (x ,pos) (y 3) ,(gvars "wall")))
	     (iota (/ len 2)))
	  ,@(map
	     (lambda (pos)
	       `(pixel
		 (x ,(if (>= pos (/ len 2)) (- (- len pos) 1) pos))
		 (y ,(if (>= pos (/ len 2)) 2 1))
		 ,(gvars-list
		   particle
		   `((,rna-sense-base-var ,(list-ref seq pos))
		     ,@(if
			(< pos (- len 1))
			`((,rna-has-fs-bond-var 1)
			  (,rna-fs-bond-dir-var
			   ,(cond
			     ((< pos (- (/ len 2) 1)) e-dir)
			     ((= pos (- (/ len 2) 1)) s-dir)
			     (else w-dir))))
			'())
		     ,@(if
			(> pos 0)
			`((,rna-has-rs-bond-var 1)
			  (,rna-rs-bond-dir-var
			   ,(cond
			     ((< pos (/ len 2)) w-dir)
			     ((= pos (/ len 2)) n-dir)
			     (else e-dir))))
			'())))))
	     (iota len)))))))

  ;; For debugging: double-stranded RNA
  ;; Forces quick splitting
  (define (dsrna-tool particle str)  ;; str must have ODD length
    (let* ((seq (rna-string-to-list str))
	   (len (length seq))
	   (stem-len (/ (- len 1) 2))
	   (e-dir (moore-dir east))
	   (w-dir (moore-dir west))
	   (n-dir (moore-dir north)))
      `(tool
	(name ,(string-append "dsRNA " str))
	(size ,(ceiling-power-of-2 (+ stem-len 1)))
	(reserve ,(+ stem-len 1))
	(recharge ,(+ stem-len 1))
	(overwrite (tmask) (gstate "empty"))
	(brush
	 (stamp 1)
	 (center
	  (x ,(/ stem-len 2))
	  (y ,(/ stem-len 2)))
	 (pattern
	  ,@(map
	     (lambda (pos)
	       `(pixel
		 (x ,pos)
		 (y 0)
		 ,(gvars-list
		   particle
		   `((,rna-sense-base-var ,(list-ref seq pos))
		     (,rna-anti-base-var ,(list-ref seq (- (- len pos) 1)))
		     (,rna-has-anti-var 1)
		     (,rna-has-fs-bond-var 1)
		     (,rna-fs-bond-dir-var ,e-dir)
		     (,rna-has-ra-bond-var ,(if (= pos (- stem-len 1)) 1 2))
		     (,rna-ra-bond-dir-var ,e-dir)
		     ,@(if
			(> pos 0)
			`((,rna-has-rs-bond-var 1)
			  (,rna-rs-bond-dir-var ,w-dir)
			  (,rna-has-fa-bond-var 2)
			  (,rna-fa-bond-dir-var ,w-dir)))))))
	     (iota stem-len))
	  (pixel
	   (x ,stem-len)
	   (y 0)
	   ,(gvars-list
	     particle
	     `((,rna-sense-base-var ,(list-ref seq stem-len))
	       (,rna-has-fs-bond-var 2)
	       (,rna-fs-bond-dir-var ,w-dir)
	       (,rna-has-rs-bond-var 1)
	       (,rna-rs-bond-dir-var ,w-dir)))))))))

  ) ;; end
