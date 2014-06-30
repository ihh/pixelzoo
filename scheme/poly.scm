(begin
  ;; Polymers
  ;; Vars:
  ;;  has-fwd-bond (1 bit), fwd-bond-dir (3 bits), has-rev-bond (1 bit), rev-bond-dir (3 bits),
  ;;  build-state (2 bits), tail-state (2 bits), edge-len (5 bits), steps (5 bits), edges (2 bits)
  ;; Start with polymer diffusion dynamics
  ;; First, subrules to verify integrity of upstream and/or downstream bonds, then attempt a move
  ;; Registers:
  ;;  (0,1)  location of fwd-bond cell
  ;;  (2,3)  direction & inverse-direction from origin to fwd-bond cell
  ;;  (4,5)  location of rev-bond cell
  ;;  (6,7)  direction & inverse-direction from origin to rev-bond cell
  ;;  (8,9)  location of target cell for move
  ;; (10,11) direction & inverse-direction from target cell to fwd-bond cell
  ;; (12,13) direction & inverse-direction from target cell to rev-bond cell

  (define polymer-has-fwd-bond-var "has-fwd-bond")
  (define polymer-has-rev-bond-var "has-rev-bond")
  (define polymer-fwd-bond-dir-var "fwd-bond-dir")
  (define polymer-rev-bond-dir-var "rev-bond-dir")

  (define polymer-build-state-var "build-state")
  (define polymer-tail-state-var "tail-state")
  (define polymer-edge-len-var "edge-len")
  (define polymer-steps-var "steps")
  (define polymer-edges-var "edges")

  (define (polymer-move-self)
    (indirect-move-self '(8 9)))

  (define (polymer-update-f next)
    (set-var-from-register
     origin self-type polymer-fwd-bond-dir-var 10
     (indirect-set-var-from-register
      '(0 1) self-type polymer-rev-bond-dir-var 11
      next)))

  (define (polymer-update-r next)
    (set-var-from-register
     origin self-type polymer-rev-bond-dir-var 12
     (indirect-set-var-from-register
      '(4 5) self-type polymer-fwd-bond-dir-var 13
      next)))

  (define (polymer-move-f)
    (indirect-switch-type
     '(8 9) `((,empty-type ,(polymer-update-f polymer-move-self)))))

  (define (polymer-move-r)
    (indirect-switch-type
     '(8 9) `((,empty-type ,(polymer-update-r polymer-move-self)))))

  (define (polymer-move-fr)
    (indirect-switch-type
     '(8 9) `((,empty-type ,(polymer-update-f
			     (lambda () (polymer-update-r polymer-move-self)))))))

  (define (polymer-detach-f)
    (set-var origin self-type polymer-has-fwd-bond-var 0))

  (define (polymer-detach-r)
    (set-var origin self-type polymer-has-rev-bond-var 0))

  (define (polymer-verify-f next)
    (indirect-switch-type
     '(0 1) `((,self-type
	       ,(indirect-switch-var
		 '(0 1) self-type polymer-has-rev-bond-var
		 `((0 ,polymer-detach-f)
		   (1 ,(indirect-compare-var-to-register
			'(0 1) self-type polymer-rev-bond-dir-var 3
			`(neq (rule ,(polymer-detach-f)))
			`(eq (rule ,(eval-or-return next)))))))))
     polymer-detach-f))

  (define (polymer-verify-r next)
    (indirect-switch-type
     '(4 5) `((,self-type
	       ,(indirect-switch-var
		 '(4 5) self-type polymer-has-fwd-bond-var
		 `((0 ,polymer-detach-r)
		   (1 ,(indirect-compare-var-to-register
			'(4 5) self-type polymer-fwd-bond-dir-var 7
			`(neq (rule ,(polymer-detach-r)))
			`(eq (rule ,(eval-or-return next)))))))))
     polymer-detach-r))

  (define (polymer-fr-just-verify-subrule-name) (string-append self-type ".verify.fr"))
  (define (polymer-fr-subrule-name) (string-append self-type ".fr"))
  (define (polymer-f-subrule-name) (string-append self-type ".f"))
  (define (polymer-r-subrule-name) (string-append self-type ".r"))

  (define (polymer-fr-just-verify-subrule)
    (subrule
     (polymer-fr-just-verify-subrule-name)
     (polymer-verify-f
      (lambda () (polymer-verify-r nop-rule)))))

  (define (polymer-fr-subrule)
    (subrule
     (polymer-fr-subrule-name)
     (polymer-verify-f
      (lambda () (polymer-verify-r polymer-move-fr)))))

  (define (polymer-f-subrule)
    (subrule
     (polymer-f-subrule-name)
     (polymer-verify-f polymer-move-f)))

  (define (polymer-r-subrule)
    (subrule
     (polymer-r-subrule-name)
     (polymer-verify-r polymer-move-r)))

  (define (polymer-grep-nbr-dirs dir candidate-nbr-dirs)
    (grep (lambda (nbr-dir) (moore-neighbor? (loc-minus (moore-loc dir) (moore-loc nbr-dir)))) candidate-nbr-dirs))

  (define (polymer-nbr-dirs dir)
    (polymer-grep-nbr-dirs dir moore-dirs))

  (define (polymer-nbr2-dirs dir1 dir2)
    (polymer-grep-nbr-dirs dir1 (polymer-grep-nbr-dirs dir2 moore-dirs)))

  ;; procedure to load registers and perform goto
  (define (polymer-load-reg fwd-dir rev-dir move-dir goto-label)
    (let* ((fwd-loc (moore-loc fwd-dir))
	   (rev-loc (moore-loc rev-dir))
	   (move-loc (moore-loc move-dir))
	   (inv-fwd-dir (moore-back fwd-dir))
	   (inv-rev-dir (moore-back rev-dir))
	   (new-fwd-dir (moore-relative-dir move-dir fwd-dir))
	   (new-rev-dir (moore-relative-dir move-dir rev-dir))
	   (new-inv-fwd-dir (moore-back new-fwd-dir))
	   (new-inv-rev-dir (moore-back new-rev-dir)))
      (load-rule `((0 ,(car fwd-loc))
		   (1 ,(cadr fwd-loc))
		   (2 ,fwd-dir)
		   (3 ,inv-fwd-dir)
		   (4 ,(car rev-loc))
		   (5 ,(cadr rev-loc))
		   (6 ,rev-dir)
		   (7 ,inv-rev-dir)
		   (8 ,(car move-loc))
		   (9 ,(cadr move-loc))
		   (10 ,new-fwd-dir)
		   (11 ,new-inv-fwd-dir)
		   (12 ,new-rev-dir)
		   (13 ,new-inv-rev-dir))
		 `(goto ,goto-label))))

;; polymer cage builders
;; outline of program:

;; switch (state)
;;  case 0: (paused)
;;   nop

;;  case 1: (init)
;;   set orig.steps = orig.edge_len
;;   set orig.edges = $edges - 1
;;   set orig.tail_state = 0 (paused)
;;   set orig.state = 2 (build)

;;  case 2: (build)
;;   switch (orig.r_dir)  (loop over neighborhood)
;;    if (steps = 0)
;;     if (edges = 0)
;;      bind (r_pos = neighborhood[r_dir])
;;       case polymer:  (connect the ends)
;;        switch (r_pos.state)
;;         case 0: (paused)
;;          set r_pos.state = 3 (active)
;;          set r_pos.l_bond = 1
;;          set r_pos.l_dir = [direction from r_pos to orig]
;;          set orig.state = 3 (active)
;;          set orig.r_bond = 1
;;    else (edges > 0)
;;     set orig.r_dir = orig.r_dir + $turn_angle  (turn right)
;;     set orig.steps = orig.edge_len
;;     set orig.edges = orig.edges - 1
;;     if (orig.edges = 0)
;;      set orig.steps = orig.steps - 1
;;   else (steps > 0)
;;    bind (r_pos = neighborhood[r_dir])
;;     case empty:
;;      set r_pos = orig
;;      set orig.state = orig.tail_state
;;      set orig.r_bond = 1
;;      set r_pos.l_bond = 1
;;      set r_pos.l_dir = [direction from r_pos to orig]
;;      set r_pos.tail_state = 3 (active)
;;      decrement r_pos.steps

;;  case 3: (active)
;;   poly_rule

;; where...
;; poly_rule:
;;  if (l_bond)
;;   verify_or_die (l_dir, r_dir)
;;   if (r_bond)
;;    verify_or_die (r_dir, l_dir)
;;    random_lr_step
;;   else
;;    random_l_step
;;  else
;;   if (r_bond)
;;    verify_or_die (r_dir, l_dir)
;;    random_r_step
;;  else
;;   random_step

  ;; Put it all together
  (define (polymer-move-rule)
    `(rule
      ,(polymer-f-subrule)
      ,(polymer-r-subrule)
      ,(polymer-fr-subrule)
      ,(polymer-fr-just-verify-subrule)
      ,(switch-var
	origin self-type polymer-build-state-var
	`((1 ,(copy-self-var  ;; build-state == 1
	       polymer-edge-len-var polymer-steps-var
	       (set-self-var
		polymer-edges-var 3
		(set-self-var
		 polymer-tail-state-var 0
		 (set-self-var
		  polymer-build-state-var 2)))))

	  (2 ,(switch-var  ;; build-state == 2
	       origin self-type polymer-steps-var
	       `((0 ,(switch-var  ;; build-state == 2, steps == 0
		      origin self-type polymer-edges-var
		      `((0 ,(bind-moore-dir  ;; build-state == 2, steps == 0, edges == 0
			     polymer-fwd-bond-dir-var
			     (lambda (loc dir)
			       (switch-type
				loc
				`((,self-type ,(switch-var
						loc self-type polymer-build-state-var
						`((0 ,(set-var
						       loc self-type polymer-build-state-var 3
						       (set-var
							loc self-type polymer-has-rev-bond-var 1
							(set-var
							 loc self-type polymer-rev-bond-dir-var
							 (moore-back dir)
							 (set-self-var
							  polymer-build-state-var 3
							  (set-self-var polymer-has-fwd-bond-var 1)))))))))))))))
		      (modify-self-var  ;; build-state == 2, steps == 0, edges > 0
		       polymer-fwd-bond-dir-var +2 polymer-fwd-bond-dir-var
		       (copy-self-var
			polymer-edge-len-var polymer-steps-var
			(modify-self-var
			 polymer-edges-var -1 polymer-edges-var
			 (switch-var
			  origin self-type polymer-edges-var
			  `((0 ,(modify-self-var polymer-steps-var -1 polymer-steps-var))))))))))
	       (bind-moore-dir  ;; build-state == 2, steps > 0, edges > 0
		polymer-fwd-bond-dir-var
		(lambda (loc dir)
		  (switch-type
		   loc
		   `((,empty-type ,(copy-self
				    loc
				    (set-var
				     loc self-type polymer-rev-bond-dir-var (moore-back dir)
				     (set-var
				      loc self-type polymer-has-rev-bond-var 1
				      (set-var
				       loc self-type polymer-tail-state-var 3
				       (modify-rule
					loc self-type polymer-steps-var -1 loc self-type polymer-steps-var
					(copy-self-var
					 polymer-tail-state-var polymer-build-state-var
					 (set-self-var
					  polymer-has-fwd-bond-var 1))))))))))))))

	  (3 ,(switch-var  ;; build-state == 3
	       origin self-type polymer-has-fwd-bond-var
	       `((0 ,(switch-var
		      origin self-type polymer-has-rev-bond-var
		      `((0 ,neumann-drift)
			(1 ,(bind-moore-dir
			     polymer-rev-bond-dir-var
			     (lambda (rev-loc rev-dir)
			       (apply-random-switch
				(map
				 (lambda (move-dir)
				   (list 1 (polymer-load-reg 0 rev-dir move-dir (polymer-r-subrule-name))))
				 (polymer-nbr-dirs rev-dir)))))))))
		 (1 ,(bind-moore-dir
		      polymer-fwd-bond-dir-var
		      (lambda (fwd-loc fwd-dir)
			(switch-var
			 origin self-type polymer-has-rev-bond-var
			 `((0 ,(apply-random-switch
				(map
				 (lambda (move-dir)
				   (list 1 (polymer-load-reg fwd-dir 0 move-dir (polymer-f-subrule-name))))
				 (polymer-nbr-dirs fwd-dir))))
			   (1 ,(bind-moore-dir
				polymer-rev-bond-dir-var
				(lambda (rev-loc rev-dir)
				  (let ((move-dirs (polymer-nbr2-dirs fwd-dir rev-dir)))
				    (if
				     (null? move-dirs)
				     (polymer-load-reg fwd-dir rev-dir 0 (polymer-fr-just-verify-subrule-name))
				     (apply-random-switch
				      (map
				       (lambda (move-dir)
					 (list 1 (polymer-load-reg fwd-dir rev-dir move-dir (polymer-fr-subrule-name))))
				       move-dirs)))))))))))))))))))

  (define (polymer-particle name)
    `(particle
      (name ,name)
      (vars
       (varsize (name ,polymer-has-fwd-bond-var) (size 1))
       (varsize (name ,polymer-fwd-bond-dir-var) (size 3))
       (varsize (name ,polymer-has-rev-bond-var) (size 1))
       (varsize (name ,polymer-rev-bond-dir-var) (size 3))
       (varsize (name ,polymer-build-state-var) (size 2))
       (varsize (name ,polymer-tail-state-var) (size 2))
       (varsize (name ,polymer-edge-len-var) (size 5))
       (varsize (name ,polymer-steps-var) (size 5))
       (varsize (name ,polymer-edges-var) (size 2)))

      (colrule (var ,polymer-edges-var) (hexmul "20000"))
      (colrule (var ,polymer-steps-var) (hexmul "80000") (hexinc "14ffff"))
      (rule (scheme "(polymer-move-rule)")))))
