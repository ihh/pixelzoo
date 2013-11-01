;; ZooGas XML generators
(begin

  ;; general
  (define type-mask "ffff000000000000")
  (define type-shift 48)

  (define state-mask "ffffffffffffffff")

  (define empty-type "empty")
  (define self-type empty-type)

  ;; directions
  (define origin '(0 0))

  (define north '(0 -1))
  (define east '(1 0))
  (define south '(0 1))
  (define west '(-1 0))

  (define northeast '(1 -1))
  (define southeast '(1 1))
  (define southwest '(-1 1))
  (define northwest '(-1 -1))

  ;; (loc-minus loc1 loc2) returns loc2-loc1 as a vector
  (define (loc-minus loc1 loc2)
    (list (- (car loc2) (car loc1)) (- (cadr loc2) (cadr loc1))))

  ;; invert-loc flips the sign of a vector
  (define (invert-loc xy)
    (let ((x (car xy))
	  (y (cadr xy)))
      (list (- 0 x) (- 0 y))))

  ;; neighborhood iterators & functions
  (define neumann-neighborhood (list north east south west))
  (define moore-neighborhood (list north northeast east southeast south southwest west northwest))

  (define (map-neumann f) (map f neumann-neighborhood))
  (define (map-moore f) (map f moore-neighborhood))

  (define (map-neumann-dir f) (map f neumann-dirs))
  (define (map-moore-dir f) (map f moore-dirs))

  (define (map-neighborhood-loc-dir neighborhood f)
    (map (lambda (dir) (f (list-index neighborhood dir) dir)) (iota (length neighborhood))))

  (define (map-neumann-loc-dir f) (map-neighborhood-loc-dir neumann-neighborhood f))
  (define (map-moore-loc-dir f) (map-neighborhood-loc-dir moore-neighborhood f))

  (define (neumann-loc dir) (list-ref neumann-neighborhood dir))
  (define (moore-loc dir) (list-ref moore-neighborhood dir))

  (define (neumann-dir loc) (list-index loc neumann-neighborhood))
  (define (moore-dir loc) (list-index loc moore-neighborhood))

  (define (neumann-neighbor? loc) (>= (neumann-dir loc) 0))
  (define (moore-neighbor? loc) (>= (moore-dir loc) 0))

  (define (moore-relative-dir dir1 dir2)
    (moore-dir (loc-minus (moore-loc dir1) (moore-loc dir2))))

  (define (neumann-rev loc) (list-index (invert-loc loc) neumann-neighborhood))
  (define (moore-rev loc) (list-index (invert-loc loc) moore-neighborhood))

  (define (neumann-rotate . dirs) (modulo (apply + dirs) 4))
  (define (moore-rotate . dirs) (modulo (apply + dirs) 8))

  (define (neumann-left dir) (neumann-rotate -1 dir))
  (define (neumann-right dir) (neumann-rotate 1 dir))
  (define (neumann-back dir) (neumann-rotate 2 dir))

  (define (moore-left-45 dir) (moore-rotate -1 dir))
  (define (moore-left-90 dir) (moore-rotate -2 dir))
  (define (moore-left-135 dir) (moore-rotate -3 dir))
  (define (moore-right-45 dir) (moore-rotate 1 dir))
  (define (moore-right-90 dir) (moore-rotate 2 dir))
  (define (moore-right-135 dir) (moore-rotate 3 dir))
  (define (moore-back dir) (moore-rotate 4 dir))
  (define moore-left moore-left-90)
  (define moore-right moore-right-90)

  ;; addresses
  (define (xy tag loc)
    `(,tag (x ,(car loc)) (y ,(cadr loc))))

  (define (xy-indirect tag loc)
    `(,tag (@ (mode "indirect")) (x ,(car loc)) (y ,(cadr loc))))

  ;; gvars
  (define (gvars . args)
    (let ((type (car args))
	  (var-val-list (cdr args)))
      (gvars-list args var-val-list)))

  (define (gvars-list type var-val-list)
    (if (null? var-val-list)
	`(gstate ,type)
	`(gvars
	  (type ,type)
	  ,@(list (map (lambda (var-val) `(val (@ (var ,(car var-val))) ,(cadr var-val))) var-val-list)))))

  ;; color rule
  ;; (hsb hue saturation brightness)
  (define (hsb hue . rest)
    (let ((sat (opt-arg rest 0 255))
	  (bri (opt-arg rest 1 255)))
      `(colrule (mask 0) (inc ,(+ (* 256 (+ (* 256 hue) sat)) bri)))))

  ;; helpers for optional rule chains
  (define (opt-rule tag arg-or-false)
    (if arg-or-false `((,tag ,(rule-eval-or-return arg-or-false))) '()))

  (define (listform-opt-rule tag arglist-or-null)
    (if (not (null? arglist-or-null)) `((,tag ,(rule-eval-or-return (car arglist-or-null)))) '()))

  (define (rule-eval-or-return arg)
    (rule (eval-or-return arg)))

  ;; rule wrapper
  (define (rule r . rest) `(rule ,r ,@rest))

  ;; nop (dummy) rule
  (define nop-rule
    '(modify (destmask 0)))

  ;; subrules & gotos
  (define (subrule name rule)
    `(subrule (@ (name ,name)) ,(eval-or-return rule)))

  ;; load rules
  (define (load-rule reg-val-list next)
    `(load
      ,(map (lambda (reg-val) `(register (index ,(car reg-val)) (state ,(cadr reg-val)))) reg-val-list)
      (next ,(rule-eval-or-return next))))

  ;; modify rule
  (define (modify-rule src-loc src-type src-var inc dest-loc dest-type dest-var . next)
    `(modify
      ,(xy 'src src-loc)
      (vsrcmask (type ,src-type) (var ,src-var))
      (vrshift (type ,src-type) (var ,src-var))
      (inc ,inc)
      (vlshift (type ,dest-type) (var ,dest-var))
      (vdestmask (type ,dest-type) (var ,dest-var))
      ,@(listform-opt-rule 'next next)))

  ;; copy var
  (define (copy-var src-loc src-type src-var dest-loc dest-type dest-var . next)
    (apply modify-rule (append (list src-loc src-type src-var 0 dest-loc dest-type dest-var) next)))

  ;; modify self
  (define (modify-self-var src-var inc dest-var . next)
    (apply modify-rule (append (list origin self-type src-var inc origin self-type dest-var) next)))

  ;; copy self
  (define (copy-self-var src-var dest-var . next)
    (apply modify-self-var (append (list src-var 0 dest-var) next)))

  ;; set rule
  (define (set-rule loc type . rest)
    (let ((var-val-list (opt-arg rest 0 '()))
	  (next (opt-arg rest 1 #f)))
      `(modify
	(srcmask 0)
	,(gvars-list type var-val-list)
	(lshift 0)
	,(xy 'dest loc)
	,@(opt-rule 'next next))))

  ;; set var
  (define (set-var loc type var val . next)
    `(modify
      (srcmask 0)
      (inc ,val)
      (vlshift (type ,type) (var ,var))
      (vdestmask (type ,type) (var ,var))
      ,(xy 'dest loc)
      ,@(listform-opt-rule 'next next)))

  ;; set self var
  (define (set-self-var var val . next)
    (apply set-var (append (list origin self-type var val) next)))

  ;; indirect set rule
  (define (indirect-set-rule loc type . rest)
    (let ((var-val-list (opt-arg rest 0 '()))
	  (next (opt-arg rest 1 #f)))
      `(modify
	(srcmask 0)
	,(gvars-list type var-val-list)
	(lshift 0)
	,(xy-indirect 'dest loc)
	,@(opt-rule 'next next))))

  ;; indirect set var from register
  (define (indirect-set-var-from-register loc type var reg . next)
    `(modify
      (srcmask 0)
      (reginc ,reg)
      (vlshift (type ,type) (var ,var))
      (vdestmask (type ,type) (var ,var))
      ,(xy-indirect 'dest loc)
      ,@(listform-opt-rule 'next next)))

  ;; set var from register
  (define (set-var-from-register loc type var reg . next)
    `(modify
      (srcmask 0)
      (reginc ,reg)
      (vlshift (type ,type) (var ,var))
      (vdestmask (type ,type) (var ,var))
      ,(xy 'dest loc)
      ,@(listform-opt-rule 'next next)))

  ;; switch rule for types
  (define (switch-type loc type-case-list . default)
    `(switch
      ,(xy 'pos loc)
      (mask ,type-mask)
      (rshift ,type-shift)
      ,(map (lambda (type-case) `(case (gtype ,(car type-case)) ,(rule-eval-or-return (cadr type-case)))) type-case-list)
      ,@(listform-opt-rule 'default default)))

  ;; switch rule for vars
  (define (switch-var loc type var val-case-list . default)
    `(switch
      ,(xy 'pos loc)
      (vmask (type ,type) (var ,var))
      (vrshift (type ,type) (var ,var))
      ,(map (lambda (val-case) `(case (state ,(car val-case)) ,(rule-eval-or-return (cadr val-case)))) val-case-list)
      ,@(listform-opt-rule 'default default)))

  ;; indirect switch rule for types
  (define (indirect-switch-type loc type-case-list . default)
    `(switch
      ,(xy-indirect 'pos loc)
      (mask ,type-mask)
      (rshift ,type-shift)
      ,(map (lambda (type-case) `(case (gtype ,(car type-case)) ,(rule-eval-or-return (cadr type-case)))) type-case-list)
      ,@(listform-opt-rule 'default default)))

  ;; indirect switch rule for vars
  (define (indirect-switch-var loc type var val-case-list . default)
    `(switch
      ,(xy-indirect 'pos loc)
      (vmask (type ,type) (var ,var))
      (vrshift (type ,type) (var ,var))
      ,(map (lambda (val-case) `(case (state ,(car val-case)) ,(rule-eval-or-return (cadr val-case)))) val-case-list)
      ,@(listform-opt-rule 'default default)))

  ;; indirect compare var to register
  (define (indirect-compare-var-to-register loc type var reg . rest)
    `(compare
      ,(xy-indirect 'pos loc)
      (vmask (type ,type) (var ,var))
      (vrshift (type ,type) (var ,var))
      (regindex ,reg)
      ,@rest))

  ;; random rule
  ;; (random-rule prob pass fail)
  (define (random-rule prob pass . fail)
    `(random (prob ,prob) (pass (rule ,pass)) ,@(listform-opt-rule 'fail fail)))

  ;; Convert a probability distribution over rules into a Huffman tree of <random> rules
  (define (prob-rule-cmp a b)
    (numcmp (car a) (car b)))

  (define (prob-rule-quicksort lst)
    (quicksort prob-rule-cmp lst))

  (define (apply-random-switch prob-rule-list)
    (let ((num-rules (length prob-rule-list)))
      (cond
       ((= num-rules 0) nop-rule)
       ((= num-rules 1) (cadar prob-rule-list))
       (else (let* ((sorted-prob-rule-list (prob-rule-quicksort prob-rule-list))
		    (prob-rule1 (car sorted-prob-rule-list))
		    (prob-rule2 (cadr sorted-prob-rule-list))
		    (rest-of-prob-rule-list (cddr sorted-prob-rule-list))
		    (prob1 (car prob-rule1))
		    (rule1 (cadr prob-rule1))
		    (prob2 (car prob-rule2))
		    (rule2 (cadr prob-rule2))
		    (prob1plus2 (+ prob1 prob2))
		    (rule1or2 `(random (prob ,(exact->inexact (/ prob2 prob1plus2))) (pass (rule ,rule2)) (fail (rule ,rule1)))))
	       (apply-random-switch (cons (list prob1plus2 rule1or2) rest-of-prob-rule-list)))))))

  (define (random-switch . prob-rule-list)
    (apply-random-switch prob-rule-list))

  ;; huffman: shorthand for random-switch. A mixture of syntactic sugar & nostalgia (old name for this function...)
  (define huffman random-switch)

  ;; kill & kill-self rules
  (define (kill-rule loc . next)
    (apply set-rule (append (list loc empty-type '()) next)))

  (define (kill-self . next)
    (apply kill-rule (append (list origin) next)))

  (define (suicide-pact loc . next)
    (kill-rule loc (apply kill-self next)))

  ;; Neighborhood bindings
  ;; (bind-neighborhood-dir neighborhood var func)
  ;; Calls (func loc dir) with 'dir' bound to 'var' and 'loc' bound to the corresponding neighborhood cell
  (define (bind-neighborhood-dir neighborhood dir-var func)
    (switch-var
     origin self-type dir-var
     (map (lambda (dir) (list dir (func (list-ref neighborhood dir) dir))) (iota (length neighborhood)))))

  ;; (bind-neighborhood neighborhood var func)
  ;; Calls (func loc) with 'loc' bound to the corresponding neighborhood cell
  (define (bind-neighborhood neighborhood dir-var func)
    (bind-neighborhood-dir neighborhood dir-var (lambda (loc dir) (func loc))))

  ;; (bind-moore var func)
  ;; Calls (func loc) with 'loc' bound to the corresponding Moore-neighborhood cell
  (define (bind-moore dir-var func)
    (bind-neighborhood moore-neighborhood dir-var func))

  ;; (bind-neumann var func)
  ;; Calls (func loc) with 'loc' bound to the corresponding Neumann-neighborhood cell
  (define (bind-neumann dir-var func)
    (bind-neighborhood neumann-neighborhood dir-var func))

  ;; (bind-moore-dir var func)
  ;; Calls (func loc dir) with 'dir' bound to 'var' and 'loc' bound to the corresponding Moore-neighborhood cell
  (define (bind-moore-dir dir-var func)
    (bind-neighborhood-dir moore-neighborhood dir-var func))

  ;; (bind-neumann-dir var func)
  ;; Calls (func loc dir) with 'dir' bound to 'var' and 'loc' bound to the corresponding Neumann-neighborhood cell
  (define (bind-neumann-dir dir-var func)
    (bind-neighborhood-dir neumann-neighborhood dir-var func))

  ;; Copy and move
  (define (copy-rule src dest . next)
    `(modify
      ,(xy 'src src)
      (srcmask ,state-mask)
      (rshift 0)
      (lshift 0)
      (destmask ,state-mask)
      ,(xy 'dest dest)
      ,@(listform-opt-rule 'next next)))

  (define (move-rule src dest . next)
    (copy-rule src dest (apply set-rule (append (list src empty-type '()) next))))

  (define (copy-self dest . next)
    (apply copy-rule (append (list origin dest) next)))

  (define (move-self dest . next)
    (apply move-rule (append (list origin dest) next)))

  ;; indirect copy & move
  (define (indirect-dest-copy-rule src dest . next)
    `(modify
      ,(xy 'src src)
      (srcmask ,state-mask)
      (rshift 0)
      (lshift 0)
      (destmask ,state-mask)
      ,(xy-indirect 'dest dest)
      ,@(listform-opt-rule 'next next)))

  (define (indirect-dest-move-rule src dest . next)
    (indirect-dest-copy-rule src dest (apply set-rule (append (list src empty-type '()) next))))

  (define (indirect-copy-self dest . next)
    (apply indirect-dest-copy-rule (append (list origin dest) next)))

  (define (indirect-move-self dest . next)
    (apply indirect-dest-move-rule (append (list origin dest) next)))

  ;; (if-type dest dest-type func next fail)
  ;; if location dest contains dest-type, do (func dest next), otherwise do (fail dest)
  (define (if-type dest dest-type func . rest)
    (let ((next (opt-arg rest 0 #f))
	  (fail (opt-arg rest 1 #f)))
      (apply switch-type (append
			  (list dest `((,dest-type ,(apply func (list dest next)))))
			  (cond ((procedure? fail) (list (fail dest)))
				(fail (list fail))
				(else '()))))))

  ;; (if-empty dest func next fail)
  ;; if location dest is empty, do (func dest next), otherwise do fail
  (define (if-empty dest func . rest)
    (apply if-type (append (list dest empty-type func) rest)))

  ;; (if-empty-copy-self dest next fail)
  ;; if location dest is empty, do (copy-self dest next), otherwise do fail
  (define (if-empty-copy-self dest . rest)
    (apply if-empty (append (list dest copy-self) rest)))

  ;; (if-empty-move-self dest next fail)
  ;; if location dest is empty, do (move-self dest next), otherwise do fail
  (define (if-empty-move-self dest . rest)
    (apply if-empty (append (list dest move-self) rest)))

  ;; (indirect-if-type dest dest-type func next fail)
  ;; if indirectly-addressed location dest contains dest-type, do (func dest next), otherwise do fail
  (define (indirect-if-type dest dest-type func . rest)
    (let ((next (opt-arg rest 0 #f))
	  (fail (opt-arg rest 1 #f)))
      (apply indirect-switch-type (append (list dest `((,dest-type ,(apply func (cons dest next))))) fail))))

  ;; (indirect-if-empty dest func next fail)
  ;; if location dest is empty, do (func dest next), otherwise do fail
  (define (indirect-if-empty dest func . rest)
    (apply indirect-if-type (append (list dest empty-type func) rest)))

  ;; (indirect-if-empty-copy-self dest next fail)
  ;; if location dest is empty, do (copy-self dest next), otherwise do fail
  (define (indirect-if-empty-copy-self dest . rest)
    (apply indirect-if-empty (append (list dest indirect-copy-self) rest)))

  ;; (indirect-if-empty-move-self dest next fail)
  ;; if location dest is empty, do (move-self dest next), otherwise do fail
  (define (indirect-if-empty-move-self dest . rest)
    (apply indirect-if-empty (append (list dest indirect-move-self) rest)))

  ;; Random walks
  ;; (drift-rule map-neighborhood next fail)
  ;; if random neighborhood location dest is empty, do (move-self dest (next dest)), otherwise do (fail dest)
  ;; next & fail are optional arguments, and can be data instead of functions
  (define (drift-rule map-neighborhood . rest)
    (apply-random-switch
     (map-neighborhood
      (lambda (dest)
	(list 1 (apply if-empty-move-self (cons dest (map (lambda (f) (if (procedure? f) (f dest) f)) rest))))))))

  ;; (neumann-drift next fail)
  (define (neumann-drift . rest)
    (apply drift-rule (cons map-neumann rest)))

  ;; (moore-drift next fail)
  (define (moore-drift . rest)
    (apply drift-rule (cons map-moore rest)))

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
      (rule (scheme "(polymer-move-rule)"))))

  ;; Utility functions.
  ;; Optional arguments
  (define (opt-arg args n default)
    (if (< n (length args))
	(list-ref args n)
	default))

  ;; Simple right fold
  (define (foldr binary-func init lst)
    (cond
     ((null? lst) init)
     ((pair? lst) (let ((fcar (binary-func init (car lst))))  ;; force this first
		    (foldr binary-func fcar (cdr lst))))
     (else init)))

  ;; Simple map
  (define (map unary-func lst)
    (foldr (lambda (x y) (append x (list (unary-func y)))) '() lst))

  ;; Spliced map
  (define (map-spliced unary-func lst)
    (foldr (lambda (x y) (append x (unary-func y))) '() lst))

  ;; Simple grep
  (define (grep predicate lst)
    (let* ((map-function (lambda (x) (if (predicate x) (list x) '()))))
      (map-spliced map-function lst)))

  ;; grep -v
  (define (grep-not-equal x list)
    (grep (lambda (y) (not (equal? x y))) list))

  ;; iota
  (define (iota n)
    (if (= n 0) '() (append (iota (- n 1)) (list (- n 1)))))

  (define neumann-dirs (iota (length neumann-neighborhood)))
  (define moore-dirs (iota (length moore-neighborhood)))

  ;; (eval-or-return f)  ... if f is a function, evaluate; otherwise, return
  (define (eval-or-return f)
    (if (procedure? f) (f) f))

  ;; (as-function f)  ... if f is a function, return f; otherwise, return function returning f
  (define (as-function f)
    (if (procedure? f) f (lambda () f)))

  ;; (as-string s)   ... convert s to string
  (define (as-string s)
    (cond ((string? s) s)
	  ((number? s) (number->string s))
	  ((symbol? s) (symbol->string s))
	  (else "")))


  ;; list helpers
  (define (as-list lst) (if (pair? lst) lst (list lst)))

  (define (append-as-lists lst1 lst2)
    (append (as-list lst1) (as-list lst2)))

  (define (reverse-list lst)
    (define (reverse-list-2 lst acc)
      (if (null? lst)
	  acc
	  (reverse-list-2 (cdr lst) (cons (car lst) acc))))
    (reverse-list-2 lst '()))

  (define (last lst)
    (cond ((null? (cdr lst)) (car lst))
	  (else (last (cdr lst)))))

  (define (list-index e lst)
    (if (null? lst)
	-1
	(if (equal? (car lst) e)
	    0
	    (if (= (list-index e (cdr lst)) -1)
		-1
		(+ 1 (list-index e (cdr lst)))))))

  ;; Quicksort
  ;; (quicksort cmp lst) returns sorted lst
  ;; (cmp a b) should return -1 if a<b, 0 if a=b, +1 if a>b
  (define (qs-pivot cmp l)
    (cond ((null? l) 'done)
	  ((null? (cdr l)) 'done)
	  ((<= (cmp (car l) (cadr l)) 0) (qs-pivot cmp (cdr l)))
	  (#t (car l))))

  (define (qs-partition cmp piv l p1 p2)
    (if (null? l) (list p1 p2)
	(if (< (cmp (car l) piv) 0)
	    (qs-partition cmp piv (cdr l) (cons (car l) p1) p2)
	    (qs-partition cmp piv (cdr l) p1 (cons (car l) p2)))))

  (define (quicksort cmp l)
    (let ((piv (qs-pivot cmp l)))
      (if (equal? piv 'done) l
	  (let ((parts (qs-partition cmp piv l '() '())))
	    (append (quicksort cmp (car parts))
		    (quicksort cmp (cadr parts)))))))

  (define (numcmp a b)
    (if (< a b) -1 (if (> a b) +1 0)))

  ;; convert an SXML S-expression to an XML string

  ;; For example...
  ;;   (sxml->string '(hello (@ (tone "perky") (volume 11)) "world"))
  ;; ...yields the string...
  ;;   "<hello tone=\"perky\" volume=\"11\">world</hello>"

  (define (sxml->string lst)
;    (let ((result (fold-sxml-outer "" lst)))
;      (display result)
;      result))
    (fold-sxml-outer "" lst))

  (define (fold-sxml-outer str lst)
    (cond ((string? lst) (string-append str lst))
	  ((null? lst) str)
	  ((pair? lst)
	   (let ((tag (car lst))
		 (rest (cdr lst)))
	     (if (symbol? tag)
		 (string-append
		  str "<" (symbol->string tag)
		  (if (null? rest)
		      "/>"
		      (let ((fold-rest (lambda (r) (string-append ">" (fold-sxml-inner "" r) "</" (symbol->string tag) ">"))))
			(if (and
			     (pair? rest)
			     (pair? (car rest))
			     (symbol? (caar rest))
			     (string=? (symbol->string (caar rest)) "@"))  ;; SXML attribute list?
			    (string-append (fold-sxml-attrs "" (cdar rest)) (fold-rest (cdr rest)))
			    (fold-rest rest)))))
		 (fold-sxml-inner str lst))))
	  (else (string-append str (as-string lst)))))

  (define (fold-sxml-inner str lst)
    (cond ((string? lst) (string-append str lst))
	  ((null? lst) str)
	  ((pair? lst)
	   (let ((elem (car lst))
		 (rest (cdr lst)))
	     (fold-sxml-inner (fold-sxml-outer str elem) rest)))
	  (else (string-append str (as-string lst)))))

  (define (fold-sxml-attrs str attr-list)
    (if (null? attr-list) str
	(let* ((attr (car attr-list))
	       (name (car attr))
	       (value (cadr attr))
	       (rest (cdr attr-list)))
	  (fold-sxml-attrs
	   (string-append str " " (symbol->string name) "=\"" (as-string value) "\"")
	   rest)))))