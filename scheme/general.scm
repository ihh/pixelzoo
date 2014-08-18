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

  (define north2 '(0 -2))
  (define east2 '(2 0))
  (define south2 '(0 2))
  (define west2 '(-2 0))

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

  (define hex-neighborhood (list southwest west north northeast east south))
  (define vhex-neighborhood (list northwest north east southeast south west))
  (define diag-neighborhood (list northwest northeast southeast southwest))

  (define neumann2-neighborhood
    (list
     north2 east2 south2 west2
     northwest northeast southeast southwest
     north east south west))

  (define (map-neumann f) (map f neumann-neighborhood))
  (define (map-moore f) (map f moore-neighborhood))

  (define neumann-dirs (iota (length neumann-neighborhood)))
  (define moore-dirs (iota (length moore-neighborhood)))

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

  ;; particle neighborhoods
  (define
    (make-particle-neighborhood-loc loc)
    (xy 'neighbor loc))

  (define
    (make-particle-neighborhood neighborhood)
    `(hood
      ,@(map make-particle-neighborhood-loc neighborhood)))

  (define moore-particle-neighborhood (make-particle-neighborhood moore-neighborhood))
  (define neumann-particle-neighborhood (make-particle-neighborhood neumann-neighborhood))

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
	  ,@(map (lambda (var-val) `(val (@ (var ,(car var-val))) ,(cadr var-val))) var-val-list))))

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
      ,@(map (lambda (reg-val) `(register (index ,(car reg-val)) (state ,(cadr reg-val)))) reg-val-list)
      (next ,(rule-eval-or-return next))))

  ;; modify rule
  (define (modify-rule src-loc src-type src-var inc dest-loc dest-type dest-var . next)
    `(modify
      ,(xy 'src src-loc)
      (vsrcmask (type ,src-type) (var ,src-var))
      (vrshift (type ,src-type) (var ,src-var))
      (inc ,inc)
      ,(xy 'dest dest-loc)
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

  ;; indirect copy own var
  (define (copy-self-var-to-indirect src-type src-var dest-loc dest-type dest-var . next)
    `(modify
      ,(xy 'src '(0 0))
      (vsrcmask (type ,src-type) (var ,src-var))
      (vrshift (type ,src-type) (var ,src-var))
      (inc 0)
      ,(xy-indirect 'dest dest-loc)
      (vlshift (type ,dest-type) (var ,dest-var))
      (vdestmask (type ,dest-type) (var ,dest-var))
      ,@(listform-opt-rule 'next next)))

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

  ;; indirect set var
  (define (indirect-set-var loc type var val . next)
    `(modify
      (srcmask 0)
      (inc ,val)
      (vlshift (type ,type) (var ,var))
      (vdestmask (type ,type) (var ,var))
      ,(xy-indirect 'dest loc)
      ,@(listform-opt-rule 'next next)))

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

  ;; indirect get register from var
  (define (indirect-get-register-from-var loc type var reg next)
    `(switch
      ,(xy-indirect 'pos loc)
      (vmask (type ,type) (var ,var))
      (vrshift (type ,type) (var ,var))
      (index ,reg)
      (default ,next)))

  ;; get register from var
  (define (get-register-from-var loc type var reg next)
    `(switch
      ,(xy 'pos loc)
      (vmask (type ,type) (var ,var))
      (vrshift (type ,type) (var ,var))
      (index ,reg)
      (default ,next)))

  ;; switch rule for types
  (define (switch-type loc type-case-list . default)
    `(switch
      ,(xy 'pos loc)
      (mask ,type-mask)
      (rshift ,type-shift)
      ,@(map (lambda (type-case) `(case (gtype ,(car type-case)) ,(rule-eval-or-return (cadr type-case)))) type-case-list)
      ,@(listform-opt-rule 'default default)))

  ;; switch rule for vars
  (define (switch-var loc type var val-case-list . default)
    `(switch
      ,(xy 'pos loc)
      (vmask (type ,type) (var ,var))
      (vrshift (type ,type) (var ,var))
      ,@(map (lambda (val-case) `(case (state ,(car val-case)) ,(rule-eval-or-return (cadr val-case)))) val-case-list)
      ,@(listform-opt-rule 'default default)))

  ;; indirect switch rule for types
  (define (indirect-switch-type loc type-case-list . default)
    `(switch
      ,(xy-indirect 'pos loc)
      (mask ,type-mask)
      (rshift ,type-shift)
      ,@(map (lambda (type-case) `(case (gtype ,(car type-case)) ,(rule-eval-or-return (cadr type-case)))) type-case-list)
      ,@(listform-opt-rule 'default default)))

  ;; indirect switch rule for vars
  (define (indirect-switch-var loc type var val-case-list . default)
    `(switch
      ,(xy-indirect 'pos loc)
      (vmask (type ,type) (var ,var))
      (vrshift (type ,type) (var ,var))
      ,@(map (lambda (val-case) `(case (state ,(car val-case)) ,(rule-eval-or-return (cadr val-case)))) val-case-list)
      ,@(listform-opt-rule 'default default)))

  ;; indirect compare var to register
  (define (indirect-compare-var-to-register loc type var reg . rest)
    `(compare
      ,(xy-indirect 'pos loc)
      (vmask (type ,type) (var ,var))
      (vrshift (type ,type) (var ,var))
      (regindex ,reg)
      ,@rest))

  ;; compare var to register
  (define (compare-var-to-register loc type var reg . rest)
    `(compare
      ,(xy 'pos loc)
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
    (apply drift-rule (cons map-moore rest))))
