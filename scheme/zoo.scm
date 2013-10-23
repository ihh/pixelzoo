;; ZooGas XML generators
(begin

  ;; general
  (define type-mask "ffff000000000000")
  (define type-shift 48)

  (define state-mask "ffffffffffffffff")

  (define empty-type "empty")
  (define self-type empty-type)

  ;; locations, neighborhoods, directions
  (define origin '(0 0))

  (define north '(0 -1))
  (define east '(1 0))
  (define south '(0 1))
  (define west '(-1 0))

  (define northeast '(1 -1))
  (define southeast '(1 1))
  (define southwest '(-1 1))
  (define northwest '(-1 -1))

  (define (invert-loc xy)
    (let ((x (car xy))
	  (y (cadr xy)))
      (list (- 0 x) (- 0 y))))

  (define neumann-neighborhood (list north east south west))
  (define moore-neighborhood (list north northeast east southeast south southwest west northwest))

  (define (map-neumann f) (map f neumann-neighborhood))
  (define (map-moore f) (map f moore-neighborhood))

  (define (neumann-loc dir) (list-ref neumann-neighborhood dir))
  (define (moore-loc dir) (list-ref moore-neighborhood dir))

  (define (neumann-dir loc) (list-index loc neumann-neighborhood))
  (define (moore-dir loc) (list-index loc moore-neighborhood))

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

  ;; helpers for optional rule chains
  (define (opt-rule tag arg-or-false)
    (if arg-or-false `((,tag ,(rule-eval-or-return arg-or-false))) '()))

  (define (listform-opt-rule tag arglist-or-null)
    (if (not (null? arglist-or-null)) `((,tag ,(rule-eval-or-return (car arglist-or-null)))) '()))

  (define (rule-eval-or-return arg)
    `(rule ,(eval-or-return arg)))

  ;; set rule
  (define (set-rule loc type . rest)
    (let ((var-val-list (opt-arg rest 0 '()))
	  (next (opt-arg rest 1 #f)))
      `(modify
	(srcmask 0)
	,(xy 'dest loc)
	(lshift 0)
	,(gvars-list type var-val-list)
	,@(opt-rule 'next next))))

  ;; set var
  (define (set-var loc type var val . next)
    `(modify
      (srcmask 0)
      ,(xy 'dest loc)
      (vlshift (type ,type) (var ,var))
      (vlmask (type ,type) (var ,var))
      (inc ,val)
      ,@(listform-opt-rule 'next next)))

  ;; indirect set rule
  (define (indirect-set-rule loc type . rest)
    (let ((var-val-list (opt-arg rest 0 '()))
	  (next (opt-arg rest 1 #f)))
      `(modify
	(srcmask 0)
	,(xy-indirect 'dest loc)
	(lshift 0)
	,(gvars-list type var-val-list)
	,@(opt-rule 'next next))))

  ;; indirect set var from register
  (define (indirect-set-var-from-register loc type var reg . next)
    `(modify
      (srcmask 0)
      ,(xy-indirect 'dest loc)
      (vlshift (type ,type) (var ,var))
      (reginc ,reg)
      ,@(listform-opt-rule 'next next)))

  ;; set var from register
  (define (set-var-from-register loc type var reg . next)
    `(modify
      (srcmask 0)
      ,(xy 'dest loc)
      (vlshift (type ,type) (var ,var))
      (reginc ,reg)
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

  ;; Neighborhood bindings
  (define (bind-neighborhood-dir neighborhood dir-var func)
    (switch-var
     origin self-type dir-var
     (map (lambda (dir) (list dir (func (list-ref neighborhood dir) dir))) (iota (length neighborhood)))))

  (define (bind-neighborhood neighborhood dir-var func)
    (bind-neighborhood-dir neighborhood dir-var (lambda (loc dir) (func loc))))

  (define (bind-moore dir-var func)
    (bind-neighborhood moore-neighborhood dir-var func))

  (define (bind-neumann dir-var func)
    (bind-neighborhood neumann-neighborhood dir-var func))

  (define (bind-moore-dir dir-var func)
    (bind-neighborhood-dir moore-neighborhood dir-var func))

  (define (bind-neumann-dir dir-var func)
    (bind-neighborhood-dir neumann-neighborhood dir-var func))

  ;; Copy and move
  (define (copy-rule src dest . next)
    `(modify
      ,(xy 'src src)
      (srcmask ,state-mask)
      (rshift 0)
      (lshift 0)
      ,(xy 'dest dest)
      (destmask ,state-mask)
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
      ,(xy-indirect 'dest dest)
      (destmask ,state-mask)
      ,@(listform-opt-rule 'next next)))

  (define (indirect-dest-move-rule src dest . next)
    (indirect-dest-copy-rule src dest (apply set-rule (append (list src empty-type '()) next))))

  (define (indirect-copy-self dest . next)
    (apply indirect-dest-copy-rule (append (list origin dest) next)))

  (define (indirect-move-self dest . next)
    (apply indirect-dest-move-rule (append (list origin dest) next)))

  ;; (if-type dest dest-type func next fail)
  ;; if location dest contains dest-type, do (func dest next), otherwise do fail
  (define (if-type dest dest-type func . rest)
    (let ((next (opt-arg rest 0 #f))
	  (fail (opt-arg rest 1 #f)))
      (apply switch-type (append (list dest `((,dest-type ,(apply func (cons dest next))))) fail))))

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

  ;; Random walks
  ;; (drift-rule map-neighborhood next fail)
  ;; if random neighborhood location dest is empty, do (move-self dest (next dest)), otherwise do (fail dest)
  ;; next & fail are optional arguments, and can be data instead of functions
  (define (drift-rule map-neighborhood . rest)
    (map-neighborhood
     (lambda (dest) (apply if-empty-move-self (cons dest (map (lambda (f) (if (procedure? f) (f dest) f)) rest))))))

  ;; (neumann-drift next fail)
  (define (neumann-drift . rest)
    (apply drift-rule (cons map-neumann rest)))

  ;; (moore-drift next fail)
  (define (moore-drift . rest)
    (apply drift-rule (cons map-moore rest)))

  ;; Polymers
  ;; Vars: has-fwd-bond (1 bit), fwd-bond-dir (3 bits), has-rev-bond (1 bit), rev-bond-dir (3 bits)
  ;; First, a subrule that verify integrity of upstream & downstream bonds, then attempt a move
  ;; Registers:
  ;;  (0,1)  location of fwd-bond cell
  ;;  (2,3)  direction & inverse-direction from origin to fwd-bond cell
  ;;  (4,5)  location of rev-bond cell
  ;;  (6,7)  direction & inverse-direction from origin to rev-bond cell
  ;;  (8,9)  location of target cell for move
  ;; (10,11) direction & inverse-direction from target cell to fwd-bond cell
  ;; (12,13) direction & inverse-direction from target cell to rev-bond cell

  (define (polymer-move-self)
    (indirect-move-self '(8 9)))

  (define (polymer-update-f next)
    (set-var-from-register
     origin self-type "fwd-bond-dir" 10
     (indirect-set-var-from-register
      '(0 1) self-type "rev-bond-dir" 11
      next)))

  (define (polymer-update-r next)
    (set-var-from-register
     origin self-type "rev-bond-dir" 12
     (indirect-set-var-from-register
      '(4 5) self-type "fwd-bond-dir" 13
      next)))

  (define (polymer-move-f)
    (polymer-update-f polymer-move-self))

  (define (polymer-move-r)
    (polymer-update-r polymer-move-self))

  (define (polymer-move-fr)
    (polymer-update-f polymer-move-r))

  (define (polymer-detach-f)
    (set-var origin self-type "has-fwd-bond" 0))

  (define (polymer-detach-r)
    (set-var origin self-type "has-rev-bond" 0))

  (define (polymer-verify-f next)
    (indirect-switch-type
     '(0 1) `((,self-type
	       ,(indirect-switch-var
		 '(0 1) self-type "has-rev-bond"
		 `((0 ,polymer-detach-f)
		   (1 ,(indirect-compare-var-to-register
			'(0 1) self-type "rev-bond-dir" 3
			`(neq ,(polymer-detach-f))
			`(eq ,(eval-or-return next))))))))
     polymer-detach-f))

  (define (polymer-verify-r next)
    (indirect-switch-type
     '(4 5) `((,self-type
	       ,(indirect-switch-var
		 '(4 5) self-type "has-fwd-bond"
		 `((0 ,polymer-detach-r)
		   (1 ,(indirect-compare-var-to-register
			'(4 5) self-type "fwd-bond-dir" 7
			`(neq ,(polymer-detach-r))
			`(eq ,(eval-or-return next))))))))
     polymer-detach-r))

  (define (polymer-verify-fr)
    (polymer-verify-f
     (lambda () (polymer-verify-r polymer-move-fr))))

  (define (polymer-move-subrule)
    (switch-var origin self-type "has-fwd-bond"
		`((0 ,(switch-var origin self-type "has-rev-bond"
				  `((0 ,neumann-drift)
				    (1 ,(polymer-verify-r polymer-move-r)))))
		  (1 ,(switch-var origin self-type "has-rev-bond"
				  `((0 ,(polymer-verify-f polymer-move-f))
				    (1 ,polymer-verify-fr)))))))


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

  ;; convert an SXML S-expression to an XML string

  ;; For example...
  ;;   (sxml->string '(hello (@ (tone "perky") (volume 11)) "world"))
  ;; ...yields the string...
  ;;   "<hello tone=\"perky\" volume=\"11\">world</hello>"

  (define (sxml->string lst)
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