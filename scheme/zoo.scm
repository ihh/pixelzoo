(begin
(begin
  ;; Utility functions.
  (define (caddddr lst) (cadddr (cdr lst)))
  (define (cadddddr lst) (caddddr (cdr lst)))

  ;; Numerics
  (define (ceiling-power-of-2 n)
    (if
     (> n 1)
     (* 2 (ceiling-power-of-2 (/ n 2)))
     1))

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
    (make-hood-loc loc)
    (xy 'neighbor loc))

  (define
    (make-hood map-func)
    `(hood
      ,@(map-func make-hood-loc)))

  (define moore-particle-neighborhood (make-hood map-moore))
  (define neumann-particle-neighborhood (make-hood map-neumann))

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

      (rule (scheme "(rna-move-rule)"))))

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
	   (x ,(/ len 2))
	   (y ,(/ len 2)))
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
)
