;; ZooGas XML generators
(begin

  ;; general
  (define type-mask "ffff000000000000")
  (define type-shift 48)

  (define self-type "empty")

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

  (define (xy tag loc)
    `(,tag (x ,(car loc)) (y ,(cadr loc))))

  ;; gvars
  (define (gvars . args)
    (let ((type (car args))
	  (var-val-list (cdr args)))
      (gvars-list args var-val-list)))

  (define (gvars-list type var-val-list)
    `(gvars
      (type ,type)
      ,(map (lambda (var-val) `(val (@ (var ,(car var-val))) ,(cadr var-val))) var-val-list)))

  ;; helper for optional rule chains
  (define (opt-rule tag arg)
    (if arg `((tag ,(rule-eval-or-return arg))) '()))

  (define (rule-eval-or-return arg)
    `((rule ,(eval-or-return arg))))

  ;; set rule
  (define (set-rule . args)
    (let ((loc (list-ref args 0))
	  (type (list-ref args 1))
	  (var-val-list (opt-arg args 2 '()))
	  (next (opt-arg args 3 #f)))
      `(modify
	(srcmask 0)
	,(xy 'dest loc)
	(lshift 0)
	,(gvars-list type var-val-list)
	,@(opt-rule 'next next))))

  ;; switch rule for types
  (define (switch-type . args)
    (let ((loc (list-ref args 0))
	  (type-case-list (list-ref args 1))
	  (default (opt-arg args 2)))
      `(switch
	,(xy 'pos loc)
	(mask ,type-mask)
	(rshift ,type-shift)
	(map (lambda (type-case) `(case (gtype ,(car type-case)) ,(rule-eval-or-return (cdr type-case)))) type-case-list)
	,@(opt-rule 'default default))))

  ;; switch rule for vars
  (define (switch-var . args)
    (let ((loc (list-ref args 0))
	  (type (list-ref args 1))
	  (var (list-ref args 2))
	  (val-case-list (list-ref args 3))
	  (default (opt-arg args 4)))
      `(switch
	,(xy 'pos loc)
	(vmask (type ,type) (var ,var))
	(vrshift (type ,type) (var ,var))
	(map (lambda (val-case) `(case (state ,(car val-case)) ,(rule-eval-or-return (cdr val-case)))) val-case-list)
	,@(opt-rule 'default default))))

  ;; Neighborhood bindings
  (define (bind-moore dir-var func)
    (switch-var
     origin self-type dir-var
     (map (lambda (dir) (list dir (func (moore-loc dir) dir))) moore-dirs)))

  (define (bind-neumann dir-var func)
    (switch-var
     origin self-type dir-var
     (map (lambda (dir) (list dir (func (neumann-loc dir) dir))) neumann-dirs)))

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
    (if (= n 0) '() (append (iota (- n 1)) (list n))))

  (define neumann-dirs (iota 8))
  (define moore-dirs (iota 4))

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