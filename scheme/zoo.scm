;; ZooGas XML generators

(use-syntax (ice-9 syncase))

;; (bindloc "north" '(0 -1) (type "empty" "foo") (type "wall" "bar") (default "whatever"))
(define bindloc
  (lambda args
  (let* ((loc (car args))
	 (xy (cadr args))
	 (cases (cddr args))
	 (named-cases (grep (lambda (type-rule)
			      (let* ((type (car type-rule)))
				(string? type)))
			    cases))
	 (default-cases (grep (lambda (type-rule)
				(let* ((type (car type-rule)))
				  (and (symbol? type) (string=? (symbol->string type) "default"))))
			      cases))
	 (default (if (null? default-cases) '() (cadr (car default-cases))))
	 (x (car xy))
	 (y (cadr xy)))
    `(bind
      (loc ,loc)
      (x ,x)
      (y ,y)
      ,@(if (null? named-cases) '()
	    `((bcase ,(map
		       (lambda (type-rule)
			 (let* ((type (car type-rule))
				(rule (cadr type-rule)))
			   `(bmatch (@ (type ,type)) ,rule)))
		       named-cases))))
      ,@(if (null? default) '()
	    `((default ,default)))))))

(define (default rule) `(default ,rule))
(define (type type value) (list type value))


;; Utility functions.
;; Simple fold
(define (fold-right binary-func init lst)
  (cond
   ((null? lst) init)
   ((pair? lst) (let ((fcar (binary-func init (car lst))))  ;; force this first
		  (fold-right binary-func fcar (cdr lst))))
   (else init)))

;; Simple map
(define (map unary-func lst)
  (fold-right (lambda (x y) (append x (list (unary-func y)))) '() lst))

;; Spliced map
(define (map-spliced unary-func lst)
  (fold-right (lambda (x y) (append x (unary-func y))) '() lst))

;; Simple grep
(define (grep predicate lst)
  (let* ((map-function (lambda (x) (if (predicate x) (list x) '()))))
    (map-spliced map-function lst)))

;; grep -v
(define (grep-not-equal x list)
  (grep (lambda (y) (not (equal? x y))) list))

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
	 rest))))
