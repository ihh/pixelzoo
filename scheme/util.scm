(begin
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
