;; misc.scm -- miscellaneous list utilities
;; Copyright (c) 2009-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (map-onto proc ls init)
  (let lp ((ls (reverse ls)) (res init))
    (if (null? ls) res (lp (cdr ls) (cons (proc (car ls)) res)))))

(define (append! . lists) (concatenate! lists))

(define (concatenate lists)
  (let lp ((ls (reverse lists)) (res '()))
    (if (null? ls) res (lp (cdr ls) (append (car ls) res)))))

(define (concatenate! lists)
  (if (null? lists)
      '()
      (let lp ((ls lists))
        (cond ((not (pair? (cdr ls)))
               lists)
              (else
               (set-cdr! (last-pair (car ls)) (cadr ls))
               (lp (cdr ls)))))))

(define (append-reverse rev tail)
  (if (null? rev) tail (append-reverse (cdr rev) (cons (car rev) tail))))

(define (append-reverse! rev tail)
  (if (null? rev)
      tail
      (let ((head (reverse! rev)))
        (set-cdr! rev tail)
        head)))

(define (zip . lists) (apply map list lists))

(define (unzip1 ls) (map car ls))
(define (unzip2 ls) (values (map car ls) (map cadr ls)))
(define (unzip3 ls) (values (map car ls) (map cadr ls) (map caddr ls)))
(define (unzip4 ls)
  (values (map car ls) (map cadr ls) (map caddr ls) (map cadddr ls)))
(define (unzip5 ls)
  (values (map car ls) (map cadr ls) (map caddr ls)
          (map cadddr ls) (map (lambda (x) (car (cddddr x))) ls)))

(define (count pred ls . lists)
  (if (null? lists)
      (let lp ((ls ls) (res 0))
        (if (pair? ls) (lp (cdr ls) (if (pred (car ls)) (+ res 1) res)) res))
      (let lp ((lists (cons ls lists)) (res 0))
        (if (every pair? lists)
            (lp (map cdr lists) (if (apply pred (map car lists)) (+ res 1) res))
            res))))

