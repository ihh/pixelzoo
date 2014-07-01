(begin

  ;; RNA
  ;; Vars:
  ;;  has-fwd-sense-bond (1 bit), fwd-sense-bond-dir (3 bits), has-rev-sense-bond (1 bit), rev-sense-bond-dir (3 bits),
  ;;  has-fwd-anti-bond (1 bit), fwd-anti-bond-dir (3 bits), has-rev-anti-bond (1 bit), rev-anti-bond-dir (3 bits)
  ;;  sense-base (2 bits), anti-base (2 bits), has-anti (1 bit)
  ;; Registers:
  ;;  (0,1)  location of fwd-sense-bond cell
  ;;  (2,3)  direction & inverse-direction from origin to fwd-sense-bond cell
  ;;  (4,5)  location of rev-sense-bond cell
  ;;  (6,7)  direction & inverse-direction from origin to rev-sense-bond cell
  ;;  (8,9)  location of fwd-anti-bond cell
  ;; (10,11) direction & inverse-direction from origin to fwd-anti-bond cell
  ;; (12,13) location of rev-anti-bond cell
  ;; (14,15) direction & inverse-direction from origin to rev-anti-bond cell
  ;; (16,17) location of target cell for move
  ;; (18,19) direction & inverse-direction from target cell to fwd-sense-bond cell
  ;; (20,21) direction & inverse-direction from target cell to rev-sense-bond cell
  ;; (22,23) direction & inverse-direction from target cell to fwd-anti-bond cell
  ;; (24,25) direction & inverse-direction from target cell to rev-anti-bond cell

  (define rna-has-fwd-sense-bond-var "has-fwd-sense-bond")
  (define rna-has-rev-sense-bond-var "has-rev-sense-bond")
  (define rna-fwd-sense-bond-dir-var "fwd-sense-bond-dir")
  (define rna-rev-sense-bond-dir-var "rev-sense-bond-dir")

  (define rna-has-fwd-anti-bond-var "has-fwd-anti-bond")
  (define rna-has-rev-anti-bond-var "has-rev-anti-bond")
  (define rna-fwd-anti-bond-dir-var "fwd-anti-bond-dir")
  (define rna-rev-anti-bond-dir-var "rev-anti-bond-dir")

  (define rna-sense-base-var "sense-base")
  (define rna-anti-base-var "anti-base")
  (define rna-has-anti-var "has-anti")

  (define (rna-particle name)
    `(particle
      (name ,name)
      (vars
       (varsize (name ,rna-has-fwd-sense-bond-var) (size 1))
       (varsize (name ,rna-fwd-sense-bond-dir-var) (size 3))
       (varsize (name ,rna-has-rev-sense-bond-var) (size 1))
       (varsize (name ,rna-rev-sense-bond-dir-var) (size 3))
       (varsize (name ,rna-has-fwd-anti-bond-var) (size 1))
       (varsize (name ,rna-fwd-anti-bond-dir-var) (size 3))
       (varsize (name ,rna-has-rev-anti-bond-var) (size 1))
       (varsize (name ,rna-rev-anti-bond-dir-var) (size 3))
       (varsize (name ,rna-sense-base-var) (size 2))
       (varsize (name ,rna-anti-base-var) (size 2))
       (varsize (name ,rna-has-anti-var) (size 1)))

      (colrule (var ,rna-sense-base-var) (hexmul "20000"))
      (colrule (var ,rna-anti-base-var) (hexmul "80000") (hexinc "14ffff"))
;      (rule (scheme "(rna-move-rule)"))
))

  ;; rna-move-rule:
  ;;  if (has-fwd-sense-bond)
  ;;   verify-fwd-sense
  ;;   if (has-rev-sense-bond)
  ;;    verify-rev-sense
  ;;    if (has-fwd-anti-bond)
  ;;      verify-fwd-anti
  ;;      if (has-rev-anti-bond)
  ;;       verify-rev-anti
  ;;       random-step-fs-rs-fa-ra
  ;;      else (!has-rev-anti-bond)
  ;;       random-step-fs-rs-fa
  ;;    else (!has-fwd-anti-bond)
  ;;      if (has-rev-anti-bond)
  ;;       verify-rev-anti
  ;;       random-step-fs-rs-ra
  ;;      else (!has-rev-anti-bond)
  ;;       random-step-fs-rs
  ;;  else (!has-rev-sense-bond)
  ;;    if (has-fwd-anti-bond)
  ;;      verify-fwd-anti
  ;;      if (has-rev-anti-bond)
  ;;       verify-rev-anti
  ;;       random-step-fs-fa-ra
  ;;      else (!has-rev-anti-bond)
  ;;       random-step-fs-fa
  ;;    else (!has-fwd-anti-bond)
  ;;      if (has-rev-anti-bond)
  ;;       verify-rev-anti
  ;;       random-step-fs-ra
  ;;      else (!has-rev-anti-bond)
  ;;       random-step-fs
  ;; else (!has-fwd-sense-bond)
  ;;   if (has-rev-sense-bond)
  ;;    verify-rev-sense
  ;;    if (has-fwd-anti-bond)
  ;;      verify-fwd-anti
  ;;      if (has-rev-anti-bond)
  ;;       verify-rev-anti
  ;;       random-step-rs-fa-ra
  ;;      else (!has-rev-anti-bond)
  ;;       random-step-rs-fa
  ;;    else (!has-fwd-anti-bond)
  ;;      if (has-rev-anti-bond)
  ;;       verify-rev-anti
  ;;       random-step-rs-ra
  ;;      else (!has-rev-anti-bond)
  ;;       random-step-rs
  ;;  else (!has-rev-sense-bond)
  ;;    if (has-fwd-anti-bond)
  ;;      verify-fwd-anti
  ;;      if (has-rev-anti-bond)
  ;;       verify-rev-anti
  ;;       random-step-fa-ra
  ;;      else (!has-rev-anti-bond)
  ;;       random-step-fa
  ;;    else (!has-fwd-anti-bond)
  ;;      if (has-rev-anti-bond)
  ;;       verify-rev-anti
  ;;       random-step-ra
  ;;      else (!has-rev-anti-bond)
  ;;       random-step


)
