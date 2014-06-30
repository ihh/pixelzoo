(begin

  ;; RNA
  ;; Vars:
  ;;  has-fwd-sense-bond (1 bit), fwd-sense-bond-dir (3 bits), has-rev-sense-bond (1 bit), rev-sense-bond-dir (3 bits),
  ;;  has-fwd-anti-bond (1 bit), fwd-anti-bond-dir (3 bits), has-rev-anti-bond (1 bit), rev-anti-bond-dir (3 bits)
  ;;  sense-base (2 bits), anti-base (2 bits), has-anti (1 bit)
  ;; Registers:
  ;;  (0,1)  location of fwd-bond cell
  ;;  (2,3)  direction & inverse-direction from origin to fwd-bond cell
  ;;  (4,5)  location of rev-bond cell
  ;;  (6,7)  direction & inverse-direction from origin to rev-bond cell
  ;;  (8,9)  location of target cell for move
  ;; (10,11) direction & inverse-direction from target cell to fwd-bond cell
  ;; (12,13) direction & inverse-direction from target cell to rev-bond cell

)