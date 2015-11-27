;;;;-------------------------------------------------------------------------
;;;; Cauchy random variate.
;;;;
;;;; f(x) = 1/pi/(1 + x^2)
;;;; F(x) = 1/2 - 1/pi*atan(x)
;;;;
;;;;-------------------------------------------------------------------------

(in-package :com.github.lisperng)

;; GEN-CAUCHY-VARIATE-TAN
;;
;; Use the inverse of the CDF to generate the desired Cauchy variate.

(defun gen-cauchy-variate-tan (&optional (*random-state* *random-state*))
  (tan (* #.(dfloat pi)
	  (- (random 1d0) 0.5d0))))

(declaim (inline gen-cauchy-variate-algorithm-ca-aux))

;; GEN-CAUCHY-VARIATE-ALGORITHM-CA
;;
;; Ahrens (1988), Algorithm CA for generating Cauchy variates.
;;
;; 0.  Constants
;;      a 0.6380631366077803d0
;;      b 0.5959486060529070d0
;;      q 0.9339962957603656d0
;;      W 0.2488702280083841d0
;;      A 0.6366197723675813d0
;;      B 0.5972997593539963d0
;;      H 0.0214949004570452d0
;;      P 4.9125013953033204d0
;;
;; 1.  Generate U.  Set T = U - 1/2 and S = W - T^2.  If S <= 0, go to
;; 3.
;;
;; 2.  Return X = T*(A/S+B).
;;
;; 3.  Generate U.  Set T = U - 1/2 and s = 1/4 - T^2 and X = T*(a/s +
;; b).
;;
;; 4.  Generate U'.  If s^2*((1 + X^2)*(H*U' + P) - q) + s > 1/2, go
;; to 3.
;;
;; 5.  Return X.
;;

(let ((a 0.6380631366077803d0)
      (b 0.5959486060529070d0)
      (q 0.9339962957603656d0)
      (ww 0.2488702280083841d0)
      (aa 0.6366197723675813d0)
      (bb 0.5972997593539963d0)
      (hh 0.0214949004570452d0)
      (pp 4.9125013953033204d0))
  (declare (double-float a b q ww aa bb hh pp))
  (defun gen-cauchy-variate-algorithm-ca-aux
      (u &optional (*random-state* *random-state*))
    (declare (type (non-negative-float double-float (1d0)) u)
	     (optimize (speed 3) (safety 0) (space 0)))
    (let* ((tt (- u 0.5d0))
	   (s (- ww (* tt tt))))
      (when (> s 0)
	(return-from gen-cauchy-variate-algorithm-ca-aux (* tt (+ (/ aa s) bb))))

      (do ((u (random 1d0) (random 1d0))
	   (u1 (random 1d0) (random 1d0)))
	  (nil)
	(let* ((tt (- u 0.5d0))
	       (s (- 1/4 (* tt tt)))
	       (x (* tt (+ (/ a s) b))))
	  (when (<= (+ s (* s s
			    (- (* (+ 1 (* x x))
				  (+ (* hh u1) pp))
			       q)))
		    1/2)
	    (return-from gen-cauchy-variate-algorithm-ca-aux x))))))
  (defun gen-cauchy-variate-algorithm-ca
      (&optional (*random-state* *random-state*))
    (gen-cauchy-variate-algorithm-ca-aux (random 1d0)))
  )

;;; Select the one that works best for you.

(defmacro gen-cauchy-variate (&optional (state '*random-state*))
  `(gen-cauchy-variate-tan ,state))

