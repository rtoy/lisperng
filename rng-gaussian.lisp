;;;;-------------------------------------------------------------------------
;;;;
;;;; Gaussian random variate
;;;;
;;;; f(x) = 1/sqrt(2*pi) e^{-1/2 * x^2}
;;;;
;;;;-------------------------------------------------------------------------

(in-package :com.github.lisperng)

;; A simple structure to hold the cached value for the Gaussian
;; generators, which usually generate two variates at a time.
(defstruct gaussian-generator-cache
  (cached-value 0d0 :type double-float)
  (cache-valid nil :type (member t nil) ))

(let ((cache (make-gaussian-generator-cache)))
  (defun gen-gaussian-variate-polar (&optional (*random-state* *random-state*))
    (cond ((gaussian-generator-cache-cache-valid cache)
	   (setf (gaussian-generator-cache-cache-valid cache) nil)
	   (gaussian-generator-cache-cached-value cache))
	  (t
	   (do* ((u1 (- (* 2 (random 1d0)) 1)
		     (- (* 2 (random 1d0)) 1))
		 (u2 (- (* 2 (random 1d0)) 1)
		     (- (* 2 (random 1d0)) 1))
		 (w (+ (* u1 u1) (* u2 u2))
		    (+ (* u1 u1) (* u2 u2))))
		((<= w 1)
		 (locally
		   (declare (type (non-negative-float double-float 1d0) w))
		   (let ((s (sqrt (/ (* -2 (log w)) w))))
		     (setf (gaussian-generator-cache-cached-value cache) (* u2 s))
		     (setf (gaussian-generator-cache-cache-valid cache) t)
		     (* u1 s)))))))))

;; GEN-GAUSSIAN-VARIATE-ALGORITHM-NA
;;
;; Ahrens Algorithm NA for normal variates.
;;
;; 2.  Generate U.  Save the first bit B of U.  (B = 0 if U < 1/2.
;; Otherwise B = 1).
;;
;; 3.  Generate E, a standard exponential deviate and set S = E + E.
;;
;; 4.  Generate C, a standard Cauchy deviate.  Use algorithm CA but
;; instead of "Generate U" in Step 1 of CA, reuse the remaining bits
;; of U from Step 2.
;;
;; 5.  Set X = sqrt(S/(1+C^C)) and save Y = C*X.
;;
;; 6.  If B = 0, return X.  Otherwise return -X.
;;
;; On the next call, return Y

#+(or)
(let ((save 0d0)
      (gen -1))
  (declare (double-float save)
	   (fixnum gen))
  (defun gen-gaussian-variate-algorithm-na
      (&optional (*random-state* *random-state*))
    (declare (optimize (speed 3) (safety 0)))
    (setf gen (- gen))
    (cond ((= gen 1)
	   (let* ((u (random 1d0))
		  (e (gen-exponential-variate-log-method 1d0))
		  (s (+ e e))
		  (b 0))
	     (declare (type (non-negative-float double-float) u))
	     (cond ((< u 1/2)
		    (setf u (+ u u)))
		   (t
		    (setf b 1)
		    (setf u (+ u u -1))))
	     (let* ((c (gen-cauchy-variate-algorithm-ca-aux u))
		    (x (sqrt (/ s (+ 1 (* c c)))))
		    (y (* c x)))
	       (setf save y)
	       (if (zerop b)
		   x
		   (- x)))))
	  (t
	   save))))

(let ((cache (make-gaussian-generator-cache)))
  (defun gen-gaussian-variate-algorithm-na
      (&optional (*random-state* *random-state*))
    (declare (optimize (speed 3) (safety 0)))
    (cond ((gaussian-generator-cache-cache-valid cache)
	   (setf (gaussian-generator-cache-cache-valid cache) nil)
	   (gaussian-generator-cache-cached-value cache))
	  (t
	   (let* ((u (random 1d0))
		  (e (gen-exponential-variate-log-method 1d0))
		  (s (+ e e))
		  (b 0))
	     (declare (type (non-negative-float double-float) u))
	     (cond ((< u 1/2)
		    (setf u (+ u u)))
		   (t
		    (setf b 1)
		    (setf u (+ u u -1))))
	     (let* ((c (gen-cauchy-variate-algorithm-ca-aux u))
		    (x (sqrt (/ s (+ 1 (* c c)))))
		    (y (* c x)))
	       (setf (gaussian-generator-cache-cached-value cache) y)
	       (setf (gaussian-generator-cache-cache-valid cache) t)
	       (if (zerop b)
		   x
		   (- x))))))))

(let ((cache (make-gaussian-generator-cache)))
  (defun gen-gaussian-variate-box-trig
      (&optional (*random-state* *random-state*))
    "Generate a pseudo-random number drawn from a Gaussian PDF with a mean
of zero and a variance of 1.  The PDF is

                    2
                   X
                 - --
                   2
               %E
          ----------------
          SQRT(2) SQRT(PI)


 STATE is the random state to use.
 The Box-Mueller method is used but the acceptance/rejection method is
 replaced by direct calculation via trigonometric functions..  See
 Knuth, Seminumerical Algorithms.
"
    (cond ((gaussian-generator-cache-cache-valid cache)
	   (setf (gaussian-generator-cache-cache-valid cache) nil)
	   (gaussian-generator-cache-cached-value cache))
	  (t
	   (let ((r1 (sqrt (* -2d0 (log (random 1d0)))))
		 (r2 (random #.(* (dfloat pi) 2d0))))
	     ;;(declare (double-float r1 r2))
	     (setf (gaussian-generator-cache-cached-value cache)
		   (* r1 (sin r2)))
	     (setf (gaussian-generator-cache-cache-valid cache) t)
	     (* r1 (cos r2)))))))

(let ((+sqrt-8/e+ #.(sqrt (/ 8.0d0 (exp 1d0))))
      (+4-exp-1/4+ #.(* 4.0d0 (exp 0.25d0)))
      (+4-exp-minus-1.35+ #.(* 4.0d0 (exp (- 1.35d0)))))
  (declare (double-float +sqrt-8/e+ +4-exp-1/4+ +4-exp-minus-1.35+))
  (defun gen-gaussian-variate-ratio (&optional (*random-state* *random-state*))
    "Generate a pseudo-random number drawn from a Gaussian PDF with a mean
of zero and a variance of 1.

                    2
                   X
                 - --
                   2
               %E
          ----------------
          SQRT(2) SQRT(PI)

 STATE is the random state to use.

 The ratio of uniform variates method is used.  See Knuth,
 Seminumerical Algorithms, Algorithm R.
"
    (do ((u (random 1d0) (random 1d0)))
	(nil)
      (declare (double-float u))
      (let* ((x (/ (* +sqrt-8/e+
		      (- (random 1d0) 0.5d0))
		   u))
	     (xs (* x x)))
	(declare (double-float x xs))
	(if (or (<= xs (- 5.0d0 (* u +4-exp-1/4+)))
		(and (<= xs (+ 1.4d0 (/ +4-exp-minus-1.35+ u)))
		     (<= xs (- (* 4.0d0 (log u))))))
	    (return-from gen-gaussian-variate-ratio x))))))

;; Marsaglia's Ziggurat method for Gaussians
(let ((r 3.442619855899d0))
  (flet ((density (x)
	   (declare (double-float x)
		    (optimize (speed 3) (safety 0)))
	   (exp (* -0.5d0 x x))))
    (declare (inline density))
    (multiple-value-bind (k-table w-table f-table)
	(ziggurat-init 127 r 9.91256303526217d-3 31
		       #'density
		       #'(lambda (x)
			   (sqrt (* -2 (log x)))))
      (defun gen-gaussian-variate-ziggurat
          (&optional (*random-state* *random-state*))
	(declare (optimize (speed 3)))
	(loop
	    ;; We really want a signed 32-bit random number. So make a
	    ;; 32-bit unsigned number, take the low 31 bits as the
	    ;; number, and use the most significant bit as the sign.
	    ;; Doing this in other ways can cause consing.
	    (let* ((ran (random (ash 1 32)))
		   (sign (ldb (byte 1 31) ran))
		   (j (if (plusp sign)
			  (- (ldb (byte 31 0) ran))
			  (ldb (byte 31 0) ran)))
		   (i (logand j 127))
		   (x (* j (aref w-table i))))
	      (when (< (abs j) (aref k-table i))
		(return x))
	      (when (zerop i)
		(loop
		    (let ((x (/ (- (log (random 1d0))) r))
			  (y (- (log (random 1d0)))))
		      (when (> (+ y y) (* x x))
			(return-from gen-gaussian-variate-ziggurat
			  (if (plusp j)
			      (- (+ r x))
			      (+ r x)))))))
	      (when (< (* (random 1d0) (- (aref f-table (1- i))
						(aref f-table i)))
		       (- (density x) (aref f-table i)))
		(return x))))))))


;;; Some timing results for CMUCL 18c+ on a 866 MHz Pentium III:
;;;
;;; (cllib::time-gaussian 1000000)
;;;
;;; Method 	real	user	sys	cons
;;;
;;; Polar	0.45	0.39	0.05	15994880
;;; NA		0.88	0.71	0.16	39997432
;;; Box/Trig	0.5	0.4	0.08	16003064
;;; Ratio	0.58	0.5	0.08	16003064
;;; Zigg	0.28	0.22	0.05	16437240
;;;
;;; Based on these results, Marsalia's Ziggurat method is far and away
;;; the fastest.
;;;
;;; Some timing results for CMUCL 2010-10 (roughly) on a Mac Mini with
;;; a 2.0 GHz Core 2 Duo, using sse2:
;;;
;;; Method 	real	user	sys	cons
;;;
;;; Polar	0.34	0.25	0.06	56000080
;;; NA		0.48	0.22	0.04	40000080
;;; Box/Trig	0.41	0.27	0.04	40000072
;;; Ratio	0.26	0.22	0.02	16000064
;;; Zigg	0.14	0.08	0.02	16000064
;;;
;;; Some timing results for CMUCL 18c+ (sparc-v9) on a 300 MHz Ultra
;;; 30:
;;;
;;; (cllib::time-gaussian 500000)
;;;
;;; Method 	real	user	sys	cons
;;;
;;; Polar	0.83	0.78	0.02	 8000480
;;; NA		1.03	0.83	0.12	20000480
;;; Box/Trig	1.39	1.15	0.06	 8000480
;;; Ratio	1.27	0.93	0.08	 8000480
;;; Zigg	0.42	0.33	0.06	 8220912
;;;
;;; Based on these results, Marsalia's Ziggurat method is far and away
;;; the fastest on this platform too.

;;; Select one that works for you.
(defmacro gen-gaussian-variate (&optional (state '*random-state*))
  `(gen-gaussian-variate-ziggurat ,state))
