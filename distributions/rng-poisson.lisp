
;;; Poisson random variate

(in-package :com.github.lisperng)

#+(or)
(eval-when (:compile-toplevel)
  (declaim (ftype (function ((double-float 0d0) &optional random-state)
			    (and (integer 0) fixnum))
		  gen-poisson-variate)))

(defun gen-poisson-variate (mean &optional (*random-state* *random-state*))
  "Generate a pseudo-random number from a Poisson distribution
with mean M:

               K   - M
              M  %E
       P(K) = --------
                 K!

 MEAN       = Mean (M) of the distribution, M >= 0
 STATE      = random state to use.

 The output is an integer.
"
  (declare (type (double-float 0d0) mean))
  (let ((threshold 30d0))
    (cond ((< mean threshold)
	   ;; Direct generation
	   (let ((limit (exp (- mean))))
	     (do ((prod (random 1d0))
		  (n 1 (+ n 1)))
		 ((<= prod limit)
		  (- n 1))
	       (declare (fixnum n)
			(type (double-float 0d0) prod))
	       (setf prod (* prod (random 1d0))))))
	  (t
	   ;; Indirect generation
	   (let* ((alpha #.(coerce 7/8 'double-float)) ; Suggested value
		  (order (floor (* alpha mean)))
		  (x (gen-gamma-variate (dfloat order))))
	     (declare (fixnum order))
	     (if (< x mean)
		 (+ order (gen-poisson-variate (- mean x)))
		 (gen-binomial-variate (1- order)
				       (/ mean x))))))))
