;;; Binomial random variate

(in-package :com.github.lisperng)

#+(or)
(eval-when (:compile-toplevel :execute)
(declaim (ftype (function ((and (integer 0) fixnum)
			   (non-negative-float double-float 1d0)
			   &optional random-state)
			  (and (integer 0) fixnum))
		gen-binomial-variate))
)

(defun gen-binomial-variate (ntrials p
                             &optional (*random-state* *random-state*))
  "Generate a pseudo-random number from a beta distribution function
with parameters N and p:

                                   N - K  K
      P(K) = BINOMIAL(N, K) (1 - P)      P

 where BINOMIAL(N, K) is

              N!
          -----------
          M! (N - M)!

 NTRIALS      = N, above, the number of trials
 P            = probability of success
 STATE        = random state to use

 The output is an integer.
"
  (declare (type (and (integer 0) fixnum) ntrials)
	   (type (non-negative-float double-float 1d0) p))
  ;; Select some suitable threshold between the direct generation and
  ;; the iterative technique. For a 486-66, the break-even point is
  ;; near 100.  Same is true for a Sparc-20
  (cond ((< ntrials 100)
	 ;; Direct generation
	 (let ((n 0))
	   (declare (fixnum n))
	   (dotimes (k ntrials n)
	     (declare (fixnum k))
	     (if (< (random 1d0) p)
		 (incf n)))))
	(t
	 (let* ((a (1+ (floor ntrials 2)))
		(b (1+ (- ntrials a)))
		(x (gen-beta-variate (dfloat a) (dfloat b))))
	   (declare (fixnum a b)
		    (double-float x))
	   (if (>= x p)
	       (gen-binomial-variate (1- a) (/ p x))
	       (+ a (gen-binomial-variate (1- b) (/ (- p x) (- 1d0 x)))))))))
