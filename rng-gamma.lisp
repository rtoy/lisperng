
;;;;-------------------------------------------------------------------------
;;;;
;;;; Gamma random variate, of order a
;;;;
;;;; f(x) = 1/Gamma(a) x^{a-1} e^{-x}
;;;;
;;;;-------------------------------------------------------------------------

;;;
;;; Here are some timing results for the gamma generators
;;;
;;; 10000 numbers generated.  Time is elapsed real time in seconds,
;;; Second number is bytes consed.  (For the Ultra-30, 50000 numbers
;;; were generated.  For the Core2D (Core 2 Duo, 2.0 GHz), 1,000,000
;;; numbers were generated.)
;;;
;;; Order = 1.1
;;; CPU	          Squeeze  GN         Algo. A           Algo GO
;;; 486-66        0.75     39.2       0.94
;;;               525k     11738k     391k
;;; Sparc20       0.19      9.0       0.25
;;;               335k     11493k     160k
;;; U-30 (300)    0.05      2.9       0.13
;;;               334k     12401k     160k
;;; Core2D-2.0    0.42     0.97       1.08
;;;               50.8M    66.7M      99.1M
;;;
;;; Order = 10
;;; CPU	          Squeeze  GN         Algo. A  Direct
;;; 486-66        .71      14.9       0.93
;;;               498k     5026k      420k
;;; Sparc20       0.17      3.6       0.22
;;;               321k     4890k      160k
;;; U-30 (300)    0.28      5.00      0.30     0.31       0.26
;;;               2411k    21700k     800k     1600k      1874k
;;; Core2D-2.0    0.42     0.94       5.38
;;;               48.2M    36.8M      73.7M
;;;
;;; Order = 100
;;; CPU	          Squeeze  GN         Algo. A
;;; 486-66        .70      13.1       0.93
;;;               496k     4254k      442k
;;; Sparc20       0.17      3.5       0.22
;;;               321k     4102k      160k
;;; U-30 (300)    0.28      0.28      0.31     2.54       0.22
;;;               2400k    1674k      800k     1600k      1663k
;;; Core2D-2.0    0.38     0.56       2.2                 0.36
;;;               48.0M    33.5M      72.8M               33.3M
;;;
;;; Order = 1000
;;; CPU	          Squeeze  GN         Algo. A
;;; 486-66        .70      12.1       0.96
;;;               496k     4017k      450k
;;; Sparc20       0.17      2.8       0.22
;;;               321k     3878k      160k
;;; U-30 (300)    0.23      0.33      0.32                0.22
;;;               1600k    1627k      800k                1626k
;;; Core2D-2.0    0.38     0.54       1.58                0.33
;;;               48.0M    33.5M      72.8M               32.6M
;;;
;;; Order = 10000
;;; CPU	          Squeeze  GN         Algo. A
;;; 486-66        0.67     10.9       0.95
;;;               496K     3950K      458k
;;; Sparc20       0.17      2.6       0.25
;;;               321k     3800k      160k
;;; U-30 (300)    0.24      0.26      0.33                0.25
;;;               1600k    1612k      800k                1619k
;;; Core2D-2.0    0.38     0.51       0.93                0.33
;;;               48.0M    32.2M      72.7M               32.4M
;;;

#+(or)
(defun gen-gamma-variate-squeeze
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above),
            ORDER >= 1.
  STATE   = random state to use.

  This uses Marsaglia's squeeze method.
"
  (declare (type (double-float 1d0) order))
  ;; Marsaglia's squeeze method for gamma variates.  This method is
  ;; valid for all order >= 1/3.  However, its efficiency gets better
  ;; with larger orders.  Thus, we want order to be at least 1.

  (let* ((s (/ #.(dfloat 1/3) (sqrt order)))
	 (z0 (- 1d0 (* s #.(sqrt 3d0))))
	 (cs (- 1d0 (* s s)))
	 (x0 (- s #.(sqrt 3d0)))
	 (cc (- (* order z0 z0 z0)
		(* 0.5d0 x0 x0)))
	 (cl (- 1d0 (* 3d0 order))))
    (do* ((x (gen-gaussian-variate)
	     (gen-gaussian-variate))
	  (z (+ (* s x) cs)
	     (+ (* s x) cs)))
	 (nil)
      (declare (double-float x z))
      (when (plusp z)
	(let* ((z z)
	       (rgama (* order z z z))
	       (e (gen-exponential-variate 1d0))
	       (cd (- (+ e (* 0.5d0 x x) cc)
		      rgama))
	       (tt (- 1d0 (/ z0 z))))
	  (declare (double-float e)
		   (type (non-negative-float double-float) z))
	  (when (or (plusp (- cd
			      (* cl tt
				 (+ 1d0 (* tt
					   (+ 0.5d0
					      (* #.(dfloat 1/3) tt)))))))
		    (>= (- cd (* cl (log (/ z z0))))
			0d0))
	    (return-from gen-gamma-variate-squeeze rgama)))))))

(defun gen-gamma-variate-squeeze
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above),
            ORDER >= 1.
  STATE   = random state to use.

  This uses Marsaglia's squeeze method.
"
  (declare (type (double-float 1d0) order))
  ;; Marsaglia's squeeze method for gamma variates.  This method is
  ;; valid for all order >= 1/3.  However, its efficiency gets better
  ;; with larger orders.  Thus, we want order to be at least 1.

  (let* ((s (/ #.(dfloat 1/3) (sqrt order)))
	 (z0 (- 1d0 (* s #.(sqrt 3d0))))
	 (cs (- 1d0 (* s s)))
	 (x0 (- s #.(sqrt 3d0)))
	 (cc (- (* order z0 z0 z0)
		(* 0.5d0 x0 x0)))
	 (cl (- 1d0 (* 3d0 order)))
	 (x 0d0)
	 (z 0d0))
    (declare (type double-float x z))
    (loop
	(tagbody
	  step-1
	   (setf x (gen-gaussian-variate))
	   (setf z (+ (* x s) cs))
	   (when (<= z 0)
	     (go step-1))
	   (let* ((z z)
		  (rgama (* order z z z))
		  (e (gen-exponential-variate 1d0))
		  (cd (- (+ e (* 0.5d0 x x) cc)
			 rgama))
		  (tt (- 1d0 (/ z0 z))))
	     (declare (double-float e)
		      (type (non-negative-float double-float) z))
	     (when (or (plusp (- cd
				 (* cl tt
				    (+ 1d0 (* tt
					      (+ 0.5d0
						 (* #.(dfloat 1/3) tt)))))))
		       (>= (- cd (* cl (log (/ z z0))))
			   0d0))
	       (return-from gen-gamma-variate-squeeze rgama)))))))


#+(or)
(defun gen-gamma-variate-gn (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1.
  STATE   = random state to use.

  This uses Ahrens and Dieter's Algorithm GN
"
  (declare (type (double-float 1d0) order)
	   (optimize (speed 3)))
  ;; Ahrens and Dieter Algorithm GN for gamma variates
  (let* ((mu (- order 1d0))
	 (sigma (sqrt (+ order (* (sqrt order) #.(sqrt (dfloat 8/3))))))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d)))
    ;;(declare (double-float mu sigma d b))
    (do ((u (random 1d0)
	    (random 1d0)))
	(nil)
      (declare (type (non-negative-float double-float (1d0)) u))
      (cond ((> u 0.009572265238289d0)
	     (let* ((s (gen-gaussian-variate))
		    (x (+ mu (* sigma s))))
	       (when (and (<= 0 x b)
			  (<= (log (random 1d0))
			      (- (+ (* mu
				       (+ 1d0
					  (log (/ (the (non-negative-float double-float) x)
						  mu))))
				    (* 0.5d0 s s))
				 x)))
		 (return-from gen-gamma-variate-gn x))))
	    (t
	     (let* ((s (gen-exponential-variate 1d0))
		    (x (* b (+ 1d0 (/ s d)))))
	       (when (<= (log (random 1d0))
			 (- (+ (* mu
				  (- (+ 2d0
					(log (/ x mu)))
				     (/ x b)))
			       3.7203284924588704d0
			       (log (the (non-negative-float double-float) (/ (* sigma d) b))))
			    b))
		 (return-from gen-gamma-variate-gn x))))))))

;; This unstructured version (which exactly follows the algorithm
;; description) is significantly faster than the structured version
;; above.  For example, for order = 100d0 and 50000 trials, the above
;; takes 4.4 sec, but this version takes 0.3 sec.  This version also
;; cons about 10 times less.  I don't know why that is.
(defun gen-gamma-variate-gn (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1.
  STATE   = random state to use.

  This uses Ahrens and Dieter's Algorithm GN
"
  (declare (type (double-float 1d0) order)
	   (optimize (speed 3)))
  ;; Ahrens and Dieter Algorithm GN for gamma variates
  (let* ((mu (- order 1d0))
	 (sigma (sqrt (+ order (* (sqrt order) #.(sqrt (dfloat 8/3))))))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d))
	 (x 0d0)
	 (s 0d0)
	 (u 0d0))
    (declare (type (non-negative-float double-float (1d0)) u)
	     (type double-float x s))
    (loop
	(tagbody
	  step-2
	   (setf u (random 1d0))
	   (when (<= u 0.009572265238289d0)
	     (go step-5))
	   (setf s (gen-gaussian-variate))
	   (setf x (+ mu (* sigma s)))
	   (when (or (< x 0) (> x b))
	     (go step-2))

	   (setf u (random 1d0))
	   (if (> (log u)
		  (- (+ (* mu
			   (+ 1d0
			      (log (the (non-negative-float double-float)
				     (/ x mu)))))
			(* 0.5d0 s s))
		     x))
	       (go step-2)
	       (return-from gen-gamma-variate-gn x))
	  step-5
	   (setf s (gen-exponential-variate 1d0))
	   (setf x (* b (+ 1 (/ s d))))
	   (setf u (random 1d0))
	   (if (> (log u)
		  (- (+ (* mu
			   (- (+ 2d0
				 (log (the (non-negative-float double-float) (/ x mu))))
			      (/ x b)))
			3.7203284924588704d0
			(log (the (non-negative-float double-float) (/ (* sigma d) b))))
		     b))
	       (go step-2)
	       (return-from gen-gamma-variate-gn x))))))

(defun gen-gamma-variate-algo-a
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1
  STATE   = random state to use.

  This uses Algorithm A, in Knuth, Seminumerical Algorithms.
"
  (declare (type (double-float 1d0) order)
	   (optimize (speed 3)))
  ;; The large order case. This is Algorithm A, sec 3.4.1 E.
  (let* ((a order)
	 (sqrt2a-1 (sqrt (- (* 2d0 a) 1d0)))
	 (a-1 (- a 1d0)))
    (declare (type (double-float (1d0)) a))
    (do* ((y (tan (* #.(dfloat pi) (random 1d0)))
	     (tan (* #.(dfloat pi) (random 1d0))))
	  (x (+ (* sqrt2a-1 y) a-1)
	     (+ (* sqrt2a-1 y) a-1)))
	 ((and (> x 0d0)
	       (<= (random 1d0)
		   (* (+ 1d0 (* y y))
		      (exp (- (* a-1 (log (/ (the (double-float (0d0)) x)
					     a-1)))
			      (* sqrt2a-1 y))))))
	  x))))

;; Knuth mentions that instead of computing tan(pi*u), we can use a
;; polar method.  This implements that idea.
;;
;; Some simple timing tests show that this isn't any faster than the
;; above on an Ultra-30 300 MHz.  I guess the tan function is very
;; fast (or the CMUCL's RNG is slow).
(defun gen-gamma-variate-algo-a-2
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1
  STATE   = random state to use.

  This uses Algorithm A, in Knuth, Seminumerical Algorithms.
"
  (declare (type (double-float 1d0) order)
	   (optimize (speed 3)))
  ;; The large order case. This is Algorithm A, sec 3.4.1 E.
  (let* ((a order)
	 (sqrt2a-1 (sqrt (- (* 2d0 a) 1d0)))
	 (a-1 (- a 1d0)))
    (declare (type (double-float (1d0)) a))
    (flet ((tan-pi-u ()
	     (do* ((u (- (random 2d0) 1d0)
		      (- (random 2d0) 1d0))
		   (v (- (random 2d0) 1d0)
		      (- (random 2d0) 1d0))
		   (s (+ (* u u) (* v v))
		      (+ (* u u) (* v v)))
		   )
		((< s 1d0)
		 (/ v u)))))
      (do* ((y (tan-pi-u) (tan-pi-u))
	    (x (+ (* sqrt2a-1 y) a-1)
	       (+ (* sqrt2a-1 y) a-1)))
	   ((and (> x 0d0)
		 (<= (random 1d0)
		     (* (+ 1d0 (* y y))
			(exp (- (* a-1 (log (/ (the (double-float (0d0)) x)
					       a-1)))
				(* sqrt2a-1 y))))))
	    x)))))

(defun gen-gamma-variate-small-order
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            0 < ORDER < 1.
  STATE   = random state to use.

 This uses the method given in problem 16, in Knuth, Seminumerical
 Algorithms.
"
  (declare (type (double-float (0d0) (1d0)) order))
  ;; order < 1.  This is the algorithm in problem 16 in Sec. 3.4.1, in
  ;; Knuth.
  (let ((p (/ #.(exp 1d0) (+ order #.(exp 1d0))))
	(recip-order (/ order))
	(a-1 (- order 1d0)))
    (do* ((u (random 1d0)
	     (random 1d0))
	  (v (random 1d0)
	     (random 1d0))
	  (x (if (< u p) (expt v recip-order) (- 1d0 (log v)))
	     (if (< u p) (expt v recip-order) (- 1d0 (log v))))
	  (q (if (< u p) (exp (- x)) (expt x a-1))
	     (if (< u p) (exp (- x)) (expt x a-1))))
	 ((< (random 1d0) q)
	  x)
      (declare (type (non-negative-float double-float (1d0))
		     u v)
	       (type (double-float (0d0)) x)))))

(defun gen-gamma-variate-direct
    (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER > 0
  STATE   = random state to use.

 This uses a direct method to generate the random variates.  It uses
 the fact that the sum of two gamma's of order A and B is also a gamma
 of order A + Note that gamma of order 1 is exponential and we know
 how to generate that easily.  This is only good for moderate orders.
 For non-integral orders, the small-order algorithm is called.
"
  (declare (type (double-float (0d0) (20d0)) order))
  ;; Direct generation of Gamma variates using the fact that the sum
  ;; of two gamma's of order A and B is also a gamma of order A +
  ;; B. Note that gamma of order 1 is exponential and we know how to
  ;; generate that easily.  This is only good for moderate orders.
  ;; Use one of the obove generators for higher orders.
  (multiple-value-bind (n r)
      (floor order)
    (declare (fixnum n)
	     #+(or)(type (double-float 0d0) r))
    (let ((x 1d0))
      (declare (type (double-float (0d0)) x))
      ;; Sum up the exponential variates here.  This is done my
      ;; multiplying the uniform variates and taking the log at
      ;; the end, instead of summing the log of uniform variates.
      (dotimes (k n)
	(declare (fixnum k))
	(setf x (* x (random 1d0))))
      ;; If we still have some left, add in a gamma variate of
      ;; the remaining order.
      (if (zerop r)
	  (- (log x))
	  (- (gen-gamma-variate-small-order r)
	     (log x))))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defconst +beta-algo-go+
    (double-float 0.009572265238289d0 0.009572265238289d0)
    0.009572265238289d0
    "gen-gamma-variate-algo-go threshold"))

;; Ahrens and Dieter's Algorithm GO.
#+(or)
(defun gen-gamma-variate-algo-go (a &optional (*random-state* *random-state*))
  (declare (type (double-float (2.5327805161251d0)) a)
	   (optimize (speed 3)))
  (let* ((mu (- a 1))
	 (v (sqrt a))
	 (sigma-2 (+ a #.(sqrt (/ 8d0 3d0))))
	 (sigma (sqrt sigma-2))
	 (w (/ sigma-2 mu))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d)))
    ;;(declare (type (double-float 0d0) w))

    (do ((u (random 1d0)
	    (random 1d0)))
	(nil)
      (cond ((<= u +beta-algo-go+)
	     ;; Step 8 and 9
	     (let ((x (* b (+ 1 (/ (gen-exponential-variate 1d0) d))))
		   (u (random 1d0)))
	       (when (<= (log u) (- (+ (* mu (+ 2 (log (/ x mu)) (- (/ x b))))
				       #.(- (log (/ (* (sqrt (* 2 pi)) +beta-algo-go+)
						  (- 1 +beta-algo-go+)))))
				    (+ b (log (/ (* sigma d) b)))))
		 (return-from gen-gamma-variate-algo-go x))))

	    (t
	     ;; Step 3
	     (let* ((s (gen-gaussian-variate))
		    (x (+ mu (* sigma s))))
	       (if (<= 0 x b)
		   (let ((x x)
			 (u (random 1d0))
			 (big-s (* 0.5d0 s s)))
		     (declare (type (non-negative-float double-float) x))
		     (if (< s 0)
			 (when (<= u (- 1 (* big-s (- (- 1 (* 2 (/ s v))) 1))))
			   (return-from gen-gamma-variate-algo-go x))
			 (when (<= u (- 1 (* big-s (- w 1))))
			   (return-from gen-gamma-variate-algo-go x)))
		     (when (<= (log u)
			       (+ (* mu (+ 1 (log (/ x mu))))
				  (- x)
				  big-s))
		       (return-from gen-gamma-variate-algo-go x))))))))))

;; This unstructured version (which exactly follows the algorithm
;; description) is significantly faster than the structured version
;; above.  For example, for order = 100d0 and 50000 trials, the above
;; takes 4.4 sec, but this version takes 0.3 sec.  This version also
;; cons about 10 times less.  I don't know why that is.
(defun gen-gamma-variate-algo-go (a &optional (*random-state* *random-state*))
  (declare (type (double-float (2.5327805161251d0)) a)
	   (optimize (speed 3)))
  (let* ((mu (- a 1))
	 (v (sqrt a))
	 (sigma-2 (+ a #.(sqrt (/ 8d0 3d0))))
	 (sigma (sqrt sigma-2))
	 (w (/ sigma-2 mu))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d))
	 (u 0d0)
	 (s 0d0)
	 (big-s 0d0)
	 (x 0d0))
    (declare (type (non-negative-float double-float (1d0)) u)
	     (type double-float s)
	     (type (non-negative-float double-float) big-s x))
    (loop
	(tagbody
	  step-2
	   (setf u (random 1d0))
	   (when (<= u +beta-algo-go+)
	     (go step-8))
	   (setf s (gen-gaussian-variate))
	   (setf x (+ mu (* sigma s)))
	   (when (or (< x 0) (> x b))
	     (go step-2))
	   (setf u (random 1d0))
	   (setf big-s (* 0.5d0 s s))
	   (when (>= s 0)
	     (go step-6))
	   (if (<= u (- 1 (* big-s (- (* w (- 1 (/ (+ s s) v))) 1))))
	       (return-from gen-gamma-variate-algo-go x)
	       (go step-7))
	  step-6
	   (when (<= u (- 1 (* s (- w 1))))
	     (return-from gen-gamma-variate-algo-go x))
	  step-7
	   (if (> (log u) (+ (* mu (+ 1 (log (/ x mu))))
			     (- x)
			     big-s))
	       (go step-2)
	       (return-from gen-gamma-variate-algo-go x))
	  step-8
	   (setf s (gen-exponential-variate 1d0))
	   (setf x (* b (+ 1 (/ s d))))
	   (setf u (random 1d0))
	   (if (> (log u) (- (+ (* mu (+ 2 (log (/ x mu)) (- (/ x b))))
				#.(- (log (/ (* (sqrt (* 2 (dfloat pi)))
                                                +beta-algo-go+)
					     (- 1 +beta-algo-go+)))))
			     (+ b (log (/ (* sigma d) b)))))
	       (go step-2)
	       (return-from gen-gamma-variate-algo-go x))))))

;;; This is probably quite broken.  It's my own attempt at using the
;;; ratio method to generate Gamma variates.  The usual method gets
;;; progressively worse as the parameter gets larger.  This tries to
;;; take advantage of the fact that the desired region actually fits
;;; in a thin rotated rectangle.  Needs more work.
#+(or)
(defun gen-gamma-variate-ratio (a &optional (*random-state* *random-state*))
  (declare (type (double-float 1.5d0) a)
	   (optimize (speed 3)))
  (flet ((g (s)
	   (declare (type (double-float (0d0)) s))
	   (exp (- (* (- a 1) (log s))
		   s)))
	 (vr-limits (s0)
	   (let* ((a+1 (+ a 1))
		  (z (+ a+1 s0))
		  (z/2 (/ z 2))
		  (y (/ (sqrt (+ (expt (- a+1 s0) 2)
				 (* 8 s0)))
			2)))
	     (values (+ z/2 y) (- z/2 y)))))
    (let* ((alpha1 (* (- a 1) (+ (* a a a) (* 3 a a) (* 5 a) -11)))
	   (alpha2 (+ (* a a a) (* 3 a a) (* 12 a) -17))
	   (alpha (expt (+ (/ (sqrt alpha1) #.(* 3 (sqrt 3d0)))
			   (/ alpha2 27))
			#.(dfloat 1/3)))
	   (s0 (+ (/ (+ a 1) 3)
		  alpha
		  (/ (+ (* a a) a a -2)
		     9 alpha)))
	   (ur-hi (sqrt (* (g s0) (+ 1 (* s0 s0))))))
      (multiple-value-bind (s1 s2)
	  (vr-limits s0)
	(format t "s1, s2 = ~A ~A~%" s1 s2)
	(let ((vr-lo (* (sqrt (g s1))
			(+ 1 (* s1 s0))))
	      (vr-hi (* (sqrt (g s2))
			(+ 1 (* s2 s0)))))
	  (do* ((ur (random ur-hi)
		    (random ur-hi))
		(vr (- (random (- vr-hi vr-lo)) vr-lo)
		    (- (random (- vr-hi vr-lo)) vr-lo))
		(u (/ (- ur (* vr s0)) (+ 1 (* s0 s0)))
		   (/ (- ur (* vr s0)) (+ 1 (* s0 s0))))
		(v/u (/ (+ (* ur s0) vr)
			(- ur (* vr s0)))
		     (/ (+ (* ur s0) vr)
			(- ur (* vr s0)))))
	       ((<= (* u u) (g v/u))
		v/u)))))))

;;;
;;; The following Gamma variate routines appear to work (according to
;;; the sample histogram).
;;;
;;;    gen-gamma-variate-algo-a
;;;    gen-gamma-variate-algo-a-2
;;;    gen-gamma-variate-squeeze
;;;
;;; These do not appear to work:
;;;    gen-gamma-variate-algo-go  (some problem for small variates)
;;;    gen-gamma-variate-gn       (doesn't quite match expected values for large values)


(defun gen-gamma-variate (order &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from a Gamma distribution of order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER > 0
  STATE   = random state to use.

 This is the main routine for generating Gamma variates.
"
  (declare (type (double-float (0d0)) order))
  ;; We divide the set of possible orders into these ranges:
  ;; order > s, 1 < order <= s, order = 1, 0 < order < 1.
  ;; Select the appropriate value of s to minimize runtime.
  (cond ((> order 1d0)
	 ;; Pick the fastest of the three algorithms above.
	 (gen-gamma-variate-squeeze order))
	;; If the threshold s is 1, comment out this code.
	#+(or)
	((> order 1d0)
	 (gen-gamma-variate-direct order))
	((= order 1d0)
	 ;; Gamma variate of order 1 is an exponential variate
	 (gen-exponential-variate 1d0))
	(t
	 ;; order < 1.  This is the algorithm in problem 16 in Sec. 3.4.1
	 (gen-gamma-variate-small-order order))))

