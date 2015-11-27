;;;;-------------------------------------------------------------------------
;;;;
;;;; Standard exponential random variate with mean m.
;;;;
;;;; f(x) = 1/m*e^{-x/m}, x >= 0
;;;; F(x) = 1 - e^{-x/m}, x >= 0
;;;;
;;;;-------------------------------------------------------------------------

(in-package :com.github.lisperng)

;;; NOTE: Currently, some of methods for generating exponential
;;; variates seem to generate exponential variates that AREN'T
;;; actually exponential.  (Based on just looking at the histograms of
;;; some variates.)
;;;
;;; Here is a list of the ones that seem to work:
;;;
;;;    gen-exponential-variate-log-method
;;;    gen-exponential-variate-algo-s
;;;    gen-exponential-variate-ea
;;;    gen-exponential-variate-ea-2
;;;    gen-exponential-variate-ratio
;;;    gen-exponential-variate-ziggurat
;;;
;;; These don't seem to work:
;;;    gen-exponential-variate-algorithm-ma

;; GEN-EXPONENTIAL-VARIATE-LOG-METHOD
;;
;; Since the CDF is y = F(x) = 1 - e^(x/m), the inverse function is
;; -m*ln(1-y).  If y is a uniform random variate on [0,1), then
;; -m*ln(1-y) is exponential.  Note that if y is uniform, then 1-y is
;; also uniform, so we just use -m*ln(y).
;;
;; See Knuth, The Art of Computer Programming, Volume 2.

(defun gen-exponential-variate-log-method
    (mu &optional (*random-state* *random-state*))
  "Generate a pseudo-random number drawn from an exponential PDF with a
mean of mu:
                - X/MU
              %E
              --------
                 MU

 STATE is the random state to use.  The logarithmic method is used.
"
  (declare (type (double-float (0d0)) mu))
  (* mu (- (log (random 1d0)))))



;; This table is Knuth's Q[k] in Algorithm S.
;;
;; Q[k] = sum (ln 2)^k/k! for k = 1, 2,...,
;;
;; until Q[k] > 1 - 2^(-t) where the uniform deviates have t+1 bit
;; accuracy.
;;
;;
;; Here's how we generated this table.  I used CLISP to get the extra
;; digits in the table.
;;
;;(do* ((nbits (float-digits 1d0))
;;      (k 1 (1+ k))
;;      (term #.(log 2L0) (* term (/ #.(log 2L0) k)))
;;      (sum term (+ sum term)))
;;     ((or (> sum (- 1 (scale-float 1l0 (- nbits))))
;;          (> k 20)))
;;  (format t "~3D ~50,45F ~25G ~%" k sum term))
;;
;; Note that Q[k] -> 1 as k -> infinity.  Thus, we might get more
;; accurate results for large k if we compute
;;
;;            infty
;; Q[k] = 1 -  sum  (ln 2)^m/m!
;;            m=k+1
;;

(declaim (ftype (function ((double-float (0d0))
			   &optional random-state)
			  (non-negative-float double-float))
		gen-std-exponential-variate-algo-s))

(let ((std-exponential-variate-table
       (make-array 16
		   :element-type 'double-float
		   :initial-contents
		   '(
		     0d0
		     0.693147180559945309417232121458176568075d0
		     0.933373687519046021750783384621509053940d0
		     0.988877796183867601703925648390130811298d0
		     0.998495925291496078865904719963789676778d0
		     0.999829281106138923208245942162589294252d0
		     0.999983316410072739307790313135916717732d0
		     0.999998569143876799148070338574928727372d0
		     0.999999890692555813579019178950751556207d0
		     0.999999992473415905976016453850827533653d0
		     0.999999999528327526777139783726219715203d0
		     0.999999999972881353964220933485860571090d0
		     0.999999999998559789957709138627855373484d0
		     0.999999999999928938843099551515944568884d0
		     0.999999999999996726106647776972279059926d0
		     0.999999999999999858543354865400900694870d0
		     ))))
  (declare (type (simple-array double-float (16)) std-exponential-variate-table))

  ;; GEN-EXPONENTIAL-VARIATE-ALGO-S
  ;;
  ;; Knuth's Algorithm S for generating exponential random variates.
  ;;
  ;; 1.  Generate a (t+1)-bit uniform random binary fraction. U =
  ;; (0.b0 b1 b2...).  Locate the first zero bit b(j) and shift off
  ;; the leading j+1 bits, setting U = (0.b(j+1) b(j+2)...).
  ;;
  ;; 2.  If U < ln(2), set X = m*(j*ln(2) + U) and terminate
  ;;
  ;; 3.  Find the least k >= 2 such that U < Q[k].  Generate k new
  ;; uniform deviates U1, U2, ... U[k] and set V = min(U1, U2,...,
  ;; U[k]).
  ;;
  ;; 4.  Set X = m*(j + V)*ln(2)

  (defun gen-exponential-variate-algo-s
      (mu &optional (*random-state* *random-state*))
    "Generate a pseudo-random number drawn from an exponential PDF with a
mean of 1:

                - X
              %E

 for X >= 0.

 STATE is the random state to use.  Knuth's Algorithm S is used to
 generate the variate.
"
    (declare (type (double-float (0d0)) mu))

    (multiple-value-bind (u j)
	;; Step S1.  Find the first zero bit by comparing against 1/2.
	;; If more than 1/2, the leading bit is 1.  In this case,
	;; multiply by 2, and take the fractional part.  This drops the
	;; leading 1 bit.
	(let ((j 0)
	      (u (random 1d0)))
	  (declare (type (integer 0 100) j)
		   (type (double-float 0d0 1d0) u)
		   (optimize (speed 3) (safety 0)))
	  (loop while (> u 0.5d0) do
		(incf j)
		(setf u (- (+ u u) 1)))
	  (values (+ u u) j))

      (declare (type (integer 0 100) j))
      (let ((ln2 (aref std-exponential-variate-table 1)))
	(cond ((< u ln2)
	       (values (* mu (+ u (* j ln2)))))
	      (t
	       (do ((k 2 (+ k 1))
		    (v (min (random 1d0) (random 1d0))
		       (min v (random 1d0))))
		   ((<= u (aref std-exponential-variate-table k))
		    (values (* mu ln2 (+ j v))))
		 (declare (type (double-float 0d0 (1d0)) v)
			  (optimize (speed 3) (safety 0)))
		 ))))))

  ;; Ahrens Algorithm SA
  ;;
  ;; See Ahrens, CACM, vol 15, no. 10, 1972.
  ;;
  ;; 1.  Initialize a = 0 and generate a uniform variate u.  Let b be
  ;; the first bit of u after the binary point.
  ;;
  ;; 2.  If b = 1 (u >= 0.5) goto 4.
  ;;
  ;; 3.  If b = 0 (u < 0.5) set a = a + ln(2) and shift u to the left
  ;; by one bit (u = u + u).  This produces a new first bit b.  Go to
  ;; 2.
  ;;
  ;; 4.  Shift u to the left by one bit (u = u + u - 1).  If u >
  ;; ln(2), go to 6.
  ;;
  ;; 5.  If u <= ln(2), deliver x = a + u.
  ;;
  ;; 6.  Initialize i = 2 and generate u*.  umin = u*.
  ;;
  ;; 7.  Generate a new u*.  umin = min(umin, u*).
  ;;
  ;; 8.  If u > q[i], i = i + 1 and go to 7.
  ;;
  ;; 9.  If u <= q[k], deliver a + umin*ln(2).
  ;;
  ;; Notes:
  ;;
  ;; 1.  In the original paper, step 9 has a small typo where an
  ;; extraneous "x" is added.
  ;;
  ;; 2.  This appears to be exactly the same as Knuth's algorithm S,
  ;; but written in a floating point fashion.  Step S1 is the same as
  ;; steps 1-4.  Step S2 is step 5.  Step S3-S4 are steps 6-9.  This
  ;; also shows a typo.  In Knuth: Step S4 reads m*(j + V)*ln(2).
  ;; Step 9 here should read "deliever (a + umin)*ln(2)".

  (defun gen-exponential-variate-sa
      (mu &optional (*random-state* *random-state*))
    (declare (ignore mu))
    (let ((a 0d0)
	  (u (random 1d0)))
      (declare (double-float a u))
      (loop
	  (tagbody
	   step-2
	     (when (>= u 0.5d0)
	       (go step-4))
	   step-3
	     (incf a (aref std-exponential-variate-table 1))
	     (setf u (+ u u))
	     (go step-2)
	   step-4
	     (setf u (+ u u -1))
	     (when (<= u (aref std-exponential-variate-table 1))
	       (return-from gen-exponential-variate-sa (+ a u)))
	   step-6
	     (let* ((i 2)
		    (u* (random 1d0))
		    (umin u*))
	       (declare (fixnum i)
			(type (double-float 0d0 1d0) u* umin))
	       (loop
		   (tagbody
		    step-7
		      (setf umin (min umin (random 1d0)))
		      (if (> u (aref std-exponential-variate-table i))
			  (progn
			    (incf i)
			    (go step-7))
			  (return-from gen-exponential-variate-sa (* (+ a umin (aref std-exponential-variate-table 1))))))))))))
  )

;; GEN-EXPONENTIAL-VARIATE-ALGORITHM-MA
;;
;; This is Algorithm MA given in Ahrens, but proposed by Marsaglia.
;;
;; See Ahrens, CACM, vol. 15, no. 10, 1972.
;;
;; p[k] = 1 - exp(-k)
;; q[k] = (1/1! + 1/2! + ... + 1/k!)/(e - 1)
;;
;; 1.  Initialize i = 0 and generate uniform variate u.
;;
;; 2.  If u <= p[i+1], go to 4.
;;
;; 3.  If u > p[i+1], i = i + 1, and go to 2.
;;
;; 4.  Initialize k = 1 and generate u and u*.  Set umin = u*.
;;
;; 5.  If u <= q[k], go to 8.
;;
;; 6.  If u > q[k], k = k + 1.
;;
;; 7.  Generate a new u*, and set umin=min(umin, u*).  Go to 5.
;;
;; 8.  Deliver x = i + umin.
;;
;; Here's how to compute the constants p[k] and q[k].  kernel:%expm1
;; is the function exp(x)-1, but done carefully to preserve accuracy.
;;
;; (do* ((k 0 (+ 1 k))
;; 	       (pk 0d0 (- (float (kernel:%expm1 (float (- k) 1L0)) 1d0)))
;; 	       (init (list pk) (cons pk init)))
;; 	     ((>= pk 1d0)
;; 	      (make-array (length init)
;; 			  :element-type 'double-float
;; 			  :initial-contents (nreverse init))))
;; (do* ((k 1 (+ 1 k))
;; 	       (term 1 (/ term k))
;; 	       (sum 1 (+ sum term))
;; 	       (init (list 0) (cons sum init)))
;; 	      ((>= (/ sum #.(- (exp 1d0) 1))
;; 		   1)
;; 	       (make-array (length init)
;; 			   :element-type 'double-float
;; 			   :initial-contents (mapcar #'(lambda (x)
;; 							 (/ x #.(- (exp 1d0) 1)))
;; 						     (nreverse init)))))

(let ((p
       (make-array 39
		   :element-type 'double-float
		   :initial-contents
		   '(0.0d0 0.6321205588285577d0 0.8646647167633873d0 0.950212931632136d0
		     0.9816843611112658d0 0.9932620530009145d0 0.9975212478233336d0
		     0.9990881180344455d0 0.9996645373720975d0 0.9998765901959134d0
		     0.9999546000702375d0 0.9999832982992097d0 0.9999938557876467d0
		     0.999997739670593d0 0.9999991684712809d0 0.9999996940976795d0
		     0.9999998874648253d0 0.9999999586006229d0 0.9999999847700203d0
		     0.9999999943972036d0 0.9999999979388464d0 0.999999999241744d0
		     0.9999999997210532d0 0.9999999998973812d0 0.9999999999622486d0
		     0.9999999999861121d0 0.9999999999948909d0 0.9999999999981205d0
		     0.9999999999993086d0 0.9999999999997456d0 0.9999999999999064d0
		     0.9999999999999656d0 0.9999999999999873d0 0.9999999999999953d0
		     0.9999999999999983d0 0.9999999999999993d0 0.9999999999999998d0
		     0.9999999999999999d0 1d0)))
      (q
       (make-array 17
		   :element-type 'double-float
		   :initial-contents
		   '(0.0d0 0.8729650603039897d0 0.9699611781155442d0 0.9942102075684327d0
		     0.9990600134590104d0 0.9998683144407733d0 0.9999837860095966d0
		     0.9999982199556996d0 0.9999998237274889d0 0.9999999841046677d0
		     0.9999999986844113d0 0.9999999998993898d0 0.9999999999928497d0
		     0.9999999999995255d0 0.9999999999999706d0 0.9999999999999983d0 1d0))))
  (declare (type (simple-array double-float (39)) p)
	   (type (simple-array double-float (17)) q))
  (defun gen-exponential-variate-algorithm-ma
      (mu &optional (*random-state* *random-state*))
    (declare (ignore mu))
    ;; Find i such that u <= p[i+1].
    (let* ((u (random 1d0))
	   (i (do ((k 1 (+ 1 k)))
		  ((<= u (aref p k))
		   (- k 1)))))
      ;; Find k such that min(u1, u2,...,uk) <= q[k].  Then return i +
      ;; min(u1,...,uk)
      (do* ((new-u (random 1d0))
	    (k 1 (+ 1 k))
	    (umin (random 1d0) (min umin (random 1d0))))
	  ((<= new-u (aref q k))
	   (+ i umin))
	(declare (type (double-float 0d0 1d0) umin))))))

;; GEN-EXPONENTIAL-VARIATE-EA
;;
;; Ahrens Algorithm EA for generating exponential random variates. The
;; description given by Ahrens contains several typos which were
;; corrected by Kenneth G. Hamilton.  We describe the algorithm here,
;; with corrections made by Hamilton.
;;
;; See Ahrens, CACM, vol. 31, no. 11, 1988.
;; See Hamilton, ACM Trans on Math. Software, vol. 24, no. 1, 1998.
;;
;; 0.  Constants:
;;       a (* (log 2d0) (+ 4 (* 3 (sqrt 2d0))))
;;       b (+ 2 (sqrt 2d0)))
;;       c (- (* (log 2d0) (+ 1 (sqrt 2d0))))
;;       p (* (sqrt 2d0) (log 2d0))
;;       A (* a p))
;;       B (* b p))
;;       H 0.0026106723602095d0
;;       D (/ (* b b))))
;; 1.  Generate uniform variate U and set G = c.
;;
;; 2.  Set U = U + U.  If U >= 1 go to 4.
;;
;; 3.  Set G = G + ln(2) and go to 2.
;;
;; 4.  Set U = U - 1.  If U > p go to 6.
;;
;; 5.  Return X = G + A/(B - U).
;;
;; 6.  Generate U and set Y = a / (b - U).
;;
;; 7.  Generate U'.  If (U'*H + D)*(b - U)^2 > exp(-(Y + c), go to 6.
;;
;; 8.  Return X = G + Y.
;;
(let* ((ln-2 #.(log 2d0))
       (a #.(* (log 2d0) (+ 4 (* 3 (sqrt 2d0)))))
       (b #.(+ 2 (sqrt 2d0)))
       (c #.(- (* (log 2d0) (+ 1 (sqrt 2d0)))))
       (p #.(* (sqrt 2d0) (log 2d0)))
       (big-a (* a p))
       (big-b (* b p))
       (big-h 0.0026106723602095d0)
       (big-d (/ (* b b))))
  (declare (type double-float ln-2 a b c p big-a big-b big-h big-d))

  (defun gen-exponential-variate-ea
      (mu &optional (*random-state* *random-state*))
    (declare (ignore mu))
    (declare (type (double-float (0d0)) mu)
	     (optimize (speed 3)))
    (let ((u (random 1d0))
	  (u1 0d0)
	  (y 0d0)
	  (g c))
      (declare (type (double-float 0d0) u u1)
	       (type double-float g)
	       (type double-float y))
      (loop
	  (tagbody
	   step-2
	     (setf u (+ u u))
	     (when (>= u 1)
	       (go step-4))
	     (setf g (+ g ln-2))
	     (go step-2)
	   step-4
	     (setf u (- u 1))
	     (when (> u p)
	       (go step-6))
	     (return-from gen-exponential-variate-ea (+ g (/ big-a (- big-b u))))
	   step-6
	     (setf u (random 1d0))
	     (setf y (/ a (- b u)))
	     (setf u1 (random 1d0))
	     (when (> (* (+ (* u1 big-h) big-d)
			 (expt (- b u) 2))
		      (exp (- (+ y c))))
	       (go step-6))
	     (return-from gen-exponential-variate-ea (+ g y))))))

  (defun gen-exponential-variate-ea-2
      (mu &optional (*random-state* *random-state*))
    (declare (ignore mu))
    (let ((u (random 1d0))
	  (g c))
      (declare (double-float u g))
      (setf u (+ u u))
      (do ()
	  ((>= u 1))
	(incf g ln-2)
	(incf u u))

      (decf u 1)
      (when (<= u p)
	(return-from gen-exponential-variate-ea-2 (+ g (/ big-a (- big-b u)))))
      (loop
	  (let* ((u (random 1d0))
		 (y (/ a (- b u)))
		 (up (random 1d0)))
	    (declare (double-float u y up))
	    (when (<= (* (+ (* up big-h) big-d)
			 (expt (- b u) 2))
		      (exp (- (+ y c))))
	      (return-from gen-exponential-variate-ea-2 (+ g y)))))))
    )

;; Use the ratio-of-uniforms method to generate exponential variates.
;; This could probably be optimized further.
(defun gen-exponential-variate-ratio
    (mu &optional (*random-state* *random-state*))
  (declare (type (double-float (0d0)) mu))
  (let ((max-v (* mu #.(* 2 (exp -1d0)))))
  (do ((u (random 1d0) (random 1d0))
       (v (random max-v) (random max-v)))
      ((<= (* u u) (exp (- (/ v u mu))))
       (/ v u)))))

;; Marsaglia's Ziggurat method for generating exponential
;; variates. Note: this is slightly different from the version given
;; in his paper.  We changed the scaling from 2^32 to 2^31 because
;; CMUCL 18c on sparc doesn't do a good job of converting
;; (unsigned-byte 32) to floating-point because it doesn't have such
;; an instruction (only signed-bytes).  (Apparently x86 does.)  Tests
;; show that good exponential numbers are still generated.
(let ((r 7.69711747013104972d0))
  (flet ((density (x)
	   (declare (type (double-float 0d0) x))
	   (exp (- x))))
    (declare (inline density))
    (multiple-value-bind (k-table w-table f-table)
	(ziggurat-init 255 r 0.0039496598225815571993d0 31
		       #'density
		       #'(lambda (x)
			   (- (log x))))
      (defun gen-exponential-variate-ziggurat
          (mu &optional (*random-state* *random-state*))
	(declare (type (double-float 0d0) mu)
                 (optimize (speed 3)))
	(loop
	    (let* ((j (random (ash 1 31)))
		   (i (logand j 255))
		   (x (* j (aref w-table i))))
	      (when (< j (aref k-table i))
		(return (* mu x)))
	      (when (zerop i)
		(return (* mu (- r (log (random 1d0))))))
	      (when (< (* (random 1d0) (- (aref f-table (1- i))
						(aref f-table i)))
		       (- (density x) (aref f-table i)))
		(return (* mu x)))))))))

;;; Some timing results from running CMUCL 18c+ on a 866 MHz Pentium
;;; III:
;;;
;;; (cllib::time-expo 5000000)
;;;
;;; Method 	real	user	sys	cons
;;;
;;; Log		1.97	1.66	0.27	79998968
;;; Algo S	2.15	1.81	0.3	79998968
;;; SA		1.99	1.59	0.35	79998968
;;; EA		2.7	2.38	0.29	79998968
;;; EA-2	2.34	1.92	0.38	79998976
;;; Ratio	4.18	3.75	0.3	79998976
;;; Zigg	1.28	0.94	0.32	79998976
;;;
;;; On this platform, Margaglia's Ziggurat method is far and away the
;;; fastest.
;;;
;;; Results from CMUCL 2010-10 (roughly) on a Mac Mini with a 2.0 GHz
;;; Core 2 Duo using sse2:
;;;
;;; Method 	real	user	sys	cons
;;;
;;; Log		1.23	0.71	0.09	80000040
;;; Algo S	1.57	0.73	0.09	80000072
;;; SA		4.93	0.70	0.11	80000072
;;; EA		0.8	0.70	0.09	80000040
;;; EA-2	0.81	0.71	0.08	80000032
;;; Ratio	1.56	1.44	0.09	80000072
;;; Zigg	0.5	0.41	0.09	80000064
;;;
;;;
;;; For CMUCL 18c+ (with sparc-v9 changes) running on a 300 MHz Ultra 30:
;;; (cllib::time-expo 5000000)
;;;
;;; Method 	real	user	sys	cons
;;;
;;; Log		1.63	1.5	0.02	16000480
;;; Algo S	1.44	1.24	0.12	16000480
;;; SA		1.45	1.25	0.1	16000480
;;; EA		1.17	1.06	0.08	16000480
;;; EA-2	1.33	1.07	0.11	16000480
;;; Ratio	3.31	3.02	0.11	16000480
;;; Zigg	0.81	0.62	0.15	16000480
;;;
;;; So the Ziggurat method is quite a bit faster on this platform too.
;;;
;;; Pick the one that works best for you.

(defmacro gen-exponential-variate (mu &optional (state '*random-state*))
  `(gen-exponential-variate-ziggurat ,mu ,state))

