;;;;  Class of Random number generators
;;;;

;; Do we really need to get cllib-base and cllib-withtype just to get
;; dfloat defined, which is basically a short-cut for (float x 1d0)?
;;
;; And I (rtoy) think most of the uses of dfloat below are not needed,
;; except for (dfloat pi) since pi is a long-float and the code
;; expects a double-float.  The other uses of dfloat should be needed
;; because the compiler should automatically convert the rationals to
;; a double-float anyway.  (Perhaps this is an issue with clisp's
;; contagion implementation?)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `dfloat', `with-type'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype")))

(in-package :cllib)

(export '(gen-exponential-variate-log-method gen-exponential-variate-algo-s
          gen-exponential-variate-sa gen-exponential-variate-algorithm-ma
          gen-exponential-variate-ea gen-exponential-variate-ea-2
          gen-exponential-variate-ratio gen-exponential-variate-ziggurat
          gen-exponential-variate
          gen-std-laplacian-variate
          gen-cauchy-variate-tan gen-cauchy-variate
          gen-cauchy-variate-algorithm-ca
          gen-gaussian-variate-polar gen-gaussian-variate-algorithm-na
          gen-gaussian-variate-box-trig gen-gaussian-variate-ratio
          gen-gaussian-variate-ziggurat gen-gaussian-variate
          gen-gamma-variate-squeeze gen-gamma-variate-gn
          gen-gamma-variate-algo-a gen-gamma-variate-algo-a-2
          gen-gamma-variate-small-order gen-gamma-variate-direct
          gen-gamma-variate-algo-go gen-gamma-variate-ratio
          gen-gamma-variate
          gen-geometric-variate
          gen-beta-variate
          gen-binomial-variate
          gen-poisson-variate))

;; CLOCC should not do this, IMO:
;; (eval-when (:compile-toplevel)
;;   (declaim (optimize (speed 3))))

(deftype non-negative-float (type &optional hi)
  `(or (member ,(coerce 0 type))
       (,type (,(coerce 0 type)) ,(or hi *))))

;; Initialize tables for Marsaglia's Ziggurat method of generating
;; random numbers.  See http://www.jstatsoft.org for a reference.
;;
;; Let 0 = x[0] < x[1] < x[2] <...< x[n].  Select a set of rectangles
;; with common area v such that
;;
;; x[k]*(f(x[k-1]) - f(x[k])) = v
;;
;; and
;;
;;              inf
;; v = r*f(r) + int f(x) dx
;;               r
;;
;; where r = x[n].
;;
(defun ziggurat-init (n r v scale f finv)
  ;; n = one less than the number of elements in the tables
  ;; r = x[n]
  ;; v = common area term
  ;; scale = 2^scale is the scaling to use to make integers
  ;; f = density function
  ;; finv = inverse density function
  (let ((x (make-array (1+ n) :element-type 'double-float))
	(fx (make-array (1+ n) :element-type 'double-float))
	(k-table (make-array (1+ n) :element-type '(unsigned-byte 32)))
	(w-table (make-array (1+ n) :element-type 'double-float)))
    (setf (aref x n) r)
    (loop for k from (1- n) downto 1 do
	  (let ((prev (aref x (1+ k))))
	    (setf (aref x k) (funcall finv (+ (/ v prev)
					      (funcall f prev))))
	    (setf (aref fx k) (funcall f (aref x k)))))

    (setf (aref x 0) 0d0)
    (setf (aref fx 0) (funcall f (aref x 0)))
    (setf (aref fx n) (funcall f (aref x n)))

    (loop for k from 1 to n do
	  (setf (aref k-table k)
		(floor (scale-float (/ (aref x (1- k)) (aref x k)) scale)))
	  (setf (aref w-table k)
		(* (aref x k) (expt .5d0 scale))))

    (setf (aref k-table 0) (floor (scale-float (/ (* r (funcall f r)) v) scale)))
    (setf (aref w-table 0) (* (/ v (funcall f r)) (expt 0.5d0 scale)))
    (values k-table w-table fx)))


#||
 (defun time-expo (n)
  (declare (fixnum n))
  (flet (#+cmu
	 (timer (f)
	   (let ((func (coerce f 'function)))
	     (gc)
	     (format t "~A~%" f)
	     (system:without-gcing
	      (time (dotimes (k n)
		      (declare (fixnum k))
		      (funcall func 1d0))))))
	 #-cmu
	 (timer (f)
	   (let ((func (coerce f 'function)))
	     (gc)
	     (format t "~&~A~%" f)
	     (time (dotimes (k n)
		     (declare (fixnum k))
		     (funcall func 1d0))))))
    (declare (inline timer))
    (dolist (f (list #'gen-exponential-variate-log-method
		     #'gen-exponential-variate-algo-s
		     #'gen-exponential-variate-sa
		     #'gen-exponential-variate-ea
		     #'gen-exponential-variate-ea-2
		     #'gen-exponential-variate-ratio
		     #'gen-exponential-variate-ziggurat))
      (timer f))))

 (defun time-gaussian (n)
  (declare (fixnum n))
  (flet (#+cmu
	 (timer (f)
	   (let ((func (coerce f 'function)))
	     (gc)
	     (format t "~A~%" f)
	     (system:without-gcing
	      (time (dotimes (k n)
		      (declare (fixnum k))
		      (funcall func))))))
	 #-cmu
	 (timer (f)
	   (let ((func (coerce f 'function)))
	     (gc)
	     (format t "~&~A~%" f)
	     (time (dotimes (k n)
		     (declare (fixnum k))
		     (funcall func))))))
    (declare (inline timer))
    (dolist (f (list #'gen-gaussian-variate-polar
		     #'gen-gaussian-variate-algorithm-na
		     #'gen-gaussian-variate-box-trig
		     #'gen-gaussian-variate-ratio
		     #'gen-gaussian-variate-ziggurat))
      (timer f))))

 (defun time-cauchy (n)
  (declare (fixnum n))
  (gc)
  (format t "gen-cauchy-variate-tan~%")
  (system:without-gcing
   (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-cauchy-variate-tan))))

  (gc)
  (format t "gen-cauchy-variate-algorithm-ca~%")
  (system:without-gcing
   (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-cauchy-variate-algorithm-ca))))
  )

 (defun time-gamma (n a)
  (declare (fixnum n))
  (flet ((timer (f)
	   (let ((func (coerce f 'function)))
	     (gc)
	     (format t "~A~%" f)
	     (system:without-gcing
	      (time (dotimes (k n)
		      (declare (fixnum k))
		      (funcall func a)))))))
    (declare (inline timer))
    (dolist (f (list #'gen-gamma-variate-squeeze
		     #'gen-gamma-variate-gn
		     #'gen-gamma-variate-algo-a
		     #'gen-gamma-variate-algo-a-2
		     #'gen-gamma-variate-algo-go
		     ))
      (timer f))))

;;;
;;; Some simple routines for plotting histograms.  This is meant to be
;;; used as a simple means of testing the generators above.

 (defun make-hist-centers (lo hi intervals)
  (let ((center (make-array intervals))
	(step (/ (- hi lo) intervals)))
    (dotimes (k intervals)
      (setf (aref center k) (+ lo (* k step) (/ step 2))))
    center))

 (defun make-hist (x &key lo hi (intervals 10))
  (let* ((lo-limit (or lo (reduce #'min x)))
	 (hi-limit (or hi (reduce #'max x)))
	 (hist (make-array intervals :initial-element 0))
	 (step (/ (- hi-limit lo-limit) intervals)))
    (dotimes (k (length x))
      (let ((posn (truncate (/ (- (aref x k) lo-limit) step))))
	(cond ((minusp posn)
	       (incf (aref hist 0)))
	      ((>= posn intervals)
	       (incf (aref hist (- intervals 1))))
	      (t
	       (incf (aref hist posn))))))
    (values step hist (make-hist-centers lo-limit hi-limit intervals))))


 (defun plot-hist (x &key (intervals 10) lo hi)
  (multiple-value-bind (step count center)
      (make-hist x :intervals intervals :lo lo :hi hi)
    (format t "step = ~A~%" step)
    (with-open-file (s "/tmp/out" :direction :output)
      (let ((n (reduce #'+ count)))
	(dotimes (k (length count))
	  (format s "~A ~A~%"
		  (float (aref center k) 1.0)
		  (float (/ (aref count k) n step))))
	(format s "~%")
	(dotimes (k (length count))
	  (format s "~A ~A~%"
		  (float (aref center k) 1.0)
		  (exp (- (float (aref center k) 1.0)))))))))


 (defun plot-hist-pdf (x pdf &key (intervals 10) lo hi)
  (multiple-value-bind (step count center)
      (make-hist x :intervals intervals :lo lo :hi hi)
    (format t "step = ~A~%" step)
    (with-open-file (s "/tmp/out" :direction :output)
      (let ((n (reduce #'+ count)))
	(dotimes (k (length count))
	  (format s "~A ~A~%"
		  (float (aref center k) 1.0)
		  (float (/ (aref count k) n step))))
	(format s "~%")
	(dotimes (k (length count))
	  (format s "~A ~A~%"
		  (float (aref center k) 1.0)
		  (float (funcall pdf (aref center k)))))))))

 (defun rng-expo-histogram (n gen)
  (let ((m 2d0)
	(r (make-array n :element-type 'double-float)))
    (dotimes (k n)
      (setf (aref r k) (funcall gen m)))
    (plot-hist-pdf r #'(lambda (x)
			 (declare (double-float x))
			 (/ (exp (- (/ x m))) m))
		   :intervals 50 :lo 0 :hi (* 10 m))))

 (defun rng-gaussian-histogram (n gen)
  (let ((r (make-array n :element-type 'double-float)))
    (dotimes (k n)
      (setf (aref r k) (funcall gen)))
    (plot-hist-pdf r #'(lambda (x)
			 (declare (double-float x))
			 (* (/ (sqrt (* 2 pi))) (exp (* -0.5d0 x x))))
		   :intervals 50 :lo -5 :hi 5)))

 (defun rng-cauchy-histogram (n gen &key (limit 100))
  (let ((r (make-array n :element-type 'double-float)))
    (dotimes (k n)
      (setf (aref r k) (funcall gen)))
    (plot-hist-pdf r #'(lambda (x)
			 (/ (* pi (+ 1 (* x x)))))
		   :intervals 500 :lo (- limit) :hi limit)))

 (defun rng-gamma-histogram (n a gamma gen)
  (let ((r (make-array n :element-type 'double-float)))
    (dotimes (k n)
      (setf (aref r k) (funcall gen a)))
    (plot-hist-pdf r #'(lambda (x)
			 (/ (* (expt x (- a 1)) (exp (- x)))
			    gamma))
		   :intervals 50 :lo 0 :hi (* 5 a))))

||#

(provide :cllib-rng)
;;; file rng.lisp ends here
