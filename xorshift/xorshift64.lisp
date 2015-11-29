(let ((x (make-array 2 :element-type '(unsigned-byte 32))))
  (declare (type (simple-array (unsigned-byte 32) (2)) x))

  (defun init-xor-shift-64 (seed)
    (declare (integer seed))
    (setf (aref x 0) (ldb (byte 32 0) seed))
    (setf (aref x 1) (ldb (byte 32 32) seed)))

  (defun xor-shift-64 ()
    (declare (optimize (speed 3)))
    (let ((t0 (aref x 0))
	  (t1 (aref x 1)))
      (declare (type (unsigned-byte 32) t0 t1))
      ;; x ^= x >> 12
      (psetf t0 (logxor t0 (logior (ash t0 -12)
				   (ash (ldb (byte 12 0) t1) 20)))
	     t1 (logxor t1 (ash t1 -12)))
      ;; x ^= x << 25
      (psetf t0 (logxor t0 (ldb (byte 32 0) (ash t0 25)))
	     t1 (logxor t1 (logior (ldb (byte 32 0) (ash t1 25))
				   (ldb (byte 25 7) t0))))
      ;; x ^= x >> 27
      (psetf t0 (logxor t0 (logior (ash t0 -27)
				   (ash (ldb (byte 27 0) t1) 5)))
	     t1 (logxor t1 (ash t1 -27)))
      (setf (aref x 0) t0
	    (aref x 1) t1)
      #-cmu
      (ldb (byte 64 0)
	   (+ (* 1332534557 t0)
	      (ash (ldb (byte 32 0)
			(+ (* 625341585 t0)
			   (* 1332534557 t1)))
		   32)))
      #+cmu
      (multiple-value-bind (hi lo)
	  (bignum::%multiply 1332534557 t0)
	(let ((p (ldb (byte 32 0)
			(+ (* 625341585 t0)
			   (* 1332534557 t1)))))
	  (+ (ash (ldb (byte 32 0)
		       (+ hi p))
		  32)
	     lo))))))

				      
  