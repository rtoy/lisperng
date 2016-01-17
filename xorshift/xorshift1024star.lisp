(declaim (type (integer 0 31) *p*)
	 (type (simple-array (unsigned-byte 32) (32)) *s*))
(defvar *p* 0)
(defvar *s* (make-array 32 :element-type '(unsigned-byte 32)))

(defun init-xor-shift-1024* (init)
  (setf *p* 0)
  (dotimes (k 32)
    (setf (aref *s* k) (aref init k))))

(defun xor-shift-1024* ()
  (declare (optimize (speed 3)))
  (let ((s0l (aref *s* *p*))
	(s0h (aref *s* (+ *p* 1))))
    (setf *p* (logand 31 (+ *p* 2)))
    (let ((s1l (aref *s* *p*))
	  (s1h (aref *s* (+ *p* 1))))
      (multiple-value-bind (t1 t0)
	  (64bit-ashl s1h s1l 31)
	(setf s1h (logxor s1h t1)
	      s1l (logxor s1l t0)))
      (let ((new-l (logxor s1l s0l))
	    (new-h (logxor s1h s0h)))
	(declare (type (unsigned-byte 32) new-l new-h))
	(multiple-value-bind (t1 t0)
	    (64bit-ashr s1h s1l 11)
	  (setf new-h (logxor new-h t1)
		new-l (logxor new-l t0)))
	(multiple-value-bind (t1 t0)
	    (64bit-ashr s0h s0l 30)
	  (setf new-h (logxor new-h t1)
		new-l (logxor new-l t0)))
	(setf (aref *s* *p*) new-l
	      (aref *s* (+ *p* 1)) new-h)
	(let ((mul-l (ldb (byte 32 0) 1181783497276652981))
	      (mul-h (ldb (byte 32 32) 1181783497276652981)))
	  #+cmu
	  (multiple-value-bind (hi lo)
	      (bignum::%multiply mul-l new-l)
	    (let ((p (ldb (byte 32 0)
			  (+ (* mul-h new-l)
			     (* mul-l new-h)))))
	      (+ (ash (ldb (byte 32 0)
			   (+ hi p))
		      32)
		 lo))))))))
	

(defun test-xor-shift-1024* ()
  (init-xor-shift-64 #x1234567887654321)
  (format t "Seed:~%")
  (let ((init-s (make-array 32 :element-type '(unsigned-byte 32))))
    (dotimes (k 16)
      (let ((s (xor-shift-64)))
	(format t "~2d ~d~%" k s) 
	(setf (aref init-s (* 2 k))
	      (ldb (byte 32 0) s))
	(setf (aref init-s (+ 1 (* 2 k)))
	      (ldb (byte 32 32) s))))
    (init-xor-shift-1024* init-s)
    (format t "Outputs:~%")
    (dotimes (k 16)
      (format t "~2d ~D~%" k (xor-shift-1024*)))))