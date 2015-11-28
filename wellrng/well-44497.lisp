;;; -*- Mode: lisp; Package: well-rng-44497 -*-
;;;
;;; Straightforward translation of well44497a.c
;;;
;;; See http://www.iro.umontreal.ca/~panneton/WELLRNG.html

(in-package #:well-rng-44497)
(eval-when (:compile-toplevel :execute :load-toplevel)
(defconstant +well-44497-w+ 32)
(defconstant +well-44497-r+ 1391)
(defconstant +well-44497-p+ 15)
)


(eval-when (:compile-toplevel :execute :load-toplevel)
(defconstant +well-44497-masku+
  (ash #xffffffff (- +well-44497-p+ +well-44497-w+)))

(defconstant +well-44497-maskl+
  (ldb (byte 32 0) (lognot +well-44497-masku+)))

(defconstant +well-44497-m1+ 23)
(defconstant +well-44497-m2+ 481)
(defconstant +well-44497-m3+ 229)

;; I think this is what this is
(defconstant +well-44497-fact+
  (/ (float #x100000000 1d0)))

#+well-44497b
(defconstant +well-44497-temperb+ #x93dd1400)
#+well-44497b
(defconstant +well-44497-temperc+ #xfa118000)
)

(defmacro maybe-temper (x)
  #+well-44497b
  `(let* ((y1 (logand #xffffffff
		      (logxor ,x
			      (logand +well-44497-temperb+
				      (ash ,x 7)))))
	  (y (logand #xffffffff
		     (logxor y1 (logand +well-44497-temperc+
				       (ash y1 15))))))
     (* y +well-44497-fact+))
  #-well-44497b
  `(* ,x +well-44497-fact+))

(declaim (inline mat0pos mat0neg mat1 mat2 mat3pos mat3neg mat4pos mat4neg mat5 mat7))

(defun mat0pos (tt v)
  (ldb (byte 32 0) (logxor v (ash v (- tt)))))

(defun mat0neg (tt v)
  (ldb (byte 32 0) (logxor v (ash v (- tt)))))

(defun mat1 (v)
  v)

(defun mat2 (a v)
  (ldb (byte 32 0)
       (if (logbitp 0 v)
	   (logxor (ash v -1) a)
	   (logxor (ash v -1)))))

(defun mat3pos (tt v)
  (ldb (byte 32 0) (ash v (- tt))))

(defun mat3neg (tt v)
  (ldb (byte 32 0) (ash v (- tt))))

(defun mat4pos (tt b v)
  (ldb (byte 32 0)
       (logxor v (logand (ash v (- tt)) b))))

(defun mat4neg  (tt b v)
  (ldb (byte 32 0)
       (logxor v (logand (ash v (- tt)) b))))

(defun mat5 (r a ds dt v)
  (if (zerop (logand v dt))
      (ldb (byte 32 0)
	   (logand (logxor (ash v r)
			   (ash v (- r +well-44497-w+)))
		   ds))
      (ldb (byte 32 0)
	   (logxor (logand (logxor (ash v r)
				   (ash v (- r +well-44497-w+)))
			   ds)
		   a))))

(defstruct well-44497-state
  (case 1 :type (integer 1 6))
  (state-i 0 :type (integer 0 (#.+well-44497-r+)))
  (state (make-array +well-44497-r+ :element-type '(unsigned-byte 32))
	 :type (simple-array (unsigned-byte 32) (#.+well-44497-r+))))
	 

(defun init-well-rng-44497 (&optional init)
  (let ((state (make-well-44497-state)))
    (if init
	(replace (well-44497-state-state state) init)
	(let ((s (well-44497-state-state state)))
	  (dotimes (k +well-44497-r+)
	    (setf (aref s k) (random #x100000000)))))
    state))

#+cmu
(declaim (ext:maybe-inline well-rng-44497))

(defun well-rng-44497 (state)
  (declare (type well-44497-state state)
	   (optimize (speed 3) (safety 0)))
  (flet
      ((case-1 (rng-state)
	 (declare (type well-44497-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 ;; state-i = 0
	 (let ((state-i (well-44497-state-state-i rng-state))
	       (state (well-44497-state-state rng-state)))
	   (declare (type (integer 0 #.+well-44497-r+) state-i))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1 (aref state (+ state-i +well-44497-m1+)))
		(vm2 (aref state (+ state-i +well-44497-m2+)))
		(vm3 (aref state (+ state-i +well-44497-m3+)))
		(vrm1under (aref state (+ state-i +well-44497-r+ -1)))
		(vrm2under (aref state (+ state-i +well-44497-r+ -2)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-44497-r+ -1))))
	     (let ((z0 (logior (logand vrm1under +well-44497-maskl+)
			       (logand vrm2under +well-44497-masku+)))
		   (z1 (logxor (mat0neg -24 v0)
			       (mat0pos 30 vm1)))
		   (z2 (logxor (mat0neg -10 vm2)
			       (mat3neg -26 vm3))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0under (logxor (mat1 z0)
					(mat0pos 20 z1)
					(mat5 9 #xb729fcec #xfbffffff #x00020000 z2)
					(mat1 newv1)))
	       (setf state-i (- +well-44497-r+ 1))
	       (setf (well-44497-state-case rng-state) 3)
	       (setf (well-44497-state-state-i rng-state) state-i)
	       (maybe-temper v0)))))
       (case-2 (rng-state)
	 (declare (type well-44497-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 (let ((state-i (well-44497-state-state-i rng-state))
	       (state (well-44497-state-state rng-state)))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1 (aref state (+ state-i +well-44497-m1+)))
		(vm2 (aref state (+ state-i +well-44497-m2+)))
		(vm3 (aref state (+ state-i +well-44497-m3+)))
		(vrm1 (aref state (- state-i 1)))
		(vrm1under (aref state (+ state-i +well-44497-r+ -1)))
		(vrm2under (aref state (+ state-i +well-44497-r+ -2)))
		(newv0 (aref state (- state-i 1)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-44497-r+ -1))))
	     (let ((z0 (logior (logand vrm1 +well-44497-maskl+)
			       (logand vrm2under +well-44497-masku+)))
		   (z1 (logxor (mat0neg -24 v0)
			       (mat0pos 30 vm1)))
		   (z2 (logxor (mat0neg -10 vm2)
			       (mat3neg -26 vm3))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0 (logxor (mat1 z0)
				   (mat0pos 20 z1)
				   (mat5 9 #xb729fcec #xfbffffff #x00020000 z2)
				   (mat1 newv1)))
	       (setf state-i 0)
	       (setf (well-44497-state-case rng-state) 1)
	       (setf (well-44497-state-state-i rng-state) state-i)
	       (maybe-temper v0)))))
       (case-3 (rng-state)
	 (declare (type well-44497-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 (let ((state-i (well-44497-state-state-i rng-state))
	       (state (well-44497-state-state rng-state)))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1over (aref state (+ state-i +well-44497-m1+
					(- +well-44497-r+))))
		(vm2over (aref state (+ state-i +well-44497-m2+
					(- +well-44497-r+))))
		(vm3over (aref state (+ state-i +well-44497-m3+
					(- +well-44497-r+))))
		(vrm1 (aref state (- state-i 1)))
		(vrm2 (aref state (- state-i 2)))
		(vrm1under (aref state (+ state-i +well-44497-r+ -1)))
		(vrm2under (aref state (+ state-i +well-44497-r+ -2)))
		(newv0 (aref state (- state-i 1)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-44497-r+ -1))))
	     (let ((z0 (logior (logand vrm1 +well-44497-maskl+)
			       (logand vrm2 +well-44497-masku+)))
		   (z1 (logxor (mat0neg -24 v0)
			       (mat0pos 30 vm1over)))
		   (z2 (logxor (mat0neg -10 vm2over)
			       (mat3neg -26 vm3over))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0 (logxor (mat1 z0)
				   (mat0pos 20 z1)
				   (mat5 9 #xb729fcec #xfbffffff #x00020000 z2)
				   (mat1 newv1)))
	       (decf state-i)
	       (when (< (+ state-i +well-44497-m1+) +well-44497-r+)
		 (setf (well-44497-state-case rng-state) 4))
	       (setf (well-44497-state-state-i rng-state) state-i)
	       (maybe-temper v0)))))
       (case-4 (rng-state)
	 (declare (type well-44497-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 (let ((state-i (well-44497-state-state-i rng-state))
	       (state (well-44497-state-state rng-state)))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1 (aref state (+ state-i +well-44497-m1+)))
		(vm2 (aref state (+ state-i +well-44497-m2+)))
		(vm2over (aref state (+ state-i +well-44497-m2+
					(- +well-44497-r+))))
		(vm3over (aref state (+ state-i +well-44497-m3+
					(- +well-44497-r+))))
		(vrm1 (aref state (- state-i 1)))
		(vrm2 (aref state (- state-i 2)))
		(vrm1under (aref state (+ state-i +well-44497-r+ -1)))
		(vrm2under (aref state (+ state-i +well-44497-r+ -2)))
		(newv0 (aref state (- state-i 1)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-44497-r+ -1))))
	     (let ((z0 (logior (logand vrm1 +well-44497-maskl+)
			       (logand vrm2 +well-44497-masku+)))
		   (z1 (logxor (mat0neg -24 v0)
			       (mat0pos 30 vm1)))
		   (z2 (logxor (mat0neg -10 vm2over)
			       (mat3neg -26 vm3over))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0 (logxor (mat1 z0)
				   (mat0pos 20 z1)
				   (mat5 9 #xb729fcec #xfbffffff #x00020000 z2)
				   (mat1 newv1)))
	       (decf state-i)
	       (when (< (+ state-i +well-44497-m3+) +well-44497-r+)
		 (setf (well-44497-state-case rng-state) 5))
	       (setf (well-44497-state-state-i rng-state) state-i)
	       (maybe-temper v0)))))
       (case-5 (rng-state)
	 (declare (type well-44497-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 (let ((state-i (well-44497-state-state-i rng-state))
	       (state (well-44497-state-state rng-state)))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1 (aref state (+ state-i +well-44497-m1+)))
		(vm2over (aref state (+ state-i +well-44497-m2+
					(- +well-44497-r+))))
		(vm3over (aref state (+ state-i +well-44497-m3+
					(- +well-44497-r+))))
		(vm3 (aref state (+ state-i +well-44497-m3+)))
		(vrm1 (aref state (- state-i 1)))
		(vrm2 (aref state (- state-i 2)))
		(vrm1under (aref state (+ state-i +well-44497-r+ -1)))
		(vrm2under (aref state (+ state-i +well-44497-r+ -2)))
		(newv0 (aref state (- state-i 1)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-44497-r+ -1))))
	     (let ((z0 (logior (logand vrm1 +well-44497-maskl+)
			       (logand vrm2 +well-44497-masku+)))
		   (z1 (logxor (mat0neg -24 v0)
			       (mat0pos 30 vm1)))
		   (z2 (logxor (mat0neg -10 vm2over)
			       (mat3neg -26 vm3))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0 (logxor (mat1 z0)
				   (mat0pos 20 z1)
				   (mat5 9 #xb729fcec #xfbffffff #x00020000 z2)
				   (mat1 newv1)))
	       (decf state-i)
	       (when (< (+ state-i +well-44497-m2+) +well-44497-r+)
		 (setf (well-44497-state-case rng-state) 6))
	       (setf (well-44497-state-state-i rng-state) state-i)
	       (maybe-temper v0)))))
       (case-6 (rng-state)
	 (declare (type well-44497-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 (let ((state-i (well-44497-state-state-i rng-state))
	       (state (well-44497-state-state rng-state)))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1 (aref state (+ state-i +well-44497-m1+)))
		(vm2 (aref state (+ state-i +well-44497-m2+)))
		(vm3 (aref state (+ state-i +well-44497-m3+)))
		(vrm1 (aref state (- state-i 1)))
		(vrm2 (aref state (- state-i 2)))
		(vrm1under (aref state (+ state-i +well-44497-r+ -1)))
		(vrm2under (aref state (+ state-i +well-44497-r+ -2)))
		(newv0 (aref state (- state-i 1)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-44497-r+ -1))))
	     (let ((z0 (logior (logand vrm1 +well-44497-maskl+)
			       (logand vrm2 +well-44497-masku+)))
		   (z1 (logxor (mat0neg -24 v0)
			       (mat0pos 30 vm1)))
		   (z2 (logxor (mat0neg -10 vm2)
			       (mat3neg -26 vm3))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0 (logxor (mat1 z0)
				   (mat0pos 20 z1)
				   (mat5 9 #xb729fcec #xfbffffff #x00020000 z2)
				   (mat1 newv1)))
	       (decf state-i)
	       (when (= state-i 1)
		 (setf (well-44497-state-case rng-state) 2))
	       (setf (well-44497-state-state-i rng-state) state-i)
	       (maybe-temper v0))))))
    ;;(format t "case = ~A~%" (well-44497-state-case state))
    (ecase (well-44497-state-case state)
      (1 (case-1 state))
      (2 (case-2 state))
      (3 (case-3 state))
      (4 (case-4 state))
      (5 (case-5 state))
      (6 (case-6 state)))))

;; Some timing results using CMUCL 2009-02 on a 1.6 GHz Ultrasparc.
;; (time-test 100000000) ->
;; Evaluation took:
;;   9.29 seconds of real time
;;   9.28 seconds of user run time
;;   0.0 seconds of system run time
;;   14,795,594,336 CPU cycles
;;   0 page faults and
;;   3,272 bytes consed.
;; 
;; Evaluation took:
;;   6.65 seconds of real time
;;   6.65 seconds of user run time
;;   0.0 seconds of system run time
;;   10,588,780,240 CPU cycles
;;   0 page faults and
;;   3,168 bytes consed.
;;
;; NOTE:  This tests inlined well-rng-44497 to reduce consing!
;;
;; So we see that WELL 19937 takes about 40% more time than the
;; reference.
(defun time-test (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 0)
		     (space 0)))
  (let ((state (init-well-rng-44497
		(loop for k of-type fixnum from 0 below +well-44497-r+
		   collect (random #xffffffff)))))
    (time (let ((sum 0d0))
	    (declare (double-float sum))
	    (dotimes (k n)
	      (incf sum (the double-float (well-rng-44497 state))))
	    (print sum)))
    ;; This test may not be testing what we want.  In particular, in
    ;; CMUCL, (random 1d0) calls the 32-bit generator twice to produce
    ;; 53 bits of randomness in each float.
    #+nil
    (time (let ((sum 0d0))
	    (declare (double-float sum))
	    (dotimes (k n)
	      (incf sum (random 1d0)))
	    (print sum)))
    ;; Since well-44497 only contains 32 bits of randomness, we
    ;; should do the same if we want to compare speed.
    (time (let ((sum 0d0))
	    (declare (double-float sum))
	    (dotimes (k n)
	      (incf sum (* (random #x100000000)
			   (scale-float 1d0 -32))))
	    (print sum)))))

(defun test-well-rng ()
  (let ((state (init-well-rng-44497 (loop for k from 0 below +well-44497-r+ collect k)))
	(sum 0d0))
    (declare (double-float sum))
    (dotimes (k 1000000)
      (incf sum (well-rng-44497 state)))
    (format t "sum = ~A~%" sum)
    (assert (= sum
	       #-well-44497b 499841.06391439447d0
	       #+well-44497b 500192.12015948305d0))
    (let ((res (loop for k from 1 to 10
		  collect (well-rng-44497 state))))
      (format t "~{~A~^~%~}~%" res)
      (assert (equal res
		     #-well-44497b
		     '(0.36787368170917034d0
		       0.3763212179765105d0
		       0.9651780363637954d0
		       0.6972852696198970d0
		       0.7564442928414792d0
		       0.22904176358133554d0
		       0.2385061145760119d0
		       0.3089646704029292d0
		       0.8744725501164794d0
		       0.02105285576544702d0)
		     #+well-44497b
		     '(0.24366642348468304d0
		       0.6684504961594939d0
		       0.5539775129873306d0
		       0.07040080917067826d0
		       0.47924526291899383d0
		       0.6081454241648316d0
		       0.8997149826027453d0
		       0.42622083495371044d0
		       0.4268862521275878d0
		       0.30759026459418237d0))))))
