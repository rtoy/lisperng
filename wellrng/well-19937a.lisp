;;; -*- Mode: lisp; Package: well-rng-1024 -*-
;;;
;;; Straightforward translation of well19937a.c
;;;
;;; See http://www.iro.umontreal.ca/~panneton/WELLRNG.html

(in-package #:well-rng-19937)
(eval-when (:compile-toplevel :execute :load-toplevel)
(defconstant +well-19937-w+ 32)
(defconstant +well-19937-r+ 624)
(defconstant +well-19937-p+ 31)
)

(defconstant +masku+
  (ash #xffffffff (- (- +well-19937-w+ +well-19937-p+))))
(defconstant +maskl+
  (logand #xffffffff (lognot +masku+)))
(defconstant +well-19937-m1+ 70)
(defconstant +well-19937-m2+ 179)
(defconstant +well-19937-m3+ 449)

;; I think this is what this is
(defconstant +well-19937-fact+
  (/ (float #x100000000 1d0)))


(declaim (inline mat0 mat1 mat3pos))
(defun mat0 (tt v)
  (ldb (byte 32 0) (logxor v (ash v tt))))

(defun mat1 (v)
  v)

(defun mat3pos (tt v)
  (ldb (byte 32 0) (ash v tt)))

(defstruct well-19937-state
  (case 1 :type (integer 1 6))
  (state-i 0 :type (integer 0 (#.+well-19937-r+)))
  (state (make-array +well-19937-r+ :element-type '(unsigned-byte 32))
	 :type (simple-array (unsigned-byte 32) (#.+well-19937-r+))))

#+well-19937c
(defconstant +well-19937-temperb+ #xe46e1700)
#+well-19937c
(defconstant +well-19937-temperc+ #x9b868000)

(defmacro maybe-temper (x)
  #+well-19937c
  `(let* ((y1 (logand #xffffffff
		      (logxor ,x
			      (logand +well-19937-temperb+
				      (ash ,x 7)))))
	  (y (logand #xffffffff
		     (logxor y1 (logand +well-19937-temperc+
				       (ash y1 15))))))
     (* y +well-19937-fact+))
  #-well-19937c
  `(* ,x +well-19937-fact+))
	 
(declaim (inline well-rng-19937))

(defun well-rng-19937 (state)
  (declare (type well-19937-state state)
	   (optimize (speed 3) (safety 0)))
  (flet
      ((case-1 (rng-state)
	 (declare (type well-19937-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 ;; state-i = 0
	 (let ((state-i (well-19937-state-state-i rng-state))
	       (state (well-19937-state-state rng-state)))
	   (declare (type (integer 0 #.+well-19937-r+) state-i))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1 (aref state (+ state-i +well-19937-m1+)))
		(vm2 (aref state (+ state-i +well-19937-m2+)))
		(vm3 (aref state (+ state-i +well-19937-m3+)))
		(vrm1under (aref state (+ state-i +well-19937-r+ -1)))
		(vrm2under (aref state (+ state-i +well-19937-r+ -2)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-19937-r+ -1))))
	     (let ((z0 (logior (logand vrm1under +maskl+)
			       (logand vrm2under +masku+)))
		   (z1 (logxor (mat0 25 v0)
			       (mat0 -27 vm1)))
		   (z2 (logxor (mat3pos -9 vm2)
			       (mat0 -1 vm3))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0under (logxor (mat1 z0)
					(mat0 9 z1)
					(mat0 21 z2)
					(mat0 -21 newv1)))
	       (setf state-i (- +well-19937-r+ 1))
	       (setf (well-19937-state-case rng-state) 3)
	       (setf (well-19937-state-state-i rng-state) state-i)
	       (maybe-temper v0)))))
       (case-2 (rng-state)
	 (declare (type well-19937-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 (let ((state-i (well-19937-state-state-i rng-state))
	       (state (well-19937-state-state rng-state)))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1 (aref state (+ state-i +well-19937-m1+)))
		(vm2 (aref state (+ state-i +well-19937-m2+)))
		(vm3 (aref state (+ state-i +well-19937-m3+)))
		(vrm1 (aref state (- state-i 1)))
		(vrm1under (aref state (+ state-i +well-19937-r+ -1)))
		(vrm2under (aref state (+ state-i +well-19937-r+ -2)))
		(newv0 (aref state (- state-i 1)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-19937-r+ -1))))
	     (let ((z0 (logior (logand vrm1 +maskl+)
			       (logand vrm2under +masku+)))
		   (z1 (logxor (mat0 25 v0)
			       (mat0 -27 vm1)))
		   (z2 (logxor (mat3pos -9 vm2)
			       (mat0 -1 vm3))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0 (logxor (mat1 z0)
				   (mat0 9 z1)
				   (mat0 21 z2)
				   (mat0 -21 newv1)))
	       (setf state-i 0)
	       (setf (well-19937-state-case rng-state) 1)
	       (setf (well-19937-state-state-i rng-state) state-i)
	       (maybe-temper v0)))))
       (case-3 (rng-state)
	 (declare (type well-19937-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 (let ((state-i (well-19937-state-state-i rng-state))
	       (state (well-19937-state-state rng-state)))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1over (aref state (+ state-i +well-19937-m1+
					(- +well-19937-r+))))
		(vm2over (aref state (+ state-i +well-19937-m2+
					(- +well-19937-r+))))
		(vm3over (aref state (+ state-i +well-19937-m3+
					(- +well-19937-r+))))
		(vrm1 (aref state (- state-i 1)))
		(vrm2 (aref state (- state-i 2)))
		(vrm1under (aref state (+ state-i +well-19937-r+ -1)))
		(vrm2under (aref state (+ state-i +well-19937-r+ -2)))
		(newv0 (aref state (- state-i 1)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-19937-r+ -1))))
	     (let ((z0 (logior (logand vrm1 +maskl+)
			       (logand vrm2 +masku+)))
		   (z1 (logxor (mat0 25 v0)
			       (mat0 -27 vm1over)))
		   (z2 (logxor (mat3pos -9 vm2over)
			       (mat0 -1 vm3over))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0 (logxor (mat1 z0)
				   (mat0 9 z1)
				   (mat0 21 z2)
				   (mat0 -21 newv1)))
	       (decf state-i)
	       (when (< (+ state-i +well-19937-m1+) +well-19937-r+)
		 (setf (well-19937-state-case rng-state) 5))
	       (setf (well-19937-state-state-i rng-state) state-i)
	       (maybe-temper v0)))))
       (case-4 (rng-state)
	 (declare (type well-19937-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 (let ((state-i (well-19937-state-state-i rng-state))
	       (state (well-19937-state-state rng-state)))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1 (aref state (+ state-i +well-19937-m1+)))
		(vm2 (aref state (+ state-i +well-19937-m2+)))
		(vm3over (aref state (+ state-i +well-19937-m3+
					(- +well-19937-r+))))
		(vrm1 (aref state (- state-i 1)))
		(vrm2 (aref state (- state-i 2)))
		(vrm1under (aref state (+ state-i +well-19937-r+ -1)))
		(vrm2under (aref state (+ state-i +well-19937-r+ -2)))
		(newv0 (aref state (- state-i 1)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-19937-r+ -1))))
	     (let ((z0 (logior (logand vrm1 +maskl+)
			       (logand vrm2 +masku+)))
		   (z1 (logxor (mat0 25 v0)
			       (mat0 -27 vm1)))
		   (z2 (logxor (mat3pos -9 vm2)
			       (mat0 -1 vm3over))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0 (logxor (mat1 z0)
				   (mat0 9 z1)
				   (mat0 21 z2)
				   (mat0 -21 newv1)))
	       (decf state-i)
	       (when (< (+ state-i +well-19937-m3+) +well-19937-r+)
		 (setf (well-19937-state-case rng-state) 6))
	       (setf (well-19937-state-state-i rng-state) state-i)
	       (maybe-temper v0)))))
       (case-5 (rng-state)
	 (declare (type well-19937-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 (let ((state-i (well-19937-state-state-i rng-state))
	       (state (well-19937-state-state rng-state)))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1 (aref state (+ state-i +well-19937-m1+)))
		(vm2over (aref state (+ state-i +well-19937-m2+
					(- +well-19937-r+))))
		(vm3over (aref state (+ state-i +well-19937-m3+
					(- +well-19937-r+))))
		(vrm1 (aref state (- state-i 1)))
		(vrm2 (aref state (- state-i 2)))
		(vrm1under (aref state (+ state-i +well-19937-r+ -1)))
		(vrm2under (aref state (+ state-i +well-19937-r+ -2)))
		(newv0 (aref state (- state-i 1)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-19937-r+ -1))))
	     (let ((z0 (logior (logand vrm1 +maskl+)
			       (logand vrm2 +masku+)))
		   (z1 (logxor (mat0 25 v0)
			       (mat0 -27 vm1)))
		   (z2 (logxor (mat3pos -9 vm2over)
			       (mat0 -1 vm3over))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0 (logxor (mat1 z0)
				   (mat0 9 z1)
				   (mat0 21 z2)
				   (mat0 -21 newv1)))
	       (decf state-i)
	       (when (< (+ state-i +well-19937-m2+) +well-19937-r+)
		 (setf (well-19937-state-case rng-state) 4))
	       (setf (well-19937-state-state-i rng-state) state-i)
	       (maybe-temper v0)))))
       (case-6 (rng-state)
	 (declare (type well-19937-state rng-state)
		  (optimize (speed 3) (safety 0)))
	 (let ((state-i (well-19937-state-state-i rng-state))
	       (state (well-19937-state-state rng-state)))
	   (symbol-macrolet
	       ((v0 (aref state state-i))
		(vm1 (aref state (+ state-i +well-19937-m1+)))
		(vm2 (aref state (+ state-i +well-19937-m2+)))
		(vm3 (aref state (+ state-i +well-19937-m3+)))
		(vrm1 (aref state (- state-i 1)))
		(vrm2 (aref state (- state-i 2)))
		(vrm1under (aref state (+ state-i +well-19937-r+ -1)))
		(vrm2under (aref state (+ state-i +well-19937-r+ -2)))
		(newv0 (aref state (- state-i 1)))
		(newv1 (aref state state-i))
		(newv0under (aref state (+ state-i +well-19937-r+ -1))))
	     (let ((z0 (logior (logand vrm1 +maskl+)
			       (logand vrm2 +masku+)))
		   (z1 (logxor (mat0 25 v0)
			       (mat0 -27 vm1)))
		   (z2 (logxor (mat3pos -9 vm2)
			       (mat0 -1 vm3))))
	       (setf newv1 (logxor z1 z2))
	       (setf newv0 (logxor (mat1 z0)
				   (mat0 9 z1)
				   (mat0 21 z2)
				   (mat0 -21 newv1)))
	       (decf state-i)
	       (when (= state-i 1)
		 (setf (well-19937-state-case rng-state) 2))
	       (setf (well-19937-state-state-i rng-state) state-i)
	       (maybe-temper v0))))))
    (ecase (well-19937-state-case state)
      (1 (case-1 state))
      (2 (case-2 state))
      (3 (case-3 state))
      (4 (case-4 state))
      (5 (case-5 state))
      (6 (case-6 state)))))

(defun init-well-rng-19937 (&optional init)
  (let ((state (make-well-19937-state)))
    (if init
	(replace (well-19937-state-state state) init)
	(let ((s (well-19937-state-state state)))
	  (dotimes (k +well-19937-r+)
	    (setf (aref s k) (random #x100000000)))))
  state))
  

;; Some timing results using CMUCL 2009-02 on a 1.6 GHz Ultrasparc.
;; (time-test 100000000) ->
;; Evaluation took:
;;   8.06 seconds of real time
;;   8.06 seconds of user run time
;;   0.0 seconds of system run time
;;   12,845,364,752 CPU cycles
;;   0 page faults and
;;   3,368 bytes consed.
;; 
;; Evaluation took:
;;   6.65 seconds of real time
;;   6.63 seconds of user run time
;;   0.0 seconds of system run time
;;   10,586,723,968 CPU cycles
;;   0 page faults and
;;   3,168 bytes consed.
;;
;; NOTE:  This tests inlined well-rng-19937 to reduce consing!
;;
;; So we see that WELL 19937 takes about 20% more time than the
;; reference.
;;
(defun time-test (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0)))
  (let ((state (init-well-rng-19937
		(loop for k of-type fixnum from 0 below +well-19937-r+
		   collect (random #xffffffff)))))
    (time (let ((sum 0d0))
	    (declare (double-float sum))
	    (dotimes (k n)
	      (incf sum (the double-float (well-rng-19937 state))))
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
    ;; Since well-19937 only contains 32 bits of randomness, we
    ;; should do the same if we want to compare speed.
    (time (let ((sum 0d0))
	    (declare (double-float sum))
	    (dotimes (k n)
	      (incf sum (* (random #x100000000)
			   (scale-float 1d0 -32))))
	    (print sum)))))

(defun test-well-rng ()
  (let ((state (init-well-rng-19937 (loop for k from 0 below 624 collect k)))
	(sum 0d0))
    (declare (double-float sum))
    (dotimes (k 1000000)
      (incf sum (well-rng-19937 state)))
    (format t "sum = ~A~%" sum)
    (assert (= sum
	       #-well-19937c 499825.4391014336d0
	       #+well-19937c 499858.8421096562d0))
    (let ((res (loop for k from 1 to 10
		  collect (well-rng-19937 state))))
      (format t "~{~A~^~%~}~%" res)
      (assert (equal res
		     #-well-19937c
		     '(0.3730275493580848d0
		       0.9711934693623334d0
		       0.25652473443187773d0
		       0.28815762395970523d0
		       0.7461836116854101d0
		       0.06806588033214211d0
		       0.3029438881203532d0
		       0.261084889061749d0
		       0.5892307865433395d0
		       0.5558243873529136d0)
		     #+well-19937c
		     '(0.889407740207389d0
		       0.6666052707005292d0
		       0.019150428706780076d0
		       0.7189352333080024d0
		       0.6519386547151953d0
		       0.7423223839141428d0
		       0.07531339209526777d0
		       0.22458598483353853d0
		       0.4657556521706283d0
		       0.6603473904542625d0))))))
