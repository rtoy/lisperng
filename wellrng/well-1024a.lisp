;;; -*- Mode: lisp; Package: well-rng-1024 -*-
;;;
;;; Straightforward translation of well1024a.c
;;; http://www.iro.umontreal.ca/~panneton/WELLRNG.html

(in-package #:well-rng-1024)
(eval-when (:compile-toplevel :execute :load-toplevel)
(defconstant +well-1024a-w+ 32)
(defconstant +well-1024a-r+ 32)
(defconstant +well-1024a-m1+ 3)
(defconstant +well-1024a-m2+ 24)
(defconstant +well-1024a-m3+ 10)
;; I think this is what this is
(defconstant +well-1024a-fact+
  (/ (float #x100000000 1d0)))
)

(declaim (inline mat0))
(defun mat0 (tt v)
  (ldb (byte 32 0) (logxor v (ash v tt))))

(defstruct well-1024a-state
  (state-i 0 :type (integer 0 (#.+well-1024a-r+)))
  (state (make-array +well-1024a-r+ :element-type '(unsigned-byte 32))
	 :type (simple-array (unsigned-byte 32) (#.+well-1024a-r+))))

(defun init-well-rng-1024a (&optional init)
  (let ((state (make-well-1024a-state)))
    (if init
	(replace (well-1024a-state-state state) init)
	(let ((s (well-1024a-state-state state)))
	  (dotimes (k +well-1024a-r+)
	    (setf (aref s k) (random #x100000000)))))
    state))

#+cmu
(declaim (ext:maybe-inline well-rng-1024a))

(defun well-rng-1024a (rng-state)
  (declare (type well-1024a-state rng-state)
	   (optimize (speed 3) (safety 0)))
  ;; state-i = 0
  (let ((state-i (well-1024a-state-state-i rng-state))
	(state (well-1024a-state-state rng-state)))
    (declare (type (integer 0 #.+well-1024a-r+) state-i))
    (symbol-macrolet
	((v0 (aref state state-i))
	 (vm1 (aref state (logand #x1f (+ state-i +well-1024a-m1+))))
	 (vm2 (aref state (logand #x1f (+ state-i +well-1024a-m2+))))
	 (vm3 (aref state (logand #x1f (+ state-i +well-1024a-m3+))))
	 (vrm1 (aref state (logand #x1f (+ state-i 31))))
	 (newv0 (aref state (logand #x1f (+ state-i 31))))
	 (newv1 (aref state state-i)))
      (let* ((z0 vrm1)
	     (z1 (logxor v0
			 (mat0 -8 vm1)))
	     (z2 (logxor (mat0 19 vm2)
			 (mat0 14 vm3))))
	(setf newv1 (logxor z1 z2))
	(setf newv0 (logxor (mat0 11 z0)
			    (mat0 7 z1)
			    (mat0 13 z2)))
	(setf state-i (logand #x1f (+ state-i 31)))
	(setf (well-1024a-state-state-i rng-state) state-i)
	(* v0 +well-1024a-fact+)))))

;; Some timing results using CMUCL 2009-02 on a 1.6 GHz Ultrasparc.
;; (time-test 100000000) ->
;; Evaluation took:
;;   7.61 seconds of real time
;;   7.59 seconds of user run time
;;   0.0 seconds of system run time
;;   12,122,644,144 CPU cycles
;;   0 page faults and
;;   3,120 bytes consed.
;;
;; Evaluation took:
;;   6.65 seconds of real time
;;   6.65 seconds of user run time
;;   0.0 seconds of system run time
;;   10,589,713,088 CPU cycles
;;   0 page faults and
;;   3,408 bytes consed.
;; NOTE:  This tests inlined well-rng-44497 to reduce consing!
;;
;; So we see that WELL 1024a takes about 15% more time than the
;; reference.
(defun time-test (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 0)
		     (space 0)))
  (let ((state (init-well-rng-1024a
		(loop for k of-type fixnum from 0 below +well-1024a-r+
		   collect (random #xffffffff)))))
    (time (let ((sum 0d0))
	    (declare (double-float sum))
	    (dotimes (k n)
	      (incf sum (the double-float (well-rng-1024a state))))
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
    ;; Since well-1024a only contains 32 bits of randomness, we
    ;; should do the same if we want to compare speed.
    (time (let ((sum 0d0))
	    (declare (double-float sum))
	    (dotimes (k n)
	      (incf sum (* (random #x100000000)
			   (scale-float 1d0 -32))))
	    (print sum)))))

(defun test-well-rng ()
  (let ((state (init-well-rng-1024a (loop for k from 0 below +well-1024a-r+ collect k)))
	(sum 0d0))
    (declare (double-float sum))
    (dotimes (k 1000000)
      (incf sum (well-rng-1024a state)))
    (format t "sum = ~A~%" sum)
    (assert (= sum 499719.9355355669d0))
    (let ((res (loop for k from 1 to 10
		  collect (well-rng-1024a state))))
      (format t "~{~A~^~%~}~%" res)
      (assert (equal res
		     '(
		       0.5353222233243287d0
		       0.7527371069882065d0
		       0.08926743059419096d0
		       0.31788114295341074d0
		       0.5256528835743666d0
		       0.7189311101101339d0
		       0.983498826622963d0
		       0.2218896788544953d0
		       0.006630812305957079d0
		       0.9159802114591002d0))))))
