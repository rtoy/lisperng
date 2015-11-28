;;; -*- Mode: lisp; Package: well-rng-512 -*-
;;;
;;; Straightforward translation of well512a.c
;;; http://www.iro.umontreal.ca/~panneton/WELLRNG.html

(in-package #:well-rng-512)
(eval-when (:compile-toplevel :execute :load-toplevel)
(defconstant +well-512a-w+ 32)
(defconstant +well-512a-r+ 16)
(defconstant +well-512a-p+ 0)
(defconstant +well-512a-m1+ 13)
(defconstant +well-512a-m2+ 9)
(defconstant +well-512a-m3+ 5)
;; I think this is what this is
(defconstant +well-512a-fact+
  (/ (float #x100000000 1d0)))
)

(declaim (inline mat0 mat3neg mat4neg))
(defun mat0 (tt v)
  (ldb (byte 32 0) (logxor v (ash v tt))))

(defun mat3neg (tt v)
  (ldb (byte 32 0) (ash v tt)))

(defun mat4neg (tt b v)
  (ldb (byte 32 0) (logxor v (logand (ash v tt)
				     b))))

(defstruct well-512a-state
  (state-i 0 :type (integer 0 (#.+well-512a-r+)))
  (state (make-array +well-512a-r+ :element-type '(unsigned-byte 32))
	 :type (simple-array (unsigned-byte 32) (#.+well-512a-r+))))

(defun init-well-rng-512a (&optional init)
  (let ((state (make-well-512a-state)))
    (if init
	(replace (well-512a-state-state state) init)
	(let ((s (well-512a-state-state state)))
	  (dotimes (k +well-512a-r+)
	    (setf (aref s k) (random #x100000000)))))
    state))

#+cmu
(declaim (ext:maybe-inline well-rng-512a))

(defun well-rng-512a (rng-state)
  (declare (type well-512a-state rng-state)
	   (optimize (speed 3) (safety 0)))
  ;; state-i = 0
  (let ((state-i (well-512a-state-state-i rng-state))
	(state (well-512a-state-state rng-state)))
    (declare (type (integer 0 #.+well-512a-r+) state-i))
    (symbol-macrolet
	((v0 (aref state state-i))
	 (vm1 (aref state (logand #xf (+ state-i +well-512a-m1+))))
	 (vm2 (aref state (logand #xf (+ state-i +well-512a-m2+))))
	 (vm3 (aref state (logand #xf (+ state-i +well-512a-m3+))))
	 (vrm1 (aref state (logand #xf (+ state-i 15))))
	 (vrm2 (aref state (logand #xf (+ state-i 14))))
	 (newv0 (aref state (logand #xf (+ state-i 15))))
	 (newv1 (aref state state-i))
	 (newvrm1 (aref state (logand #xf (+ state-i 14)))))
      (let* ((z0 vrm1)
	     (z1 (logxor (mat0 16 v0)
			 (mat0 15 vm1)))
	     (z2 (mat0 -11 vm2)))
	(setf newv1 (logxor z1 z2))
	(setf newv0 (logxor (mat0 2 z0)
			    (mat0 18 z1)
			    (mat3neg 28 z2)
			    (mat4neg 5 #xda442d24 newv1)))
	(setf state-i (logand #xf (+ state-i 15)))
	(setf (well-512a-state-state-i rng-state) state-i)
	(* v0 +well-512a-fact+)))))

;; Some timing results using CMUCL 2009-02 on a 1.6 GHz Ultrasparc.
;; (time-test 100000000) ->
;; Evaluation took:
;;   6.03 seconds of real time
;;   6.02 seconds of user run time
;;   0.0 seconds of system run time
;;   9,605,374,368 CPU cycles
;;   0 page faults and
;;   3,024 bytes consed.
;; 
;; Evaluation took:
;;   6.65 seconds of real time
;;   6.65 seconds of user run time
;;   0.0 seconds of system run time
;;   10,595,199,424 CPU cycles
;;   0 page faults and
;;   3,224 bytes consed.
;; NOTE:  This tests inlined well-rng-512a to reduce consing!
;;
;; So we see that WELL 512a takes about 10% less time than the
;; reference.

(defun time-test (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 0)
		     (space 0)))
  (let ((state (init-well-rng-512a
		(loop for k of-type fixnum from 0 below +well-512a-r+
		   collect (random #xffffffff)))))
    (time (let ((sum 0d0))
	    (declare (double-float sum))
	    (dotimes (k n)
	      (incf sum (the double-float (well-rng-512a state))))
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
    ;; Since well-512a only contains 32 bits of randomness, we
    ;; should do the same if we want to compare speed.
    (time (let ((sum 0d0))
	    (declare (double-float sum))
	    (dotimes (k n)
	      (incf sum (* (random #x100000000)
			   (scale-float 1d0 -32))))
	    (print sum)))))

(defun test-well-rng ()
  (let ((state (init-well-rng-512a (loop for k of-type fixnum from 0 below 16 collect k)))
	(sum 0d0))
	(declare (double-float sum))
    (locally (declare (optimize (speed 3)))
      (time
       (dotimes (k 1000000)
	 (incf sum (well-rng-512a state)))))
    (format t "sum = ~A~%" (+ 0d0 sum))
    (assert (= sum 499722.4444413881d0))
    (let ((res (loop for k from 1 to 10
		  collect (well-rng-512a state))))
      (format t "~{~A~^~%~}~%" res)
      (assert (equal res
		     '(
		       0.5281058573164046d0
		       0.7351445145905018d0
		       0.5812297654338181d0
		       0.21008714428171515d0
		       0.4534814874641597d0
		       0.4457075875252485d0
		       0.17911080038174987d0
		       0.5836300207301974d0
		       0.15211484325118363d0
		       0.24706594995222986d0))))))
