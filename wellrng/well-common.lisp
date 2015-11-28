;;; Some common routines for all of the WELL RNGs.

(declaim (inline mat0pos mat0neg mat3neg mat4neg))
(defun mat0pos (tt v)
  (ldb (byte 32 0) (logxor v (ash v (- tt)))))

(defun mat0neg (tt v)
  (ldb (byte 32 0) (logxor v (ash v (- tt)))))

(defun mat3neg (tt v)
  (logand #xffffffff (ash v (- tt))))

(defun mat4neg (tt b v)
  (ldb (byte 32 0) (logxor v (logand (ash v (- tt))
				     b))))

