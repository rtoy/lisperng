;;; Geometric random variable

(in-package :com.github.lisperng)

(defun gen-geometric-variate (p &optional (*random-state* *random-state*))
  (declare (type (non-negative-float double-float (1d0)) p) (optimize (speed 3)))
  (let ((u (random 1d0)))
    (values (ceiling (/ (log u) (log (- 1 p)))))))

