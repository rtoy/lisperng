;;;;-------------------------------------------------------------------------
;;;;
;;;; Laplacian PDF
;;;;
;;;; f(x) = 1/2*exp(-|x|)
;;;;
;;;;-------------------------------------------------------------------------

(defun gen-std-laplacian-variate (&optional (*random-state* *random-state*))
  "Generate a pseudo-random number for a Laplacian random variable, defined by

         1    -|X|
 f(X) = --- %E
         2

 for real X.
"
  ;; Instead of using the inverse CDF to generate the random number,
  ;; we generate an exponential and flip its sign with probability 1/2.
  (if (zerop (random 2))
      (gen-exponential-variate 1d0)
      (- (gen-exponential-variate 1d0))))
