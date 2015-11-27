;;; Beta random variable

(in-package :com.github.lisperng)

(defun gen-beta-variate (a b &optional (*random-state* *random-state*))
  "Generate a pseudo-random number from a beta distribution function
with parameters a and b:

                   B - 1  A - 1
            (1 - X)      X
     F(X) = -------------------
                BETA(A, B)


 where BETA(A, B) is


          GAMMA(A) GAMMA(B)
          -----------------
            GAMMA(B + A)

 The method uses the fact that

           X1
         -------
         X2 + X1
 is a beta variate if X1 is Gamma of order A, and X2 is Gamma of order
 B.

 A      = first parameter of beta density, A > 0
 B      = second parameter, B > 0
 STATE  = random state to use.
"
  (declare (type (double-float (0d0)) a b))
  (let ((x1 (gen-gamma-variate a))
	(x2 (gen-gamma-variate b)))
    (/ x1 (+ x1 x2))))

