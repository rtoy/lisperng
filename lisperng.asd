;;; -*- Mode: lisp -*-
(asdf:defsystem lisperng
  :components
  ((:file "package")
   (:file "rng" :depends-on ("package"))
   (:file "rng-beta" :depends-on ("rng"))
   (:file "rng-binomial" :depends-on ("rng"))
   (:file "rng-cauchy" :depends-on ("rng"))
   (:file "rng-exponential" :depends-on ("rng"))
   (:file "rng-gamma" :depends-on ("rng" "rng-exponential"))
   (:file "rng-gaussian" :depends-on ("rng"))
   (:file "rng-geometric" :depends-on ("rng"))
   (:file "rng-laplace" :depends-on ("rng"))
   (:file "rng-poisson" :depends-on ("rng"))))