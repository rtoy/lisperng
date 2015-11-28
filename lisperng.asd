;;; -*- Mode: lisp -*-
(asdf:defsystem lisperng
  :components
  ((:module package
	    :pathname ""
	    :components
	    ((:file "package")))
   (:module "distributions"
	    :depends-on ("package")
	    :components
	    ((:file "ziggurat")
	     (:file "rng-beta" :depends-on ("ziggurat"))
	     (:file "rng-binomial" :depends-on ("ziggurat"))
	     (:file "rng-cauchy" :depends-on ("ziggurat"))
	     (:file "rng-exponential" :depends-on ("ziggurat"))
	     (:file "rng-gamma" :depends-on ("ziggurat" "rng-exponential"))
	     (:file "rng-gaussian" :depends-on ("ziggurat"))
	     (:file "rng-geometric" :depends-on ("ziggurat"))
	     (:file "rng-laplace" :depends-on ("ziggurat"))
	     (:file "rng-poisson" :depends-on ("ziggurat"))))
   (:module "wellrng"
	    :components
	    ((:file "well-rng-package")
	     ;;(:file "well-common")
	     (:file "well-512a")
	     (:file "well-1024a")
	     (:file "well-19937a")
	     (:file "well-44497")))))
