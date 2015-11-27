(defpackage com.github.lisperng
  (:use :cl)
  (:nicknames "LISPRNG")
  (:export "GEN-EXPONENTIAL-VARIATE-LOG-METHOD"
	   "GEN-EXPONENTIAL-VARIATE-ALGO-S"
	   "GEN-EXPONENTIAL-VARIATE-SA"
	   "GEN-EXPONENTIAL-VARIATE-ALGORITHM-MA"
	   "GEN-EXPONENTIAL-VARIATE-EA"
	   "GEN-EXPONENTIAL-VARIATE-EA-2"
	   "GEN-EXPONENTIAL-VARIATE-RATIO"
	   "GEN-EXPONENTIAL-VARIATE-ZIGGURAT"
	   "GEN-EXPONENTIAL-VARIATE"
	   "GEN-STD-LAPLACIAN-VARIATE"
	   "GEN-CAUCHY-VARIATE-TAN"
	   "GEN-CAUCHY-VARIATE"
	   "GEN-CAUCHY-VARIATE-ALGORITHM-CA"
	   "GEN-GAUSSIAN-VARIATE-POLAR"
	   "GEN-GAUSSIAN-VARIATE-ALGORITHM-NA"
	   "GEN-GAUSSIAN-VARIATE-BOX-TRIG"
	   "GEN-GAUSSIAN-VARIATE-RATIO"
	   "GEN-GAUSSIAN-VARIATE-ZIGGURAT"
	   "GEN-GAUSSIAN-VARIATE"
	   "GEN-GAMMA-VARIATE-SQUEEZE"
	   "GEN-GAMMA-VARIATE-GN"
	   "GEN-GAMMA-VARIATE-ALGO-A"
	   "GEN-GAMMA-VARIATE-ALGO-A-2"
	   "GEN-GAMMA-VARIATE-SMALL-ORDER"
	   "GEN-GAMMA-VARIATE-DIRECT"
	   "GEN-GAMMA-VARIATE-ALGO-GO"
	   "GEN-GAMMA-VARIATE-RATIO"
	   "GEN-GAMMA-VARIATE"
	   "GEN-GEOMETRIC-VARIATE"
	   "GEN-BETA-VARIATE"
	   "GEN-BINOMIAL-VARIATE"
	   "GEN-POISSON-VARIATE")
  (:documentation "Lisp Random Number Generators"))