(defun pcg32-random (rng-state)
  (declare (type (simple-array (unsigned-byte 64) (2)) rng-state))
  ;; uint64_t oldstate = rng->state;
  ;; // Advance internal state
  ;; rng->state = oldstate * 6364136223846793005ULL + (rng->inc|1);
  (let* ((old-state (aref rng-state 0))
	 (new-state (+ (ldb (byte 64 0) (* old-state 6364136223846793005))
		       (logior 1 (aref rng-state 1))))
	 (xorshifted (ldb (byte 32 0)
			  (ash (logxor (ash old-state -18)
				  old-state)
			  -27)))
	 (rot (ldb (byte 32 0) (ash old-state -59)))
	 (result (logior (ash xorshifted (- rot))
			 (ldb (byte 32 0) (ash xorshifted (logand 31 (- rot)))))))
    ;;(format t "oldstate = ~D, new = ~D~%" old-state new-state)
    ;;(format t "  xorshifted = ~D rot ~D~%" xorshifted rot)
    (setf (aref rng-state 0) new-state)
    (ldb (byte 32 0) result)))

(defun pcg32-init-random (initstate initseq)
  (let ((s (make-array 2 :element-type '(unsigned-byte 64) :initial-element 0)))
    (setf (aref s 0) 0)
    (setf (aref s 1) (ldb (byte 64 0)
			  (logior (ash initseq 1) 1)))
    (pcg32-random s)
    (setf (aref s 0) (ldb (byte 64 0)
			  (+ (aref s 0) initstate)))
    (pcg32-random s)
    s))

(defun pcg32-bounded-rand (state bound)
  (declare (type (simple-array (unsigned-byte 64) (2)) state)
	   (type (unsigned-byte 32) bound))
  (let ((threshold (ldb (byte 32 0) (mod (- bound) bound))))
    ;;(format t "threshold = ~D~%" threshold)
    (loop
      (let ((r (pcg32-random state)))
	(when (>= r threshold)
	  (return-from pcg32-bounded-rand (mod r bound)))))))

(defun pcg32-test (rounds)
  (let ((state (pcg32-init-random 42 54)))
    (dotimes (k rounds)
      (format t "Round ~d:~%" (1+ k))
      (format t "  32bit:")
      (dotimes (n 6)
	(format t " 0x~8,'0x" (pcg32-random state)))
      (format t "~%  Coins: ")
      (dotimes (k 65)
	(format t "~C"
		(if (zerop (pcg32-bounded-rand state 2))
		    #\T
		    #\H)))
      (format t "~%  Rolls:")
      (dotimes (k 33)
	(format t " ~D"
		(1+ (pcg32-bounded-rand state 6))))
      (format t "~%  Cards:")
      (let ((number (map 'vector #'identity "A23456789TJQK"))
	    (suit (map 'vector #'identity "hcds")))
	(let ((cards (let ((c (make-array 52 :element-type '(unsigned-byte 8))))
		       (dotimes (k 52)
			 (setf (aref c k) k))
		       c)))
	  (loop for k from 52 above 1
		do
		   (let* ((chosen (pcg32-bounded-rand state k))
			  (card (aref cards chosen)))
		     (setf (aref cards chosen) (aref cards (1- k)))
		     (setf (aref cards (1- k)) card)))
	  (dotimes (k 52)
	    (multiple-value-bind (n s)
		(floor (aref cards k) 4)
	      (format t " ~C~C" (aref number n) (aref suit s))
	      (when (zerop (mod (1+ k) 22))
		(format t "~%~8t"))))
	  (format t "~%"))))))
      