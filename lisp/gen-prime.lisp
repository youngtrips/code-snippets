(defparameter *primes* (make-hash-table))
(defun gen-prime (y x)
    (do ((i 2 (+ i 1)))
        ((> i x))
        (if (not (gethash i *primes*))
            (do ((j i (+ j 1)))
                ((> (* i j) x))
                (setf (gethash (* i j) *primes*) t))))
     (do ((i y (+ i 1)))
        ((> i x))
        (if (and (not (gethash i *primes*))
                (/= i 1))
            (format t "~a~%" i))))

(dotimes (cases (read))
    (let ((m (read))
          (n (read)))
        (gen-prime m n)))
