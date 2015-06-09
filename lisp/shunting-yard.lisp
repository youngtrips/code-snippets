;;3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
;;3 4 2 * 1 5 - 2 3 ^ ^ / +

;;the shunting-yard algorithm is a method for parsing mathematical expressions specified in infix notation.
;;It can be used to produce output in Reverse Polish notation (RPN) or as an abstract syntax tree (AST).

;;(map 'string #'code-char #(72 101 108 108 111))


;;(a+(b*c))

;;(shunting-yard "(a+(b*c))")

(defparameter *op-orders* (make-hash-table))
(setf (gethash #\# *op-orders*) 0)
(setf (gethash #\+ *op-orders*) 1)
(setf (gethash #\- *op-orders*) 1)
(setf (gethash #\* *op-orders*) 2)
(setf (gethash #\/ *op-orders*) 2)
(setf (gethash #\^ *op-orders*) 3)

(defun op-less (a b)
  (<= (gethash a *op-orders*) (gethash b *op-orders*)))


(defun gen-res (output ops)
  (loop
    (when (<= (length ops) 1) (return))
    (vector-push-extend (vector-pop ops) output))
  output)


(defun shunting-yard (exp)
  (let ((output (make-array 1 :fill-pointer 0 :adjustable t :element-type 'character))
        (ops (make-array 1 :fill-pointer 0 :adjustable t :element-type 'character)))
    (vector-push-extend #\# ops)
    (do ((i 0 (+ i 1)))
      ((>= i (length exp)) (gen-res output ops))
      (let ((cur (elt exp i)))
        (cond ((or (CHAR= #\+ cur)
                   (CHAR= #\- cur)
                   (CHAR= #\* cur)
                   (CHAR= #\/ cur)
                   (CHAR= #\^ cur))
               (let ((top (vector-pop ops)))
                 (if (and (CHAR/= #\# top)
                          (CHAR/= #\( top)
                          (op-less cur top))
                   (vector-push-extend top output)
                   (vector-push-extend top ops))
                 (vector-push-extend cur ops)))
              ((CHAR= #\( cur) (vector-push-extend cur ops))
              ((CHAR= #\) cur) 
               (let ((op (vector-pop ops)))
                 (loop
                   (when (CHAR= #\( op) (return))
                   (vector-push-extend op output)
                   (setf op (vector-pop ops)))))
              (t  (vector-push-extend cur output))
              
              )))))

(dotimes (cases (read-from-string (read-line)))
  (let ((exp (read-line)))
    (print (shunting-yard exp))))
