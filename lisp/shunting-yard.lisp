;;3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
;;3 4 2 * 1 5 - 2 3 ^ ^ / +

;;the shunting-yard algorithm is a method for parsing mathematical expressions specified in infix notation.
;;It can be used to produce output in Reverse Polish notation (RPN) or as an abstract syntax tree (AST).

;;(map 'string #'code-char #(72 101 108 108 111))


;;(a+(b*c))

;;(shunting-yard "(a+(b*c))")

(defparameter *op-orders* (make-hash-table))
(setf (gethash #\# *opcods*) 0)
(setf (gethash #\+ *opcods*) 1)
(setf (gethash #\- *opcods*) 1)
(setf (gethash #\* *opcods*) 2)
(setf (gethash #\/ *opcods*) 2)
(setf (gethash #\^ *opcods*) 3)

(defun op-less (a b)
  (<= (gethash a *opcods*) (gethash b *opcods*)))


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
        (if (or (CHAR= #\+ cur)
                (CHAR= #\- cur)
                (CHAR= #\* cur)
                (CHAR= #\/ cur)
                (CHAR= #\^ cur))
          (let ((top (vector-pop ops)))
            ;;(print top)
            (if (and (op-less cur top)
                     (CHAR/= #\# top))
              (vector-push-extend top output)
              (vector-push-extend top ops))
            (vector-push-extend cur ops))
          (vector-push-extend cur output)))))) 

;;test
(print (shunting-yard "a+b-c"))
