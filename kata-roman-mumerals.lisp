;;; Code Kata: Roman Numerals
;;; Rainer Joswig, 2012

(defparameter *roman-number-descriptors*
  '((M 1000 900   C)
    (D 500  400   C)
    (C 100   90   X)
    (L 50    40   X)
    (X 10     9   I)
    (V 5      4   I)
    (I 1      1 nil))
  "list of (roman-digit decimal-digit interval-start prefix-digit)")

(defun roman-digit-to-decimal-value (roman-digit)
  "Returns the decimal value for a roman digit."
  (second (assoc roman-digit *roman-number-descriptors*)))

(defun find-descriptor (n)
  "find the roman number descriptor for a number"
  (find-if (lambda (desc)
             (destructuring-bind (r d s p)
                 desc
               (declare (ignore r d p))
               (>= n s)))
           *roman-number-descriptors*))

(defun print-as-roman (n)
  "prints the integer as a roman number"
  (check-type n (integer 1 3000))
  (loop while (plusp n) do
        (destructuring-bind (r d s p)
            (find-descriptor n)
          (declare (ignore s))
          (when (and (< n d) p)
            (write p)
            (incf n (roman-digit-to-decimal-value p)))
          (write r)
          (decf n d))))

(defun test-all-roman-numbers ()
  (macrolet ((str (&body body)
               `(with-output-to-string (*standard-output*)
                  ,@body)))
    (loop for i from 1 upto 3000
          always (string= (str (format t "~@R" i))
                          (str (print-as-roman i))))))


