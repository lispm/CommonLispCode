;;; Code Kata: Roman Numerals
;;; Rainer Joswig, 2012

(defparameter *roman-number-descriptors*
  '((M 1000 900)
    (D 500  400)
    (C 100   90)
    (L 50    40)
    (X 10     9)
    (V 5      4)
    (I 1      1))
  "list of (roman-digit decimal-digit interval-start)")

(defun roman-digit-to-decimal-value (roman-digit)
  "Returns the decimal value for a roman digit."
  (second (assoc roman-digit *roman-number-descriptors*)))

(defun find-descriptor (n)
  "find the roman number descriptor for a number"
  (find-if (lambda (desc)
             (>= n (third desc)))
           *roman-number-descriptors*))

(defun print-as-roman (n)
  "prints the integer as a roman number"
  (check-type n (integer 1 3000))
  (loop while (plusp n) do
        (destructuring-bind (r d s)
            (find-descriptor n)
          (let ((p (- d s)))
            (when (and (< n d) (plusp p))
              (write (first (find-descriptor p)))
              (incf n p)))
          (write r)
          (decf n d))))

(defun test-all-roman-numbers ()
  (macrolet ((str (&body body)
               `(with-output-to-string (*standard-output*)
                  ,@body)))
    (loop for i from 1 upto 3000
          always (string= (str (format t "~@R" i))
                          (str (print-as-roman i))))))


