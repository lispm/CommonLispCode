;;; Copyright Rainer Joswig, joswig@lisp.de, 2012

;;; Example for creating a vector of random numbers, using N-THREADS

#-lispworks6
(cerror "only runs in LispWorks 6 and above using the barrier functionality")

(defun make-random-vector (&key (size 10000) (n-threads 4))
  (let ((vector  (make-sequence 'vector size))
        (barrier (mp:make-barrier (1+ n-threads)))
        (delta   (truncate size n-threads)))
    (loop for i below n-threads
          do (mp:process-run-function
              "initializing vector"
              nil
              (lambda (vector start end barrier)
                (loop for i from start below end do
                      (setf (svref vector i) (random 1.0)))
                (mp:barrier-wait barrier :pass-through t))
              vector
              (* i delta)
              (+ delta (* i delta))
              barrier))
    (mp:barrier-wait barrier)
    vector))
