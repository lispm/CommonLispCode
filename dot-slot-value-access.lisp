;;; Dot notation for slot values

;;; Rainer Joswig, joswig@lisp.de, 2012


; Example
;
; #!turtle.drawing.pen-down-p
;
;  expands to
;
; (slot-value (slot-value turtle 'drawing) 'pen-down-p)
;


(defun make-calls (string)
  (let ((symbols (reverse
                  (mapcar (lambda (string)
                            (intern string))
                          (split-sequence '(#\.) string)))))
    (labels ((nest (list)
               (if (not (cddr list))
                   `(slot-value ,(second list) ',(first list))
                 `(slot-value ,(nest (cdr list))
                              ',(first list)))))
      (nest symbols))))

(defun read-instance-slot-value (stream subchar arg)
  (declare (ignore subchar arg))
  (make-calls (symbol-name (let ((*readtable* (copy-readtable nil)))
                             (read stream)))))

(set-dispatch-macro-character
  #\# #\!
  #'read-instance-slot-value)


