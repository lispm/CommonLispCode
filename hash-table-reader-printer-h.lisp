;;; -*- Mode: LISP; Package: ("HASH-TABLE-READER-PRINTER" :USE "COMMON-LISP"); BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;; a simple hash-table reader and printer in Common Lisp

;;; Copyright 2012, Rainer Joswig, joswig@lisp.de

;;; Tested in LispWorks 6.1.

;;; use:
;;;   reads #H((a 1) (b 2)) as a hash-table
;;;   arguments can determine the test function:
;;;   this use EQUALP:  #3H(("a" 1) ("B" 2))

;;; ================================================================
;;; Package HASH-TABLE-READER-PRINTER

(cl:defpackage "HASH-TABLE-READER-PRINTER"
  (:use "COMMON-LISP")
  (:export
   "INSTALL-HASH-TABLE-SYNTAX"
   "PRINT-HASH-TABLE"
   ))

(in-package "HASH-TABLE-READER-PRINTER")


;;; ================================================================
;;; reading

(defparameter *comparison-functions*
  '(eq eql equal equalp))

(defun read-hash-table (stream character n &aux (delimiter #\)))
  (declare (ignore character))
  (read-char stream)
  (let ((table (make-hash-table :test
                                (if n
                                    (nth n *comparison-functions*)
                                  'eql))))
    (loop until (char= (peek-char t stream) delimiter)
          do (destructuring-bind (key value)
                 (read stream)
               (setf (gethash key table) value)))
    (read-char stream)
    table))

(defun install-hash-table-syntax (&optional (readtable *readtable*))
  (set-dispatch-macro-character #\# #\H 'read-hash-table)
  (set-dispatch-macro-character #\# #\h 'read-hash-table)
  readtable)

(install-hash-table-syntax)

;;; ================================================================
;;; printing

(defun print-hash-table (hash-table &optional (stream *standard-output*))
  (write-char #\# stream)
  (cond ((eq (hash-table-test hash-table) 'eql) 'do-nothing)
        ((member (hash-table-test hash-table) *comparison-functions*)
         (write (position (hash-table-test hash-table) *comparison-functions*)
                :stream stream)))                          
  (write-string "H(" stream)
  (let ((first-p t))
    (maphash (lambda (key value)
               (if first-p
                   (setf first-p (not first-p))
                 (write-char #\space stream))
               (prin1 (list key value) stream))
             hash-table))
  (write-char #\) stream)
  hash-table)

(defun pprint-hash-table (*standard-output* hash-table)
  (pprint-logical-block
      (*standard-output*
       nil
       :prefix (cond ((eq (hash-table-test hash-table) 'eql)
                      "#H(")
                     ((member (hash-table-test hash-table)
                              *comparison-functions*)
                      (format nil "#~aH("
                              (position (hash-table-test hash-table)
                                        *comparison-functions*)))
                     (t "#H("))
         
       :suffix ")")
    (let ((end (hash-table-count hash-table)) (i 0))
      (when (plusp end)
        (block printing
          (maphash (lambda (key value)
                     (pprint-pop)
                     (write (list key value))
                     (if (= (incf i) end) (return-from printing nil))
                     (write-char #\Space)
                     (pprint-newline :fill))
                   hash-table))))))

(set-pprint-dispatch 'hash-table 'pprint-hash-table)


;;; ================================================================
;;; Examples

; #H((a 5) (c 6) (b 3))           ; uses EQL   as the test function
; #2H(("a" 5) ("c" 6) ("b" 3))    ; uses EQUAL as the test function


;;; ================================================================
;;; End of File
