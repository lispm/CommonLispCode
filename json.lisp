;;; -*- Mode: LISP; Package: ("JSON" :USE "COMMON-LISP"); BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;; a simple JSON reader in Common Lisp

;;; http://www.ietf.org/rfc/rfc4627.txt
;;; http://www.json.org/
;;; http://en.wikipedia.org/wiki/JSON

;;; Copyright 2010, Rainer Joswig, joswig@lisp.de

;;; This simple JSON reader uses the standard Common Lisp reader facility.

;;; use:
;;  (json-read stream eof-errop-p eof-value recursivep)
;;  (json-reader t)   installs the reader in the current readtable

(defpackage "JSON"
  (:use "COMMON-LISP")
  (:export "JSON-READ" "JSON-READER"))

(in-package "JSON")

(defun json-string-reader (stream first-char)
  "This function implements a reader for JSON strings. It should be
used with the Common Lisp reader as a macro character function."
  (declare (ignore first-char))
  (labels ((read-unicode-character (stream)
             (code-char (+ (ash (digit-char-p (read-char stream) 16) 12)
                           (ash (digit-char-p (read-char stream) 16) 8)
                           (ash (digit-char-p (read-char stream) 16) 4)
                           (digit-char-p (read-char stream) 16))))
           (read-escaped-character (stream)
             (ecase (read-char stream)
               (#\" #\")
               (#\\ #\\)
               (#\/ #\/)
               (#\b #\backspace)
               (#\f #\formfeed)
               (#\n #\newline)
               (#\r #\return)
               (#\t #\tab)
               (#\u (read-unicode-character stream)))))
    (with-output-to-string (out nil :element-type 'character)
      (loop for char = (read-char stream)
            until (char= char #\")
            do (write-char (if (char= char #\\)
                               (read-escaped-character stream)
                             char)
                           out)))))

(defmacro with-json-string-reader (&body body)
  "Install the JSON string reader temporarily during body execution."
  (let ((fn-sym (gensym "FN")))
    `(let ((,fn-sym (get-macro-character #\" *readtable*)))
       (unwind-protect
           (progn
             (set-macro-character #\" 'json-string-reader nil *readtable*)
             ,@body)
         (set-macro-character #\" ,fn-sym nil *readtable*)))))

(defun convert-json-array (list)
  "Takes a list and returns a vector."
  (coerce list 'vector))

(defun json-array-reader (stream first-char)
  "This function implements a reader for JSON arrays. It should be
used with the Common Lisp reader as a macro character function."
  (declare (ignore first-char))
  (with-json-string-reader
    (convert-json-array (prog1 (loop for char = (peek-char t stream)
                                     until (char= char #\])
                                     collect (read stream)
                                     when (char= (peek-char t stream) #\,)
                                     do (read-char stream))
                          (read-char stream)))))

(defun convert-json-object (list)
  "Converts a list of keys and values to an assoc list."
  (loop for (key value) on list by #'cddr
        collect (cons key value)))

(defparameter *json-read-objects-as-type* :clos
  "one of :clos, :hash-table or :list")

(defun read-object-as-list (stream)
  (loop until (char= (peek-char t stream) #\})
        collect (cons (read stream)
                      (progn
                        (peek-char #\: stream)
                        (read-char stream)
                        (peek-char t stream)
                        (read stream)))
        when (char= (peek-char t stream) #\,)
        do (read-char stream)))

(defun read-object-as-hash-table (stream)
  (let ((table (make-hash-table :test 'equalp)))
    (loop until (char= (peek-char t stream) #\})
          do (setf (gethash (read stream) table)
                   (progn
                     (peek-char #\: stream)
                     (read-char stream)
                     (peek-char t stream)
                     (read stream)))
          when (char= (peek-char t stream) #\,)
          do (read-char stream))
    table))

(defclass json-map ()
  ((table :initform (make-hash-table :test 'equalp)
          :accessor json-map-table)))

(defun read-object-as-clos-instance (stream)
  (let* ((object (make-instance 'json-map))
         (table (json-map-table object)))
    (loop until (char= (peek-char t stream) #\})
          do (setf (gethash (read stream) table)
                   (progn
                     (peek-char #\: stream)
                     (read-char stream)
                     (peek-char t stream)
                     (read stream)))
          when (char= (peek-char t stream) #\,)
          do (read-char stream))
    object))

(defmethod print-object ((object json-map) stream)
  (write-char #\{ stream)
  (let ((first-p t))
    (with-slots (table) object
      (maphash (lambda (key value)
                 (if first-p
                     (setf first-p (not first-p))
                   (write-string " , " stream))
                 (write key :stream stream)
                 (write-char #\space stream)
                 (write-char #\: stream)
                 (write-char #\space stream)
                 (write value :stream stream))
             table)))
  (write-char #\} stream))

(defun json-object-reader (stream first-char)
  "This function implements a reader for JSON objects. It should be
used with the Common Lisp reader as a macro character function."
  (declare (ignore first-char))
  (with-json-string-reader
    (prog1
        (ecase *json-read-objects-as-type*
          (:list       (read-object-as-list stream))
          (:hash-table (read-object-as-hash-table stream))
          (:clos       (read-object-as-clos-instance stream)))
      (read-char stream))))

(defun make-json-readtable (&optional (readtable (copy-readtable nil)))
  "Creates a readtable with added functionality to
read JSON datastructures (array, object, string).
If the readtable is supplied, it is modified."
  (loop for (char fn) in '((#\[ json-array-reader)
                           (#\{ json-object-reader))
        do (set-macro-character char fn nil readtable))
  (loop for (to from) in '((#\] #\))
                           (#\} #\)))
        do (set-syntax-from-char to from readtable))
  readtable)

(defparameter *json-readtable*
  (make-json-readtable)
  "A readtable which parses JSON expressions.")

(defun json-read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursivep)
  "Reads a JSON expression from stream. Uses the *json-readtable*."
  (let ((*readtable* (or *json-readtable* (make-json-readtable)))
        (*read-base* 10))
    (read stream eof-error-p eof-value recursivep)))

(defun json-reader (&optional (on t))
  "Modifies the current readtable to parse JSON expressions.
Uses the characters {, }, [ and ]."
  (if on
      (make-json-readtable *readtable*)
    (let ((readtable *readtable*)
          (orig-readtable (copy-readtable nil)))
      (loop for char in '(#\[ #\{)
            do (set-macro-character char (get-macro-character char orig-readtable) nil readtable))
      (loop for char in '(#\] #\})
            do (set-syntax-from-char char char readtable orig-readtable))
      readtable)))



;;; Examples

#||

(defun test ()
  (let ((strings '("12"
                   "123"
                   "1e4"
                   "\"hi\\bho\\rha\""
                   "[1,2,3]"
                   "[true,false,null]"
                   "[ true , false , null ]"
                   "\"-\\u01ae-\""
                   "[[2,3],[4,5,6]]"
                   "{\"a\":10,\"b\":\"b1\"}"
                   "{\"a\":[1,2,\"foo\"],\"b\":\"b1\"}")))
    (loop for string in strings
          collect (list string (with-input-from-string (stream string)
                                 (json-read stream))))))

(defun test-examples (&optional (file "/Users/joswig/Desktop/json-examples.json"))
  (with-open-file (stream file)
    (loop for ex = (json-read stream nil)
          while ex
          do (pprint ex)
          do (terpri))))

||#


#+ignore
(defun simple-print-json-array (*standard-output* sequence)
  (pprint-logical-block (*standard-output* sequence :prefix "[" :suffix "]")
    (pprint-newline :mandatory) 
    (loop for item in sequence do
          (princ item)
          (pprint-newline :miser))
    (pprint-newline :mandatory)))



;;; End of File
