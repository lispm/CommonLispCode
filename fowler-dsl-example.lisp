;;; -*- Mode: LISP; Package: "COMMON-LISP-USER"; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;; Copyright Rainer Joswig, joswig@lisp.de, 2012

;;; ================================================================
;;; short

(LET ((S "SVCLFOWLER         10101MS0120050313.........................
SVCLHOHPE          10201DX0320050315........................
SVCLTWO           X10301MRP220050329..............................
USGE10301TWO          X50214..7050329") (M '((SVCL (NAME 4 19) (ID 19 24) (CALL-TYPE-CODE 24 28) (DATE-OF-CALL 28 36)) (USGE (ID 4 9) (NAME 9 23) (CYCLE 30 31) (READ-DATE 31 36))))) (MAPCAR (LAMBDA (L &AUX (N (SUBSEQ L 0 4))) (CONS N (MAPCAR (LAMBDA (A) (LIST (CAR A) (SUBSEQ L (CADR A) (CADDR A)))) (CDR (ASSOC N M :TEST #'STRING=))))) (SPLIT-SEQUENCE '(#\NEWLINE) S)))


; or

(let ((text "SVCLFOWLER         10101MS0120050313.........................
SVCLHOHPE          10201DX0320050315........................
SVCLTWO           X10301MRP220050329..............................
USGE10301TWO          X50214..7050329")
      (mappings '((svcl (name 4 19) (id 19 24) (call-type-code 24 28) (date-of-call 28 36))
                  (usge (id 4 9) (name 9 23) (cycle 30 31) (read-date 31 36)))))
  (mapcar (lambda (line &aux (name (subseq line 0 4)))
            (cons name (mapcar (lambda (fields)
                                 (list (car fields) (subseq line (cadr fields) (caddr fields))))
                               (cdr (assoc name mappings :test #'string=)))))
          (split-sequence '(#\newline) text)))


;;; ================================================================
;;; different lengths keys

(defparameter *example-text*
  "SVCLFOWLER         10101MS0120050313.........................
SVCLHOHPE          10201DX0320050315........................
SVCLTWO           X10301MRP220050329..............................
USGE10301TWO          X50214..7050329")

(defparameter *mappings*
  '(("SVC"  (name            4 19)
            (id             19 24)
            (call-type-code 24 28)
            (date-of-call   28 36))
    ("USGE" (id              4  9)
            (name            9 23)
            (cycle          30 31)
            (read-date      31 36))))

(defun find-mapping (line mappings)
  (find-if (lambda (item)
             (string-equal line (first item) :end1 (length (first item))))
           mappings))

(defun parse-log-lines-example (text mappings)
  (mapcar (lambda (line)
            (destructuring-bind (name . fields)
                (find-mapping line mappings)
              (cons name (mapcar (lambda (field)
                                   (list (car field) (subseq line (cadr field) (caddr field))))
                                 fields))))
          (split-sequence '(#\newline) text)))

;;; ================================================================
;;; Using a literal hash-table

(defparameter *example-text*
  "SVCLFOWLER         10101MS0120050313.........................
SVCLHOHPE          10201DX0320050315........................
SVCLTWO           X10301MRP220050329..............................
USGE10301TWO          X50214..7050329")

(defparameter *mappings*
  '#3{("svcl" ((name            4 19)
               (id             19 24)
               (call-type-code 24 28)
               (date-of-call   28 36)))
      ("usge" ((id              4  9)
               (name            9 23)
               (cycle          30 31)
               (read-date      31 36)))})

(defun parse-log-lines-example (text mappings)
  (mapcar (lambda (line &aux (name (subseq line 0 4)))
            (cons name
                  (mapcar (lambda (fields)
                            (list (car fields)
                                  (subseq line (cadr fields) (caddr fields))))
                          (gethash name mappings))))
          (split-sequence '(#\newline) text)))

