;;; Some extensions to Common Lisp, mostly written by Rainer Joswig
;;; useful for providing solutions to questions on comp.lang.lisp

;;; Code by me (other authors are noted) is licensed as Public Domain
;;; and has Copyright Rainer Joswig, joswig@lisp.de, 2011 , 2012

;;; Runs in LispWorks 6.1


;;; ================================================================
;;; Some read macros


;; reading a list with angle brackets

#+ignore
(defun |[-reader| (stream char) 
   (declare (ignore char)) 
   (read-delimited-list #\] stream t)) 
#+ignore
(progn
  (set-macro-character #\[ #'|[-reader|) 
  (set-macro-character #\] (get-macro-character #\) nil)))

;; [a b] creates a list of numbers from a below b

#+ignore
(defun |[-reader| (stream char) 
   (declare (ignore char)) 
   (let ((list (read-delimited-list #\] stream t)))
     `(iota ,(- (second list) (first list)) ,(first list))))

#+ignore
(progn
  (set-macro-character #\[ #'|[-reader|) 
  (set-macro-character #\] (get-macro-character #\) nil)))


;;; ================================================================
;;; Control structures

(defmacro llet (name vars &body body)
  "Recursive LET like in Clojure. Name is the name for the loop call.
Vars is a list of variables for this loop, with initial bindings."
  (labels ((generate-list-of-var-syms (vars)
             (loop for var in vars
                   collect (if (consp var)
                               (first var)
                             var))))
    (let ((start-tag (gensym))
          (list-of-var-syms (generate-list-of-var-syms vars)))
      `(prog ,vars
         ,start-tag
         (macrolet ((,name (&rest args)
                      `(progn
                         (setf ,@(mapcan #'list ',list-of-var-syms args))
                         (go ,',start-tag))))
           (locally ,@body))))))

#|

(llet recur ((list '(1))) 
  (when (< (length list) 17)
    (format t "~{~6D~^,~}~%" list)
    (recur (mapcar '+ (cons 0 list) (append list '(0))))))

(defun foo (n)
  (llet recur ((x n))
    (print x)
    (unless (zerop x)
      (recur (1- x)))))

(defun foo (n)
  (llet recur ((x n) (y (+ n 10)))
    (print (list x y))
    (unless (zerop x)
      (recur (1- x) (1+ y)))))

|#

; Pascal Costanza
(defmacro pipe (&body forms) 
   (let ((var (gensym))) 
     `(macrolet ((=> (&body forms) 
                   `(let ((,',var (funcall #',(car forms) ,',var))) 
                      ,(if (cdr forms) (cdr forms) ',var)))) 
        (let ((,var ,(car forms))) 
          ,(if (cdr forms) (cdr forms) var))))) 


(defun curry (f &rest args)
  (lambda (&rest x) (apply f (append args x))))

(defun rcurry (f &rest last-args)
  (lambda (&rest x) (apply f (append x last-args))))


;;; Paul Graham, On Lisp, p191
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))


;;; ================================================================
;;; List handling

;;; Take, like in Mathematica
(defun %take (it what)
  (cond ((eq what :all) it)
        ((eq what :none) nil)
        ((and (numberp what) (plusp what))
         (subseq it 0 what))
        ((and (numberp what) (minusp what))
         (last it (- what)))
        ((and (consp what)
              (= (length what) 1)
              (numberp (first what)))
         (nth (first what) it))
        ((and (consp what)
              (= (length what) 2)
              (numberp (first what))              
              (numberp (second what)))
         (let ((end (if (minusp (second what))
                        (+ (length it) (second what))
                      (second what))))
           (subseq it (first what) end)))
        ((and (consp what)
              (= (length what) 3)
              (numberp (first what))
              (numberp (second what))
              (numberp (third what)))
         (let ((start (first what))
               (end (if (minusp (second what))
                        (+ (length it) (second what))
                      (second what)))
               (by-step (third what)))
           (loop for e = (subseq it start) then (nthcdr by-step e)
                 for i from start below end by by-step
                 collect (first e))))))

(defun take (thing &rest description)
  "Taking things from lists like in Mathematica
Description is one or more of:
   :all | :none | [sign]number | ( start [end [step]])"
  (cond ((null description) nil)
        ((and (consp description)
              (= (length description) 1))
         (%take thing (first description)))
        (t (loop for e in (%take thing (first description))
                 collect (apply #'take e (rest description))))))

; (take '(1 2 3 4 5 6 7) '(2 5))
; (take '(1 2 3 4 5 6 7) 3)
; (take '((a b c d) (1 2 3 4) (5 6 7 8)) 2 2)
; (take '(a b c d) '(0 -1 2))
; (take '(1 2 3 4 5 6 7) -3)

(defun take-n (list n)
  (loop for e in list
        repeat n
        collect e))

(defun take-best-n (n list &key (key 'identity) (predicate '>))
  "Returns the best n items. Preserves order."
  (let ((taken (take-n (sort (copy-list list) predicate :key key) n)))
    (mapcan (lambda (e)
              (when (member e taken :test #'equal) (list e)))
            list)))

(defun iota* (n &optional (start 0))
  "Creates a list of n elements starting with element START.
START can be a number, a character, a string or a symbol."
  (etypecase start
    (number (loop for i from start
                  repeat n
                  collect i))
    (character (loop with c = (char-code (or (and (characterp start) start) #\a))
                     for i from 0
                     repeat n
                     collect (code-char (+ c i))))
    (string (loop with c = (char-code (or (and (stringp start) (aref start 0)) #\a))
                     for i from 0
                     repeat n
                     collect (string (code-char (+ c i)))))
    (symbol (loop with c = (char-code (or (and (symbolp start) (aref (string start) 0)) #\a))
                     for i from 0
                     repeat n
                     collect (intern (string (code-char (+ c i))) (symbol-package start))))))

(defun repeat (it n)
  "Returns a list of n elements of IT."
  (loop repeat n collect it))

(defun distribution (seq &key (test 'eql) (sort-pred #'<))
  (let ((table (make-hash-table :test test)) result)
    (map nil (lambda (x) (incf (gethash x table 0))) seq)
    (maphash (lambda (k v)
               (push (cons k v) result))
             table)
    (sort result sort-pred :key #'cdr)))

(defun flatten (list)
  (mapcan (lambda (item)
            (if (listp item) (flatten item) (list item)))
          list))

(defun max-by (l f)
  "Returns two values, the position and the maximum value (after applying the function F)"
  (values-list (car (sort (mapcar 'list l (mapcar f l)) '> :key 'second))) )

(defun circ (list)
  "Returns a circular list. The argument can be a list of elements or a single atom."
  (let ((l (or (and (atom list) (list list)) (copy-list list))))
    (setf (cdr (last l)) l)
    l))

;; Peter Norvig
(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mapcan #'(lambda (y)
              (mapcar #'(lambda (x) (funcall fn x y))
                      xlist))
           ylist))

(defun join (type seq1 seq2)
  "Creates a new sequence of the elements of seq1 with the seq2 spliced in between."
  (ecase type
    (string (with-output-to-string (s)
              (princ (first seq1) s)
              (loop for e in (rest seq1)
                    do (princ seq2 s) (princ e s))))))

(defun rot13 (text) 
  (let ((letters '#.(let* ((uc "ABCDEFGHIJKLMNOPQRSTUVWXYZ") 
                           (lc (string-downcase uc)))
                      (concatenate 'list uc uc lc lc))))
    (map 'string
         (lambda (c)
           (aif (member c letters)
                (nth 13 it)
                c))
         text)))

(defun mapn (f list n)
  (loop for l = list then (nthcdr n l)
        for l1 = (take-n l n)
        while (= (length l1) n)
        do (funcall f l1)))

#+ignore
(defun tuple-products (list n) 
  (mapn (lambda (group)
          (format t "~%~{~D~^ * ~} = ~a" group (reduce '* group)))
        list
        n))

(defun remove-every-nth (list n)
  (mapcan (lambda (a b) (when b (list a)))
          list
          (circ (append (iota (1- n)) '(())))))

(defun split-at (seq &optional (n (ceiling (length seq) 2)))
  (values (subseq seq 0 n) (subseq seq n)))

(defun split-half (list)
  (values (loop for nil in list by #'cddr collect (pop list))
          list))


;;; SRFI 1 - List Library

;; Constructors

(defun xcons (a d)
  (cons d a))

(defun cons* (&rest elements)
  (apply #'list* (first elements) (rest elements)))

(defun list-tabulate (n function)
  "Creates a list of values of applying function to i in 0..n-1"
  (loop for i below n collect (funcall function i)))

(defun circular-list (&rest elements)
  (circ elements))

(defun iota (n &optional (start 0) (step 1))
  "Creates a list of n elements starting with element START."
  (loop for i from start by step
        repeat n
        collect i))

(defun drop (list n)
  (subseq list n))

(defun take-right (list n)
  (last list n))

(defun drop-right (list n)
  (butlast list n))

(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

(defun unzip2 (list)
  (loop for (a b) in list
        collect a into a-list
        collect b into b-list
        finally (return (values a-list b-list))))

;;; ================================================================
;;; Fowler's example in plain Lisp

(defparameter *example-text*
  "SVCLFOWLER         10101MS0120050313.........................
SVCLHOHPE          10201DX0320050315........................
SVCLTWO           X10301MRP220050329..............................
USGE10301TWO          X50214..7050329")

(defparameter *mappings*
  '((svcl (name            4 19)
          (id             19 24)
          (call-type-code 24 28)
          (date-of-call   28 36))
    (usge (id              4  9)
          (name            9 23)
          (cycle          30 31)
          (read-date      31 36))))

(defun parse-log-lines-example (text mappings)
  (mapcar (lambda (line &aux (name (subseq line 0 4)))
            (cons name (mapcar (lambda (fields)
                                 (list (car fields) (subseq line (cadr fields) (caddr fields))))
                               (cdr (assoc name mappings :test #'string=)))))
          (split-sequence '(#\newline) text)))

;;; ================================================================
;;; End of File


