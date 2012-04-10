;;; Lisp 1 vs. Common Lisp

;;; Rainer Joswig, joswig@lisp.de, 2012

#|

; THE LISP 1 PROGRAMMER'S MANUAL FROM 1960, PAGE 99FF:
; http://bitsavers.org/pdf/mit/rle_lisp/LISP_I_Programmers_Manual_Mar60.pdf

; Lisp 1 used DEFINE to define functions. The comma character was whitespace
; between the list elements.

DEFINE
(((COLLAPSE,(LAMBDA,(L),(COND,
   ((ATOM,L),(CONS,L,NIL))
   ((NULL,(CDR,L)),
     (COND,((ATOM,(CAR,L)),L),(T,(COLLAPSE,(CAR,L)))))
   (T,(APPEND,(COLLAPSE,(CAR,L)),(COLLAPSE,(CDR,L)))))
))))))

|#


; THE SAME, JUST REFORMATTED, IN COMMON LISP:

(DEFUN COLLAPSE (L)
  (COND 
   ((ATOM L) (CONS L NIL))
   ((NULL (CDR L))
    (COND ((ATOM (CAR L)) L)
          (T (COLLAPSE (CAR L)))))
   (T (APPEND (COLLAPSE (CAR L))
              (COLLAPSE (CDR L))))))

#|

CL-USER > (COLLAPSE '(((A B) ((C))) ((D (E F)) (G) ((H)))))
(A B C D E F G H)

|#