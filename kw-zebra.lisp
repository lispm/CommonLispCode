
;;; Zebra Problem in LispWorks' KnowledgeWorks Prolog

;; adapted from Peter Norvig's book PAIP by Rainer Joswig, 2012, joswig@lisp.de

(require "prolog")

(in-package "CP-USER")

;;;; Section 11.4 (The Zebra Problem)

(clog:defrel nextto
             ((nextto ?x ?y ?list) (iright ?x ?y ?list))
             ((nextto ?x ?y ?list) (iright ?y ?x ?list)))

(clog:defrel iright
             ((iright ?left ?right (?left ?right . ?rest)))
             ((iright ?left ?right (?x . ?rest))
              (iright ?left ?right ?rest)))

;(clog:defrel gleich
;             ((gleich ?x ?x)))


(clog:defrel
 zebra
 ((zebra ?h ?w ?z)
  ;; Each house is of the form:
  ;; (house nationality pet cigarette drink house-color)
  (= ?h ((house norwegian ? ? ? ?)               ;1,10
         ? 
         (house ? ? ? milk ?) ? ?))              ; 9
  (member (house englishman ? ? ? red) ?h)       ; 2
  (member (house spaniard dog ? ? ?) ?h)         ; 3
  (member (house ? ? ? coffee green) ?h)         ; 4
  (member (house ukrainian ? ? tea ?) ?h)         ; 5
  (iright (house ? ? ? ? ivory)                  ; 6
          (house ? ? ? ? green) ?h)
  (member (house ? snails winston ? ?) ?h)       ; 7
  (member (house ? ? kools ? yellow) ?h)         ; 8
  (nextto (house ? ? chesterfield ? ?)           ;11
          (house ? fox ? ? ?) ?h)
  (nextto (house ? ? kools ? ?)                  ;12
          (house ? horse ? ? ?) ?h)
  (member (house ? ? luckystrike oj ?) ?h)       ;13
  (member (house japanese ? parliaments ? ?) ?h) ;14
  (nextto (house norwegian ? ? ? ?)              ;15
          (house ? ? ? ? blue) ?h)
  (member (house ?w ? ? water ?) ?h)             ;Q1
  (member (house ?z zebra ? ? ?) ?h)))            ;Q2


; (logic '(zebra ?houses ?water-drinker ?zebra-owner) :return-type :alist)
; or call the Prolog REPL with (rqp)
