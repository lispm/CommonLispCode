;*****************************************************************
;  MICRO-TALESPIN: A STORY GENERATOR
;
;  A reconstruction, in Common Lisp, of James Meehan's program in
;  _Inside_Computer_Understanding:_Five_Programs_Plus_Miniatures_
;  Roger Schank and Christopher Riesbeck (eds.)
;
;  Warren Sack                 
;  MIT Media Lab
;  20 Ames Street, E15-486
;  Cambridge MA 02139
;  wsack@media.mit.edu
;
;  October 1992
;
;  I translated Micro-Talespin into Common Lisp as a
;  "literature review exercise":  I wanted to see and play
;  with storyteller systems that had been written in the past.
;  I am currently working on creating storyteller systems which
;  produce not only text (as Micro-Talespin does) but also
;  audio and video.  If you are working on a similar project
;  I'd love to hear from you.  I can be reached at the
;  above address.
;
;*****************************************************************


; Changes by Rainer Joswig, joswig@lisp.de, 2012:
;   fixed:  dprox-plan1, ptrans only if the actor knows the new loc
;   hacked: Output sentence starts with an uppercase letter and then lowercase letters follow.
; Changes by Rainer Joswig, joswig@lisp.de, 2008:
;   fixed:  persuade and not peruade
;   fixed:  three args to persuade
;   fixed:  changed implementation of PCVar from Structure to Symbol (symbol with leading ?)
;   fixed:  correct spaces in output
;   fixed:  put symbols instead of functions, makes tracing easier


;  Standard definition of put.
(defmacro put (x y z)
  `(setf (get ,x ,y) ,z))

;  Definition necessary for pattern variables.
(defun pcvar-p (item)
  (and (symbolp item)
       (> (length (symbol-name item)) 0)
       (char= (aref (symbol-name item) 0) #\?)))

;  Definition of Globals

(defvar *personae*)
(defvar *goals*)
(defvar *all-locations*)
(defvar *all-objects*)


(defvar *init-facts* nil
  "This is the initial data base.  It can be extended before 
running a story.")

(defun init-facts ()
  (setf *init-facts*
        '((world  (loc (actor joe)    (val cave)))
          (joe    (loc (actor joe)    (val cave)))
          (world  (loc (actor irving) (val oak-tree)))
          (irving (loc (actor irving) (val oak-tree)))
          (joe    (loc (actor irving) (val oak-tree)))
          (world  (loc (actor water)  (val river)))
          (joe    (loc (actor water)  (val river)))
          (world  (loc (actor honey)  (val elm-tree)))
          (irving (loc (actor honey)  (val elm-tree)))
          (world  (loc (actor worm)   (val ground)))
          (joe    (loc (actor worm)   (val ground)))
          (irving (loc (actor joe)    (val cave)))
          (world  (loc (actor fish)   (val river)))
          (irving (loc (actor fish)   (val river))))))


(defun init-world ()
  "init-world sets up a bunch of facts such as Joe is a bear, birds
eat worms, and so on.  The variable *init-facts* contains location
and relationship facts, along with which character knows them."
  (put 'joe  'is-a 'bear)
  (put 'joe 'home 'cave)
  (put 'irving 'is-a 'bird)
  (put 'irving 'home 'tree)
  (put 'bear 'food '(honey berries fish))
  (put 'bird 'food '(worm))
  (setf *personae* '(joe irving))
  (setf *goals* '(hungry thirsty))
  (setf *all-locations* '(cave oak-tree elm-tree ground river))
  (setf *all-objects* (append *all-locations* 
                              '(honey berries fish worm water)))
  (mapc #'(lambda (persona)
            (put persona 'facts nil)
            (put persona 'goals nil)
            (put persona 'demons nil))
        (cons 'world *personae*))
  (mapc #'(lambda (fact)
            (now-knows (car fact) (cadr fact) t))
        *init-facts*))

(defun ask-plan (actor agent action)
  "The success of asking something depends upon whether the other person
is honest and likes you."
  `(and (not (relate ',actor ',agent ',actor 'deceive))
        (relate ',actor ',actor ',agent 'like)
        (tell ',actor ',agent (question ',action))
        ;(is-true ',result)
        ))

(defun bargain-plan (actor agent action)
  "The success of bargaining with someone by giving them food depends
on whether the other person is honest, you don't already have the
goal of getting the food you're going to bargain with, and you can
get the food to the other person."
  (let ((atrans-food (atrans actor 'food agent actor)))
    `(and (not (relate ',actor ',agent ',actor 'deceive))
          (not (knows ',actor (has ',agent 'food)))
          (not (has-goal-of ',actor (has ',actor 'food)))
          (doit (mbuild ',actor (cause ',atrans-food (maybe ',action))))
          (tell ',actor 
                ',agent 
                (question (cause ',atrans-food (future ',action))))
          (dcont ',actor 'food)
          (dprox ',actor ',actor ',agent)
          (doit ',atrans-food)
          (is-true ',action))))


(defun threat-plan (actor agent action)
  "The success of threatening depends upon whether you dominate
the other person."
  `(and (not (relate ',actor ',agent ',actor 'dominate))
        (tell ',actor 
              ',agent 
              (cause (negate ',action) (future (propel ',actor 'hand ',agent))))
        (or (is-true ',action)
            (and (doit (propel ',actor 'hand ',agent))
                 (is-true ',action)))))

(defvar *default-tense* 'past  ; or present
  "Set the storytelling in the past tense.")

;  micro-talespin-demo variables for sample stories

;  No plot: joe gets a drink of water.
(defvar *story1*
  '(joe thirsty))

(defvar *story2*
  '(irving thirsty
    (irving (like (actor joe) (to irving) (mode (neg))))
    (irving (dominate (actor joe) (to irving) (mode (neg))))
    (irving (deceive (actor joe) (to irving) (mode (pos))))
    (irving (like (actor irving) (to joe) (mode (neg))))
    (joe (deceive (actor irving) (to joe) (mode (neg)))))
  "irving kills joe.")

(defvar *story3*
  '(joe hungry
    (joe (like (actor irving) (to joe) (mode (neg))))
    (joe (dominate (actor irving) (to joe) (mode (pos)))))
  "joe is frustrated at every turn.")

(defvar *story4*
  '(joe hungry
    (world (hungry (actor irving) (mode (pos))))
    (joe (like (actor irving) (to joe) (mode (pos))))
    (joe (deceive (actor irving) (to joe) (mode (neg))))
    (joe (like (actor joe) (to irving) (mode (pos))))
    (irving (like (actor irving) (to joe) (mode (pos))))
    (irving (dominate (actor irving) (to joe) (mode (neg))))
    (irving (deceive (actor irving) (to joe) (mode (neg)))))
  "joe and irving strike a deal.")

(defvar *story5*
  '(irving thirsty
    (irving (like (actor irving) (to joe) (mode (pos))))
    (irving (like (actor joe) (to irving) (mode (pos))))
    (irving (deceive (actor joe) (to irving) (mode (neg))))
    (irving (dominate (actor joe) (to irving) (mode (pos))))
    (world (hungry (actor joe) (mode (pos))))
    (joe (like (actor joe) (to irving) (mode (neg))))
    (joe (deceive (actor joe) (to irving) (mode (pos)))))
  "joe tricks irving")

(defvar *story6*
  '(joe hungry
    (joe (like (actor irving) (to joe) (mode (pos))))
    (joe (dominate (actor irving) (to joe) (mode (neg)))))
  "This is an interactive version of *story4* and/or *story5*")

;  Declare globals used in forward-chaining through goals and plans.
(defvar *actions*)
(defvar *plans*)
(defvar *conseqs*)

(defun micro-talespin ()
  (init-facts)
  (init-world)
  (let ((main-character (pick-one 'character *personae*))
        (problem (pick-one 'problem *goals*)))
     (format t "~%Once upon a time ...")
;     (init-world)
     (format t "~%One day,")
     (assert-fact (mloc 'world (state main-character problem 'pos)))
     (format t "~%The end.")))

(defun micro-talespin-demo (story)
  "micro-talespin-demo lets you predefine more facts for a story.
story should be a list of the form (character problem fact fact ...)
where
   character is either joe or irving,
   problem is either hunger or thirst,
   facts have the for (character 'CD-form).  The character field
         says who knows this fact."
  (init-facts)
  (setf *init-facts* 
        (append *init-facts* (cddr story)))
  (let ((main-character (car story))
        (problem (cadr story)))
    (format t "~%Once upon a time ...")
    (init-world)
    (format t "~%One day, ")
    (assert-fact (mloc 'world (state main-character problem 'pos)))
    (format t "~%The end.")))

(defun pick-one (name l)
  "pick-one is used to get the character and problem from the terminal."
  (format t "~%Choose a ~s from this list:~%~s~%> " name l)
  (let ((a (read)))
    (if (member a l) a (pick-one name l))))

(defun goal-eval (actor goal plans)
"goal evaluator: executes each plan until one works and the goal
can be removed, or until none do and the character fails to get the
goal.  If the goal is already true (and the actor knows that), then
return success immediately.  If the actor already has the goal,
then he's in a loop and has failed.  Otherwise, set up the goal and go."
  (cond ((knows actor goal)
         t)
        ((has-goal-of actor goal)
         nil)
        (t
         (gets-new-goal-of actor goal)
         (cond ((run-plans plans)
                (forgets-goal-of actor goal)
                t)
               (t
                (now-knows actor (negate (future goal)) t)
                nil)))))

#+ignore
(defun run-plans (plans)
  (let ((plan (car plans)))
    (if plan
       (if (eval plan)
          t
          (run-plans (cdr plans))))))

(defun run-plans (plans)
  (loop for plan in plans
        when (eval plan)
        do (return t)))

;  gen-plans replicates the same plan with different objects
;  e.g., trying to get any one of the several foods with the
;  same bargaining plan.
(defun gen-plans (var possibilities plan-form)
  (mapcar #'(lambda (possibility)
             (subst possibility var plan-form))
        possibilities))

;  Two S-goals -- thirst and hunger:

;  To satisfy thirst, go to some water and drink it.
(defun sthirst (actor)
  (goal-eval actor 
             (state actor 'thirsty 'neg)
             (list (sthirst-plan actor))))

(defun sthirst-plan (actor)
  `(and (dprox ',actor ',actor 'water)
        (doit (ingest ',actor 'water))))

;  To satisfy hunger, get some food and eat it.
(defun shunger (actor)
  (goal-eval actor 
             (state actor 'hungry 'neg)
             (gen-plans 'food
                        (get-isa 'food actor)
                        (shunger-plan actor))))

(defun shunger-plan (actor)
  `(and (dcont ',actor 'food)
        (doit (ingest ',actor 'food))))

;  Three D-goals -- dcont, dknow, dprox:

;  To get an object: if you know someone has it, persuade them to
;  give it to you; otherwise try to find out where the object is,
;  go there and take it.
(defun dcont (actor object)
  (let ((owner (knows-owner actor object)))
    (goal-eval actor 
               (has actor object)
               (if owner 
                 (list (dcont-plan1 actor object owner))
                 (list (dcont-plan2 actor object))))))

(defun dcont-plan1 (actor object owner)
  `(persuade ',actor 
             ',owner 
             (atrans ',owner ',object ',actor ',owner)))

(defun dcont-plan2 (actor object)
  `(and (dknow ',actor (where-is ',object))
        (dprox ',actor ',actor ',object)
        (doit (atrans ',actor ',object ',actor nil))))

;  To find out something: find a friend to tell you
(defun dknow (actor info)
  (goal-eval actor
             (mloc actor info)
             (gen-plans 'agent
                        (remove actor *personae*)
                        (dknow-plan actor info))))

(defun dknow-plan (actor info)
  `(and (knows-loc ',actor 'agent)
        (or (is-friend-of 'agent ',actor)
            (not (relate ',actor 'agent ',actor 'dominate)))
        (persuade ',actor
                  'agent
                  (mtrans 'agent ',info ',actor 'agent)))) 

;  To move an object (including yourself) to where some other
;  person or object is: get the first object (if not yourself), then
;  find out where the second object is and go there with the first
;  object.  If this doesn't work, try persuading the object to go
;  there itself.
(defun dprox (actor object new-object)
  (goal-eval actor 
             (is-at object new-object)
             (list (dprox-plan1 actor object new-object)
                   (dprox-plan2 actor object new-object))))

(defun dprox-plan1 (actor object new-object)
  `(and (or (equal ',actor ',object)
            (dprox ',actor ',actor ',object))
        (dknow ',actor (where-is ',new-object))
        (or (equal ',actor ',object)
            (doit (grasp ',actor ',object)))
        (or (is-prox ',actor (loc-name-of ',new-object))
            (and (knows-loc ',actor ',new-object)
                 (doit (ptrans ',actor
                               ',object
                               (knows-loc ',actor ',new-object) 
                               (knows-loc ',actor ',actor)))))
        (or (equal ',actor ',object)
            (doit (un-grasp ',actor ',object)))))

(defun dprox-plan2 (actor object new-object)
  `(and (not (equal ',actor ',object))
        (member ',object *personae*)
        (persuade ',actor
                 ',object
                 (ptrans ',object
                         ',object
                         ',new-object
                         (loc-name-of ',object)))))

;  Subgoals and plans -- persuade, ask, bargain, threaten, and tell:

;  You can persuade someone to do something by either asking them,
;  giving them food or threatening them.
(defun persuade (actor agent action)
  (goal-eval actor 
             action
             (append (gen-plans 'food 
                                (get-isa 'food agent) 
                                (bargain-plan actor agent action))
                     (list (ask-plan actor agent action))
                     (list (threat-plan actor agent action)))))

;  To tell someone something, go there and say it.
(defun tell (actor agent info)
  (goal-eval actor 
             (mloc agent info)
             (list (tell-plan actor agent info))))

(defun tell-plan (actor agent info)
  `(and (dprox ',actor ',actor ',agent)
        (doit (mtrans ',actor ',info ',agent ',actor))))

;  The simulator

;  doit adds a CD and its consequences to the data base, by calling
;  assert-fact.  mtranses with '?unspecified have to be filled out, as in
;  "Irving told Joe where the honey was" -- the "where" being represented
;  in the CD with an '?unspecified form.
(defun doit (cd)
  (let ((newcd 
         (if (and (equal (header-cd cd) 'mtrans)
                  (knows (cdpath '(actor) cd) 
                         (cdpath '(object) cd)))
           (setrole 'object 
                    (knows (cdpath '(actor) cd) 
                           (cdpath '(object) cd)) 
                    cd)
           cd)))
    (assert-fact newcd)
    newcd))

;  assert-fact is one of the central control functions.  It starts with
;  one fact, infers the consequences, infers the consequences of the
;  consequences, etc.  Besides the simple result put in *conseqs*
;  (e.g., ptrans changes locs), new states may lead to response actions
;  (put in *actions*) or new plans (put in *plans*).  The plans are
;  done after all the consequences are inferred.
(defun assert-fact (x)
  (setf *actions* nil)
  (setf *plans* nil)
  (forward-chain (list x))
  (mapc #'(lambda (cd) (doit (setrole 'time *default-tense* cd)))
        *actions*)
  (mapc #'eval *plans*))

(defun forward-chain (l)
  (setf *conseqs* nil)
  (mapc #'(lambda (i) 
            (now-knows 'world i nil)
            (conseqs i))
        l)
  (if *conseqs* (forward-chain *conseqs*)))

;  Each act and state is associated with a function for 
;  calculating the consequences.
(defun conseqs (cd)
  (case (header-cd cd)
    (atrans (atrans-conseqs cd))
    (grasp (grasp-conseqs cd))
    (ingest (ingest-conseqs cd))
    (loc (loc-conseqs cd))
    (mbuild (mbuild-conseqs cd))
    (mloc (mloc-conseqs cd))
    (mtrans (mtrans-conseqs cd))
    (plan (plan-conseqs cd))
    (propel (propel-conseqs cd))
    (ptrans (ptrans-conseqs cd))
    (t nil)))
 
;  add-conseq adds and returns a CD to the list of consequences
(defun add-conseq (x)
  (push x *conseqs*)
  x)

;  Consequences of an atrans: everyone in the area notices it and the
;  resulting change of possesion, the object changes locations, and the
;  from filler knows he no longer has it.
(defun atrans-conseqs (cd)
  (notice (cdpath '(actor) cd) 
          cd)
  (notice (cdpath '(actor) cd) 
          (add-conseq (has (cdpath '(to) cd) 
                           (cdpath '(object) cd))))
  (add-conseq (is-at (cdpath '(object) cd)
                     (cdpath '(to) cd)))
  (if (cdpath '(from) cd)
      (notice (cdpath '(actor) cd)
              (add-conseq (negate (has (cdpath '(from) cd)
                                       (cdpath '(object) cd)))))))

;  Consequences of a grasp: everyone knows that the actor either has or
;  (in the case of a tf (transition final or the end of an action) of the
;  grasp)  doesn't have the object
(defun grasp-conseqs (cd)
  (notice (cdpath '(actor) cd)
          (add-conseq (if (in-mode cd 'tf)
                        (negate (has (cdpath '(actor) cd)
                                     (cdpath '(object) cd)))
                        (has (cdpath '(actor) cd)
                             (cdpath '(object) cd))))))

;  Consequences of an ingest: everyone knows that the actor 
;  is no longer hungry or thirsty.
(defun ingest-conseqs (cd)
  (notice (cdpath '(actor) cd)
          (add-conseq (state (cdpath '(actor) cd)
                             (if (equal (cdpath '(object) cd) 'water)
                               'thirsty
                               'hungry)
                             'neg))))

;  Consequences of a loc change: everyone knows it.
(defun loc-conseqs (cd)
  (notice (cdpath '(actor) cd) cd))

;  Consequences of an mbuild: if the object is a causal then a demon
;  is set up for the actor that will be triggered by the antecedent.
(defun mbuild-conseqs (cd)
  (if (equal (cdpath '(actor) cd)
             (cdpath '(object conseq actor) cd))
    (put (cdpath '(actor) cd)
         'demons
         (cons (cons (cdpath '(object ante) cd)
                     (cdpath '(object conseq) cd))
               (get (cdpath '(actor) cd) 'demons))))
  nil)

;  Consequences of an mloc change: check the demons to see if the
;  learned fact affects the learner.  Also check the reaction list
;  for general responses to learning such facts.
(defun mloc-conseqs (cd)
  (demon-check (cdpath '(val part) cd)
               (cdpath '(con) cd))
  (if (not (member 'neg (cdpath '(con mode) cd)))
    (case (header-cd (cdpath '(con) cd))
      (loc (loc-react cd))
      (mloc (mloc-react cd))
      (hungry (hunger-react cd))
      (thirsty (thirst-react cd))
      (t nil))))

;  Stored under each character is a list of "demons."  A demon is
;  a CD pattern plus an action.  Whenever the character learns
;  something this list is checked to see if there is a response to make.
;  Demons are set up by things like the mbuild in a bargain-plan.
(defun demon-check (who event)
  (put who
       'demons
       (remove-if #'null
                  (mapc #'(lambda (demon)
                            (cond ((unify-cds (car demon) event)
                                   (push (cdr demon) *actions*)
                                   nil)
                                  (t
                                   demon)))
                        (get who 'demons)))))

;  Consequences of an mtrans: if there is a ques in the CD mtransed,
;  and if it is a causal, then it is a bargaining promise; otherwise,
;  it is a request (assuming the actors in the sub-CD are in the right
;  places).  If there is no ques in the CD mtransed, then the hearer
;  knows about the mtrans, and if he believes the speaker, then he
;  believes what the speaker believes.
(defun mtrans-conseqs (cd)
  (let ((actor (cdpath '(actor) cd))
        (object (cdpath '(object) cd))
        (hearer (cdpath '(to part) cd)))
    (cond ((member 'ques (cdpath '(object mode) cd))
           (cond ((and (equal (header-cd object) 'cause)
                       (equal actor (cdpath '(object ante actor) cd))
                       (equal hearer (cdpath '(object conseq actor) cd)))
                  (promise-conseqs hearer
                                   (cdpath '(object conseq) cd)
                                   actor
                                   (cdpath '(object ante) cd)))
                 ((equal (cdpath '(object actor) cd) hearer)
                  (request-conseqs actor
                                   hearer
                                   (future (un-question object))))))
          ((not (equal actor hearer))
           (add-conseq (mloc hearer cd))
           (cond ((not (relate hearer actor hearer 'deceive))
                  (add-conseq (mloc hearer (mloc actor object)))))))))

;  Consequences of y asking x to promise to do xdo if y does ydo:
;  If x deceives y, then after ydo, x will call y stupid, but says
;  that he will do xdo in return for ydo;
;  else if x likes y, then x will do xdo after ydo and says so.
;  Otherwise x says no.
(defun promise-conseqs (x xdo y ydo)
  (let ((a (cause ydo (affirm xdo))))
    (cond ((relate x x y 'deceive)
           (add-conseq (mbuild x
                               (cause ydo
                                      (future (mtrans x
                                                      (state y 'smart 'neg)
                                                      y
                                                      x)))))
           (add-conseq (mtrans x a y x)))
          ((relate x x y 'like)
           (add-conseq (mbuild x a))
           (add-conseq (mtrans x a y x)))
          (t
           (add-conseq (mtrans x (negate a) y x))))))

;  Consequences of x asking y to do z: 
;  If y doesn't like x or dominates x, then y will say no; otherwise
;  y will do z.
(defun request-conseqs (x y z)
  (add-conseq (if (or (not (relate y y x 'like))
                      (relate y y x 'dominate))
                (plan y (future (mtrans y (negate z) x y)))
                (plan y z))))

;  Consequences of a plan: If the actor of the plan act is the actor of 
;  the object of the plan, then add the object to the list of actions.
(defun plan-conseqs (cd)
  (if (equal (cdpath '(actor) cd) (cdpath '(object actor) cd))
    (push (cdpath '(object) cd) *actions*))
  nil)


;  Consequences of a propel: the object struck dies
(defun propel-conseqs (cd)
  (if (member (cdpath '(to) cd) *personae*)
    (add-conseq (state (cdpath '(to) cd) 'health 'neg))))

;  Consequences of a ptrans: location change, for both actor
;  and object.
(defun ptrans-conseqs (cd)
  (add-conseq (is-at (cdpath '(object) cd) (cdpath '(to) cd)))
  (if (not (equal (cdpath '(actor) cd) (cdpath '(object) cd)))
    (add-conseq (is-at (cdpath '(actor) cd) (cdpath '(to) cd)))))

;  Reactions to learning of a location change: if it's food or water,
;  check to see if learner is hungry or thirsty.
(defun loc-react (cd)
  (and (or (member (cdpath '(con actor) cd)
                   (get-isa 'food (cdpath '(val part) cd)))
           (equal (cdpath '(con actor) cd) 'water))
       (sgoal-check (cdpath '(val part) cd)
                    (if (equal (cdpath '(con actor) cd) 'water) 
                      'thirsty
                      'hungry))))

;  If a character is hungry or thirsty, add the appropriate s-goal
;  to the list of plans.
(defun sgoal-check (actor scale)
  (and (in-state actor scale)
       (push (list (if (equal scale 'thirsty)
                     'sthirst
                     'shunger)
                   (list 'quote actor))
             *plans*)))

;  Reactions to learning that someone has learned something:
;  if it's someone else, and it's about himself or you believe he
;  doesn't deceive you, then you believe it too.
(defun mloc-react (cd)
  (and (not (equal (cdpath '(val part) cd) (cdpath '(con val part) cd)))
       (or (equal (cdpath '(con con actor) cd) (cdpath '(con val part) cd))
           (not (relate (cdpath '(val part) cd) 
                        (cdpath '(con val part) cd)
                        (cdpath '(val part) cd)
                        'deceive)))
       (add-conseq (mloc (cdpath '(val part) cd)
                         (cdpath '(con con) cd)))))

;  Reactions to learning that you're hungry: add s-goal to list
;  of plans.
(defun hunger-react (cd)
  (push (list 'shunger (list 'quote (cdpath '(con actor) cd))) *plans*))

;  Reactions to learning you're thirsty: add s-goal to list 
;  of plans.
(defun thirst-react (cd)
  (push (list 'sthirst (list 'quote (cdpath '(con actor) cd))) *plans*))

;  Notice says that everyone in the same location as who knows
;  about CD.
(defun notice (who cd)
  (let ((where (loc-name-of who)))
    (mapc #'(lambda (persona)
              (if (equal (loc persona) where)
                (add-conseq (mloc persona cd))))
          *personae*)))

;  Memory functions and pattern matcher
;  addfact adds a CD to knower's knowledge set.  Also if world
;  learns a character has died, then the character is removed from the
;  global list of characters.
;  The CD is added to the front of the fact list, so that memquery
;  will get the most recent CD that matches its query pattern.  Older
;  contradicted facts are still on the list but are not seen.
(defun addfact (knower cd)
  (put knower 'facts (cons cd (get knower 'facts)))
  ;;; Now check for deceased people.
  (if (and (equal knower 'world)
           (equal (header-cd cd) 'health)
           (member 'neg (cdpath '(mode) cd)))
    (setf *personae* 
          (remove (cdpath '(actor) cd)
                  *personae*)))
  nil)

;  is-state returns non-nil if CD is one of the state forms.
(defun is-state (cd)
  (member (header-cd cd)
          '(loc 
            mloc 
            cont 
            like 
            deceive 
            dominate 
            hungry 
            thristy 
            health 
            smart)))

;  now-knows adds what to the data base for who.  It also prints in
;  English this new fact.  If who = world (a true fact) and what is
;  an mloc, then save the content of the mloc under the person who
;  learned it.  If say-flag is t, then mlocs are always generated in
;  English; otherwise only facts (who = world) are generated.  This
;  reduces the volume of the output.
(defun now-knows (who what say-flag)
  (let ((newwho
         (if (and (equal who 'world) 
                  (equal (header-cd what) 'mloc))
           (cdpath '(val part) what)
           who))
        (newwhat
         (if (and (equal who 'world) 
                  (equal (header-cd what) 'mloc))
           (cdpath '(con) what)
           what)))
    (if (or say-flag 
            (equal newwho 'world))
      (say (mloc newwho newwhat)))
    (addfact newwho newwhat)))

;  knows(knower,fact) returns fact if fact is in data base for knower:
;  -- if fact = knows(knower,subfact), assume everyone knows what they
;     know and look up subfact,
;  -- if fact has a ?unspec, then return the filler that replaces
;    the ?unspec in the data base.
(defun knows (knower fact)
  (let ((newfact
         (if (and (equal (header-cd fact) 'mloc)
                  (equal (cdpath '(val part) fact) knower))
           (cdpath '(con) fact)
           fact)))
  (memquery knower newfact)))

(defun knows-loc (knower object)
  (cdpath '(val) (knows knower (where-is object))))

(defun knows-owner (knower object)
  (cdpath '(val) (knows knower (who-has object))))

(defun knows-if (knower cd)
  (cdpath '(mode) (knows knower (setrole 'mode '?unspecified cd))))

;  memquery find the first item in knower's data base that
;  matches fact.
(defun memquery (knower pat)
  (car (pat-member pat (get knower 'facts))))

;  pat-member finds the first item in cd-list that matches
;  pat and returns cd-list from that item on.
(defun pat-member (pat cd-list)
  (if cd-list
    (let ((cd (car cd-list)))
      (if (unify-cds pat cd)
        cd-list
        (pat-member pat (cdr cd-list))))))

;  Returns non-nil if actor has goal.
(defun has-goal-of (actor pat)
  (car (pat-member pat (get actor 'goals))))

;  Adds goal to data base.
(defun gets-new-goal-of (actor goal)
  (put actor 'goals (cons goal (get actor 'goals)))
  (say (wants actor goal)))

;  Removes goal from data base
(defun forgets-goal-of (actor goal)
  (let ((goal-to-be-forgotten (has-goal-of actor goal)))
    (put actor
         'goals
         (remove-if #'(lambda (g)
                        (equal g goal-to-be-forgotten))
                   (get actor 'goals)))))

;  Returns non-nil if x is in a state, e.g., hungry.
(defun in-state (x st)
  (find-out 'world (state x st 'pos)))

;  Returns non-nil if X believes that y relates to z in a certain way.
;  Usually either y or z is x.
(defun relate (x y z rel)
  (find-out x (relation y z rel 'pos)))

;  Looks up CD in the data base for who.  If there, return non-nil if
;  the CD is not a negative fact.  If not there, ask the user at the
;  terminal and save the result.  Note that the generator is used to
;  ask questions.
;
;  find-out is used to determine if a given character is in a
;  given state (e.g., is the character hungry or thirsty) and is
;  also used to determine how two characters relate to on another
;  (e.g., do they like one another?, does one have a tendency to
;  deceive the other, etc.).
(defun find-out (who cd)
  (let ((mode (knows-if who cd)))
    (cond (mode 
           (member 'pos mode))
          (t
           (say (mloc who cd))
           (format t "~% [Y/N]? ~%>")
           (let ((answer (equal (read) 'y)))
             (addfact who
                      (setrole 'mode
                               (list (if answer 'pos 'neg))
                               cd))
             answer)))))

;  True if y thinks x is a friend of his.
(defun is-friend-of (x y)
  (and (not (equal x y))
       (relate y x y 'like)))

;  Returns location of x.
(defun loc (x)
  (knows-loc 'world x))

;  True if x and y are in the same place.
(defun is-prox (x y)
  (equal (loc-name-of x)
         (loc-name-of y)))

;  A CD is true if it's an mloc and the content is in the person's
;  data base, or it's in the data base for world.
(defun is-true (cd)
  (if (equal (header-cd cd) 'mloc)
    (knows (cdpath '(val part) cd) (cdpath '(con) cd))
    (knows 'world cd)))

;  loc-name-of returns the real location of x.  This may involve going
;  up several levels -- e.g., when Joe takes a worm, its location is
;  stored as joe, but its real location is the location Joe is at.
(defun loc-name-of (x)
  (let ((loc-of-x (loc x)))
    (cond ((member x *all-locations*)
           x)
          ((member loc-of-x *all-locations*)
           loc-of-x)
          ;;; If something isn't anywhere in particular, 
          ;;; then it on the ground.
          ((null loc-of-x)
           'ground)
          (t
           (loc-name-of loc-of-x)))))

;  get-isa is like get but checks is-a node for x if x has no
;  y property.				
(defun get-isa (x y)
  (or (get y x)
      (get (get y 'is-a) x)))

;  Functions to build CD forms

;  Acts

(defun atrans (actor object to from)
  (list 'atrans 
        (list 'actor actor)
        (list 'object object)
        (list 'to to)
        (list 'from from)))

(defun cause (x y)
  (list 'cause
        (list 'ante x)
        (list 'conseq y)))

(defun grasp (actor object)
  (list 'grasp
        (list 'actor actor)
        (list 'object object)))

(defun un-grasp (actor object)
  (tf (grasp actor object)))

(defun ingest (actor object)
  (list 'ingest
        (list 'actor actor)
        (list 'object object)))

(defun mbuild (actor object)
  (list 'mbuild
        (list 'actor actor)
        (list 'object object)))

(defun mtrans (actor object to from)
  (list 'mtrans 
        (list 'actor actor)
        (list 'object object)
        (list 'to (list 'cp (list 'part to)))
        (list 'from from)))

(defun plan (actor object)
  (list 'plan
        (list 'actor actor)
        (list 'object object)))

(defun propel (actor object to)
  (list 'propel 
        (list 'actor actor)
        (list 'object object)
        (list 'to to)))

(defun ptrans (actor object to from)
  (if to
    (list 'ptrans 
          (list 'actor actor)
          (list 'object object)
          (list 'to to)
          (list 'from from))))

(defun wants (actor goal)
  (list 'want
        (list 'actor actor)
        (list 'object goal)))

;  States

(defun has (actor object)
  (list 'cont
        (list 'actor object)
        (list 'val actor)))

(defun is-at (actor loc)
  (list 'loc
        (list 'actor actor)
        (list 'val loc)))

(defun mloc (actor con)
  (list 'mloc
        (list 'con con)
        (list 'val (list 'cp (list 'part actor)))))

(defun state (actor st mode)
  (list st
        (list 'actor actor)
        (list 'mode (list mode))))

(defun relation (actor object rel mode)
  (list rel
        (list 'actor actor)
        (list 'to object)
        (list 'mode (list mode))))

(defun where-is (x)
  (list 'loc
        (list 'actor x)
        (list 'val '?unspecified)))

(defun who-has (x)
  (list 'cont
        (list 'actor x)
        (list 'val '?unspecified)))

;  Mode functions

(defun mode (cd)
  (cdpath '(mode) cd))

;  Affirm/Negate set the mode of a CD to true/false.

(defun affirm (cd)
  (if (member 'pos (mode cd))
    cd
    (setrole 'mode (cons 'pos (remove 'neg (mode cd))) cd)))

(defun negate (cd)
  (if (member 'neg (mode cd))
    (affirm cd)
    (setrole 'mode (cons 'neg (remove 'pos (mode cd))) cd)))

;  maybe makes a CD hypothetical -- doesn't matter if it's true or false.
(defun maybe (cd)
  (if (member 'maybe (mode cd))
    cd
    (setrole 'mode (cons 'maybe (mode cd)) cd)))

;  question/un-question make a CD a question/non-question -- doesn't
;  matter if it's true or false.
(defun question (cd)
  (if (member 'ques (mode cd))
    cd
    (setrole 'mode (cons 'ques (mode cd)) cd)))

(defun un-question (cd)
  (setrole 'mode (remove 'ques (mode cd)) cd))

;  tf adds "transition final" to a CD -- doesn't matter if it's true
;  or false.
(defun tf (cd)
  (if (member 'tf (mode cd))
    cd
    (setrole 'mode (cons 'tf (mode cd)) cd)))

;  future sets a CD to a future time.
(defun future (cd)
  (setrole 'time 'future cd))

;  Path
;
;  cdpath finds the filler at the end of the role list in a CD.
;
;  For example, if
;  CD = (mtrans (actor joe)
;               (object (ptrans (actor joe) 
;                               (object worm)
;                               (from joe)
;                               (to irving))))
;  then
;  (cdpath '(actor) cd) returns joe;
;  (cdpath '(object) cd) returns (ptrans (actor joe) 
;                                        (object worm)
;                                        (from joe)
;                                        (to irving));
;  (cdpath '(object object) cd) returns worm.
;
;  If a role doesn't exist in a CD form, then cdpath returns nil.
(defun cdpath (rolelist cd)
  (if (null rolelist)
    cd
    (cdpath (cdr rolelist) (filler-role (car rolelist) cd))))

; each string should start with a capital letter and then be lower case.
(defun beautify-string (string)
  (when (plusp (length string))
    (nstring-downcase string)
    (setf (aref string 0) (char-upcase (aref string 0))))
  string)

;  micro-mumble: micro English generator

;  say prints a CD as an English sentence.  If CD is an mloc of the
;  world, then only the fact itself is said, otherwise the whole mloc
;  is used.  The original CD is returned.  say1 is called with the 
;  infinitive flag off and the say-subject flag on.
(defun say (cd)
  (let ((cd-to-be-said (if (unify-cds '(mloc (val (cp (part world)))) cd)
                         (cdpath '(con) cd)
                         cd)))
    (flet ((say-it ()
             (format t "~%")
             (write-string
              (beautify-string
               (with-output-to-string (*standard-output*)
                 (say1 cd-to-be-said 
                       (or (cdpath '(time) cd-to-be-said)
                           *default-tense*)
                       nil
                       t))))
             (format t ".")))
      (say-it))
    cd))

;  say1 prints cd according to the program under the head predicate.
;  If no program is there, the CD is printed with <>s around it.
;  
;  These generation programs are lists of expressions to be evaluated.
;  Attached to primative acts, they are normally concerned with
;  generating subject-verb-object clauses.  Since some of the acts,
;  such as mtrans, want and plan, take subclauses, the generator has to
;  be recursive, so that the atrans program that generates the clause
;  "Joe gave Irving the worm" can also generate the subclause in
;  "Joe planned to give Irving the worm." This means that the programs have
;  to know when to say or not say the subject, when to use the 
;  infinitive form, and what tense to use.
;    subj = true means print the subject,
;    inf = true means use the infinitive form,
;    tense is set to either past, present, or future, or cond (for
;            conditional, i.e., hypothetical)
(defun say1 (cd tense inf subj)
  (let ((say-fun (get (header-cd cd) 'say-fun)))
    (if say-fun 
      (apply say-fun (list cd tense inf subj))
      (progn
        ; (break)
        (format t "~% < ~s > " cd))))) ; RJ

;  subclause recursively calls say1 with the subconcept at the 
;  endpoint of rolelist.  word, if non-nil, starts the subclause,
;  unless relative-pronoun has a better idea.  Tense is calculated 
;  by sub-tense.
(defun subclause (cd word rolelist tense)
  (when word
    (format t "~s " (or (relative-pronoun rolelist cd)
                          word)))
  (let ((subcd (cdpath rolelist cd)))
    (say1 subcd (sub-tense tense subcd) nil t)))

;  sub-tense is given a tense and a CD and picks the tense to use.
;  The given tense is used, except with states (i.e., don't
;  say "he told him where the honey would be" even though conceptually
;  that's right), and with past statements about the future (i.e., say
;  "he said he would" rather than "he said he will").
(defun sub-tense (tense subcd)
  (cond ((is-state subcd)
         *default-tense*)
        ((and (equal tense 'past)
              (equal (cdpath '(time) subcd) 'future))
         'cond)
        (t
         tense)))

;  relative-pronoun returns the word to start the subclause
;  for the CD at the end of the CD role path.
(defun relative-pronoun (rolelist cd)
  (let ((subcd (cdpath rolelist cd)))
    (cond ((and (equal (header-cd subcd) 'loc)
                (pcvar-p (cdpath '(val) subcd)))
           'where)
          ((pcvar-p (next-subject cd)) 
           'who)
          (t
           nil))))

;  next-subject returns the subject of a subconcept, which is normally
;  the actor slot, except for cont (where it's in the val slot) and
;  mloc (where it's in the part slot of the val slot).
(defun next-subject (cd)
  (let ((subcd (cdpath '(object) cd)))
    (cdpath (case (header-cd subcd)
              (cont '(val))
              (mloc '(val part))
              (t '(actor)))
            subcd)))

;  infclause calls recursively say1 with the subconcept at the
;  endpoint of rolelist.  An infinitive is printed, and the subject
;  is suppressed.
(defun infclause (cd rolelist subj-flag tense)
  (say1 (cdpath rolelist cd) tense t subj-flag))

;  Store say-funs for each of the CD forms

;  atrans may go to either "take" (if actor = to) or "give."
(defun say-atrans (cd tense inf subj)
  (cond ((equal (cdpath '(actor) cd) (cdpath '(to) cd))
         (say-subj-verb cd tense inf subj '(actor) 'take)
         (format t " ")
         (say-filler cd '(object))
         (say-prep cd 'from '(from) t))
        (t
         (say-subj-verb cd tense inf subj '(actor) 'give)
         (format t " ")
         (say-filler cd '(to))
         (format t " ")
         (say-filler cd '(object)))))

(put 'atrans 'say-fun 'say-atrans)

;  mtrans may go to either "ask whether" or "tell that"
(defun say-mtrans (cd tense inf subj)
  (cond ((member 'ques (cdpath '(object mode) cd))
         (say-subj-verb cd tense inf subj '(actor) 'ask)
         (format t " ")
         (say-filler cd '(to part))
         (format t " ")
         (subclause cd 'whether '(object) 'cond))
        (t
         (say-subj-verb cd tense inf subj '(actor) 'tell)
         (format t " ")
         (say-filler cd '(to part))
         (format t " ")
         (subclause cd 'that '(object) (cdpath '(time) cd)))))

(put 'mtrans 'say-fun 'say-mtrans)

;  ptrans may go to either "go" or "move."
(defun say-ptrans (cd tense inf subj)
  (cond ((equal (cdpath '(actor) cd)
                (cdpath '(object) cd))
         (say-subj-verb cd tense inf subj '(actor) 'go))
        (t
         (say-subj-verb cd tense inf subj '(actor) 'move)
         (format t " ")
         (say-filler cd '(object))))
  (say-prep cd 'to '(to) t))

(put 'ptrans 'say-fun 'say-ptrans)

;  mbuild may go to either "decide to" or "decide that."
(defun say-mbuild (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'decide)
  (format t " ")
  (cond ((equal (cdpath '(actor) cd)
                (cdpath '(object actor) cd))
         (infclause cd '(object) nil 'future))
        (t
         (subclause cd 'that '(object) 'future))))

(put 'mbuild 'say-fun 'say-mbuild)

;  propel goes to strike
(defun say-propel (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'strike)
  (format t " ")
  (say-filler cd '(to)))

(put 'propel 'say-fun 'say-propel)

;  grasp may go to either "let go of" or "grab."
(defun say-grasp (cd tense inf subj)
  (cond ((in-mode cd 'tf)
         (say-subj-verb cd tense inf subj '(actor) 'let)
         (format t " GO OF"))
        (t
         (say-subj-verb cd tense inf subj '(actor) 'grab)))
  (say-filler cd '(object)))

(put 'grasp 'say-fun 'say-grasp)

;  ingest may go to either "eat" or "drink."
(defun say-ingest (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor)
                 (if (equal (cdpath '(object) cd) 'water)
                   'drink
                   'eat))
  (format t " ")
  (say-filler cd '(object)))

(put 'ingest 'say-fun 'say-ingest)

;  plan goes to "plan."
(defun say-plan (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'plan)
  (format t " ")
  (infclause cd '(object) nil 'future))

(put 'plan 'say-fun 'say-plan)

;  want goes to "want to" -- the third argument of infclause is set to 
;  true if the subject of the subclause is different that the subject
;  of the main clause.
(defun say-want (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'want)
  (format t " ")
  (infclause cd 
             '(object)
             (not (equal (cdpath '(actor) cd)
                         (next-subject cd)))
             'future))

(put 'want 'say-fun 'say-want)

;  loc goes to "be near."
(defun say-loc (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'be)
  (or (pcvar-p (cdpath '(val) cd))
      (say-prep cd 'near '(val) t)))

(put 'loc 'say-fun 'say-loc)

;  cont goes to "have."
(defun say-cont (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(val) 'have)
  (format t " ")
  (say-filler cd '(actor)))

(put 'cont 'say-fun 'say-cont)

;  mloc may go to either "know that", "know whether", or "think that."
(defun say-mloc (cd tense inf subj)
  (say-subj-verb cd 
                 tense 
                 inf 
                 subj 
                 '(val part)
                 (if (or (relative-pronoun '(con) cd)
                         (is-true (cdpath '(con) cd)))
                   'know
                   'think))
  (format t " ")
  (subclause cd 'that '(con) *default-tense*))

(put 'mloc 'say-fun 'say-mloc)

;  health goes to "be alive"
(defun say-health (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'be)
  (format t " ")
  (format t "ALIVE"))

(put 'health 'say-fun 'say-health)

;  smart goes to "be bright"
(defun say-smart (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'be)
  (format t " ")
  (format t  "BRIGHT"))

(put 'smart 'say-fun 'say-smart)

;  hungry goes to "be hungry"
(defun say-hungry (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'be)
  (format t " ")
  (format t  "HUNGRY"))

(put 'hungry 'say-fun 'say-hungry)

;  thirsty goes to "be thirsty"
(defun say-thirsty (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'be)
  (format t " ")
  (format t "THIRSTY"))

(put 'thirsty 'say-fun 'say-thirsty)

;  cause may go to either "x if y" or "if x then y"
(defun say-cause (cd tense inf subj)
  (declare (ignore inf))
  (declare (ignore subj))
  (cond ((in-mode cd 'ques)
         (subclause cd nil '(conseq) 'future)
         (format t " IF ")
         (subclause cd nil '(ante) (case tense
                                     (figure 'present)
                                     (cond *default-tense*)
                                     (t tense))))
        (t
         (format t "IF ")
         (subclause cd nil '(ante) 'future)
         (format t " THEN ")
         (subclause cd nil '(conseq) 'cond))))

(put 'cause 'say-fun 'say-cause)

;  like goes to "like"
(defun say-like (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'like)
  (format t " ")
  (say-filler cd '(to)))

(put 'like 'say-fun 'say-like)

;  dominate goes to "dominate"
(defun say-dominate (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'dominate)
  (format t " ")
  (say-filler cd '(to)))

(put 'dominate 'say-fun 'say-dominate)

;  deceive goes to "deceive"
(defun say-deceive (cd tense inf subj)
  (say-subj-verb cd tense inf subj '(actor) 'deceive)
  (format t " ")
  (say-filler cd '(to)))

(put 'deceive 'say-fun 'say-deceive)

;  say-filler prints the CD at the end of a CD role path
(defun say-filler (cd rolelist)
  (say-pp (cdpath rolelist cd)))

;  say-pp prints a CD (adds "the" to object).
(defun say-pp (cd)
  (if (member cd *all-objects*)
    (format t "THE "))
  (format t "~s" cd))

;  say-prep prints a preposition plus a CD at the end of a role path,
;  if any exists.
(defun say-prep (cd prep rolelist &optional space)
  (let ((subcd (cdpath rolelist cd)))
    (cond (subcd
           (when space
             (format t " "))
           (format t "~s " prep)
           (say-pp subcd)))))

;  in-mode tests whether x is in CD's mode.
(defun in-mode (cd x)
  (member x (cdpath '(mode) cd)))

;  say-neg prints "not" if CD is negative.
(defun say-neg (cd &optional space0 space1)
  (when (in-mode cd 'neg)
    (when space0
      (format t " "))
    (format t "NOT")
    (when space1
      (format t " "))))

;  say-subj-verb prints the subject (unless suppressed by
;  subj = nil, infinitives, or an ?unspec as the subject) and verb, 
;  with auxillary and tensing, if any.  Note that future tense is 
;  treated as an auxillary.
(defun say-subj-verb (cd tense inf subj rolelist infinitive)
  (let ((subject (cdpath rolelist cd)))
    (cond (inf
           (when subj (say-pp subject) (format t " "))
           (say-neg cd nil t)
           (format t "TO ~s" infinitive))
          (t
           (when (not (pcvar-p subject)) 
             (say-pp subject)
             (format t " "))
           (let ((plural 
                  (get subject 'plural))
                 (auxilary
                  (cond ((in-mode cd 'maybe)
                         'might)
                        ((equal tense 'future)
                         (if (equal *default-tense* 'past)
                             'would
                           'will))
                        ((equal tense 'cond)
                         'would)
                        ((and (in-mode cd 'neg)
                              (not (equal infinitive 'be)))
                         'do))))
             (cond (auxilary
                    (say-tense cd tense inf subj auxilary plural)
                    (say-neg cd t nil)
                    (format t " ~s" infinitive))
                   (t
                    (say-tense cd tense inf subj infinitive plural)
                    (when (equal infinitive 'be)
                      (say-neg cd t nil)))))))))

;  say-tense prints a verb, with tense and number inflection.
;  Conjugations of irregular verbs are stored under the past and present
;  properties of the verb, in the format (singular plural) for each.
;  For regular verbs, say-tense adds "d", "ed", or "s" as appropriate.
(defun say-tense (cd tense inf subj infinitive plural)
  (declare (ignore cd))
  (declare (ignore inf))
  (declare (ignore subj))
  (let ((tense-forms (get infinitive tense)))
    (cond (tense-forms
           (format t "~s" (if plural
                            (cadr tense-forms)
                            (car tense-forms))))
          (t
           (format t "~s" infinitive)
           (case tense
             (past
              (if (not (or (equal (lastchar infinitive) #\E)
                           (equal (lastchar infinitive) #\e)))
                (format t "E"))
              (format t "D"))
             (present
              (if (not plural)
                (format t "S"))))))))

;  lastchar returns that last character in x
(defun lastchar (x)
  (car (last (explode x))))

(defun explode (x)
  (coerce (princ-to-string x) 'list))

;  Generator Dictionary
;
;  Set the past and/or present tenses for irregular verbs.
;  Each tense is of the form (singular plural).

(put 'be 'past '(was were))
(put 'be 'present '(is are))
(put 'do 'past '(did did))
(put 'do 'present '(does do))
(put 'drink 'past '(drank drank))
(put 'eat 'past '(ate ate))
(put 'give 'past '(gave gave))
(put 'go 'past '(went went))
(put 'go 'present '(goes go))
(put 'grab 'past '(grabbed grabbed))
(put 'have 'past '(had had))
(put 'have 'present '(has have))
(put 'know 'past '(knew knew))
(put 'let 'past '(let let))
(put 'might 'past '(might might))
(put 'might 'present '(might might))
(put 'plan 'past '(planned planned))
(put 'strike 'past '(struck struck))
(put 'take 'past '(took took))
(put 'tell 'past '(told told))
(put 'think 'past '(thought thought))

;  Berries is the only plural in the current set-up.
(put 'berries 'plural t)

;  CD Functions

;  is-cd-p determines whether a given sexpr is a CD.
(defun is-cd-p (x)
  (and (listp x)
       (atom (header-cd x))
       (list-of-role-filler-pairs-p (roles-cd x))))

(defun list-of-role-filler-pairs-p (x)
  (or (null x)
      (and (listp x)
           (listp (car x))
           (atom (role-pair (car x)))
           (list-of-role-filler-pairs-p (cdr x)))))

;  header-cd gets the head act of a CD form.
(defun header-cd (x)
  (car x))

;  roles-cd gets the list of role-pairs of a CD form.
(defun roles-cd (x)
  (cdr x))

;  Role-pairs have the form (role filler).
;  role-pair returns the role.
(defun role-pair (x)
  (car x))

;  filler-pair returns the filler.
(defun filler-pair (x)
  (cadr x))

;  A filler for a role is found by looking for the role name in the CD,
;  and returning the filler if a pair is found.
(defun filler-role (role cd)
  (if (listp cd)
    (let ((pair (assoc role (roles-cd cd))))
      (if pair (filler-pair pair)))))

;  setrole makes a new CD form with (role filler) added
;  or replacing the old (role ...) pair.
(defun setrole (role filler cd)
  (cons (header-cd cd)
        (cons (list role filler)
              (delete-if #'(lambda (pair)
                             (eq (car pair) role))
                         (roles-cd cd)))))

;  Pattern Unifier
;  This unifier is an adapted version of the unify function which appears
;  in the book _Artificial_Intelligence_Programming_ (2nd ed.)
;  Eugene Chaniak, Drew McDermott, and James Meehan.

(defun unify (Pat1 Pat2)
  (unify-1 Pat1 Pat2 NIL))

(defun unify-1 (Pat1 Pat2 Sub)
  (cond ((pcvar-p Pat1)
         (var-unify Pat1 Pat2 Sub))
        ((pcvar-p Pat2)
         (var-unify Pat2 Pat1 Sub))
        ((atom Pat1)
         (cond ((eql Pat1 Pat2) (list Sub))
               (T NIL)))
        ((atom Pat2) 
         NIL)
        (T
         (mapcan #'(lambda (Sub)
                     (unify-1 (cdr Pat1) (cdr Pat2) Sub))
                 (unify-1 (car Pat1) (car Pat2) Sub)))))

(defvar *OccursCheck-P* T)

(defun var-unify (PCVar Pat Sub)
  (cond ((eql PCVar Pat)
         (list Sub))
        (T
         (let ((Binding (pcvar-binding PCVar Sub)))
           (cond (Binding
                  (unify-1 (binding-value Binding) Pat Sub))
                 ((and *OccursCheck-P*
                       (occurs-in-p PCVar Pat Sub))
                  NIL)
                 (T
                  (list (extend-binding PCVar Pat Sub))))))))

(defun occurs-in-p (PCVar Pat Sub)
  (cond ((pcvar-p Pat)
         (or (eq PCVar Pat)
             (let ((Binding (pcvar-binding Pat Sub)))
               (and Binding
                    (occurs-in-p PCVar (binding-value Binding) Sub)))))
        ((atom Pat)
         NIL)
        (T
         (or (occurs-in-p PCVar (car Pat) Sub)
             (occurs-in-p PCVar (cdr Pat) Sub)))))

(defun pcvar-binding (PCVar AList)
  (assoc PCVar AList))

(defun extend-binding (PCVar Pat AList)
  (cons (list PCVar Pat)
        AList))

(defun binding-value (Binding) (cadr Binding))

(defun pcvar-value (Pat Sub)
  (let ((Binding (pcvar-binding Pat Sub)))
    (cond ((null Binding) 
           Pat)
          (T
           (let ((Value (binding-value Binding)))
             (cond ((eql Value Pat)
                    Pat)
                   (T
                    (replace-variables Value Sub))))))))

(defun replace-variables (Pat Sub)
  (cond ((pcvar-p Pat)
         (pcvar-value Pat Sub))
        ((atom Pat)
         Pat)
        (T
         (cons (replace-variables (car Pat) Sub)
               (replace-variables (cdr Pat) Sub)))))

(defun instantiate (Pat Subs)
  (cond ((pcvar-p Pat)
         (let ((Entry (assoc Pat Subs)))
           (if Entry 
             (instantiate (cadr Entry) Subs)
             Pat)))
        ((atom Pat)
         Pat)
        (T
         (cons (instantiate (car Pat) Subs)
               (instantiate (cdr Pat) Subs)))))

;  CD Unifier
;  This replaces the less-general CD pattern matcher that was
;  used in the original Micro-Talespin program.  This unifier
;  allows pattern variables to appear on both of the
;  expressions to be compared while a pattern matcher
;  only allows variables to appear in one of the expressions.

(defun unify-cds (cd1 cd2)
  (unify-cds-1 cd1 cd2 nil))

(defun unify-cds-1 (cd1 cd2 sub)
  (and (eq (header-cd cd1) (header-cd cd2))
       (unify-pairs (roles-cd cd1) (roles-cd cd2) sub)))

;  unify-pairs sees if the roles and fillers of a CD can
;  be matched together.  It is more complicated than the
;  function unify-1 given above because (1) the role-filler pairs
;  do not need to be in the same order in the two CDs being
;  compared; (2) a missing pair in one CD means that that CD
;  is more general than the other CD and can, thus, be matched
;  against it; and, finally, (3) the filler of a pair can be a CD,
;  and most fillers which are lists are CDs, however, fillers which
;  are "modes" are the exception; they are fillers which are lists,
;  but are not CDs, so a special exception has to be made for them
;  in the unification procedure below.
(defun unify-pairs (pairs1 pairs2 sub)
  (if (or (null pairs1) (null pairs2))
    (list sub)
    (let* ((role 
            (role-pair (car pairs1)))
           (pair-from-pairs2
            (assoc role pairs2))
           (rest-of-pairs-from-pairs2
            (remove-if #'(lambda (pair)
                           (equal (role-pair pair) role))
                       pairs2))
           (newsubs
            (cond ((eq role 'mode)
                   (unify-1 (car pairs1) pair-from-pairs2 sub))
                   ((and pair-from-pairs2
                        (or (pcvar-p (cadr pair-from-pairs2))
                            (atom (cadr pair-from-pairs2))))
                   (unify-1 (car pairs1) pair-from-pairs2 sub))
                  ((and pair-from-pairs2
                        (or (pcvar-p (cadr (car pairs1)))
                            (atom (cadr (car pairs1)))))
                   (unify-1 (car pairs1) pair-from-pairs2 sub))
                  (pair-from-pairs2
                   (unify-cds-1 (car pairs1) pair-from-pairs2 sub))
                  (t
                   (list sub)))))
      (mapcan #'(lambda (newsub)
                  (unify-pairs (cdr pairs1)
                               rest-of-pairs-from-pairs2
                               newsub))
              newsubs))))

;  Done loading
(format t "~%Done loading Micro-Talespin")


