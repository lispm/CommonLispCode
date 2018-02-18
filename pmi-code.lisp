;;;
;;; pbi-code.cl
;;;
;;; contains all the function and macro definitions from Principles of
;;; Biomedical Informatics by Ira J. Kalet.  This code is available for
;;; use without restrictions.  I only ask that you cite the book in any
;;; written documents that report on the use of the code or actually
;;; include the code.
;;;
;;;-----------------------------------------------------

;;; only the function and macro definitions are here, not the example
;;; dialogs with the Lisp read-eval-print loop.  They are in order by chapter.

;;;-----------------------------------------------------
;;; Chapter 1
;;;-----------------------------------------------------

(defun hypotenuse (x y)
  "returns the hypotenuse of the triangle
      whose sides are x and y"
  (sqrt (+ (* x x) (* y y))))

(defun factorial (n)
  (if (= n 0) 1
    (* n (factorial (- n 1)))))

(defun all-pos (item seq start)
  (let ((pos (position item seq :start start)))
    (if pos
	(cons pos
	      (all-pos item seq (+ 1 pos)))
      nil)))

;;;-----------------------------------------------------
;;; there are three versions of all-positions.  Here I name them
;;;differently so the load function does not complain
;;;-----------------------------------------------------

(defun all-positions-1 (item seq)
  (all-pos item seq 0))

(defun all-positions-2 (item seq)
  (labels
      ((all-pos-aux (item seq start)
         (let ((pos (position item seq :start start)))
           (if pos (cons pos
                         (all-pos-aux item seq (+ 1 pos)))
             nil))))
    (all-pos-aux item seq 0)))

(defun all-positions-3 (item seq)
  (labels
      ((all-pos-aux (item seq start accum)
         (let ((pos (position item seq :start start)))
           (if pos
               (all-pos-aux item seq (+ 1 pos)
                            (cons pos accum))
             (reverse accum)))))
    (all-pos-aux item seq 0 nil)))

;;;-----------------------------------------------------

(defun count-g (dna)
  (if (null dna) 0
    (if (eql (first dna) 'g)
        (+ 1 (count-g (rest dna)))
      (count-g (rest dna)))))

(defun dna-count-simple (seq)
  "does an explicit count of the G A C and T in seq"
  (let ((ng 0)
        (na 0)
        (nc 0)
        (nt 0))
    (dolist (base seq)
      (case base
        (g (incf ng))
        (a (incf na))
        (c (incf nc))
        (t (incf nt))))
    (list ng na nc nt)))

(defun gc-ratio-broken (freq-table)
  (/ (+ (first freq-table) (third freq-table))
     (+ freq-table)))

(defun gc-ratio (freq-table)
  (/ (+ (first freq-table) (third freq-table))
     (apply #'+ freq-table)))

(defun gc-ratio-from-file (filename)
  (gc-ratio (dna-count-simple (get-data filename))))

(defun item-count (seq)
  "returns a frequency table of all the items in seq, a list
  of items, tagging the count by the item itself"
  (let ((results nil))
    (dolist (item seq results)
      (let ((tmp (find item results :key #'first)))
        (if tmp (incf (second tmp))
          (push (list item 1) results))))))

(defun naive-read-from-file (strm)
  (let ((char (read-char strm nil :eof)))
    (if (eql char :eof) nil
      (cons (intern (string char))
            (naive-read-from-file strm)))))

(defun read-from-file (strm accum)
  (let ((char (read-char strm nil :eof)))
    (if (eql char :eof) (reverse accum)
      (read-from-file strm (cons (intern (string char))
                                 accum)))))

(defun get-data (filename)
  (with-open-file (strm filename)
    (read-from-file strm nil)))

(defun read-from-file (strm accum)
  (let ((char (read-char strm nil :eof)))
    (cond ((eql char :eof) (reverse accum))
          ((member char (list #\g #\c #\a #\t #\G #\C #\A #\T))
           (read-from-file strm
                           (cons (intern (string-upcase
                                           (string char)))
                                 accum)))
          (t (read-from-file strm accum)))))

(defun parse-line (line)
  "parses tab delimited text, assumes no other whitespace between
  objects"
  (labels ((read-items (str accum pos tab)
             (if (>= pos (length str))
                 (reverse (if tab (cons "" accum) accum))
               (let ((first (char str pos)))
                 (if (eql first #\Tab)
                     (read-item str
                                (if tab (cons "" accum) accum)
                                (1+ pos) t)
                   (multiple-value-bind (item next)
                       (read-from-string str nil :eof :start pos
                                         :preserve-whitespace t)
                     (if (eql item :eof)
                         (reverse (if tab (cons "" accum) accum))
                       (read-items str (cons item accum)
                                   next nil))))))))
    (read-items line nil 0 t)))

(defun parse-loinc (filename)
  (let ((n 0))
    (labels ((read-loinc-records (strm accum)
               (let ((line (read-line strm nil :eof)))
                 (format t "Reading record ~A~%" (incf n))
                 (if (eql line :eof) (reverse accum)
                   (read-loinc-records strm
                                       (cons (parse-line line)
                                             accum))))))
      (with-open-file (strm filename)
        (list (parse-line (read-line strm))
              (read-loinc-records strm nil))))))

(defun print-loinc-record (headers record)
  (mapcar #'(lambda (hdr item)
              (format nil "~A = ~A" hdr item))
          headers record))

(defun make-sagittal-image (image-list)
  (let ((new-img (make-array '(512 512)))
        (image-num 0))
    (dolist (image image-list)
      (dotimes (i 512)
        (setf (aref new-img i image-num)
          (aref image i 256)))
      (incf image-num))
    new-img))

(defun make-graymap (window level range-top)
  (let* ((map (make-array (1+ range-top)))
         (low-end (- level (truncate (/ window 2))))
         (high-end (+ low-end window)))
    (do ((i 0 (1+ i)))
        ((= i low-end))
      (setf (aref map i) 0)) ;; black
    (do ((i low-end (1+ i)))
        ((= i high-end))
      (setf (aref map i)
        (round (/ (* 255 (- i low-end)) window))))
    (do ((i high-end (1+ i)))
        ((> i range-top))
      (setf (aref map i) 255))
    map))

(defun map-image (raw-image window level range)
  (let* ((x-dim (array-dimension raw-image 1))
         (y-dim (array-dimension raw-image 0))
         (new-image (make-array (list y-dim x-dim)))
         (map (make-graymap window level range)))
    (dotimes (i y-dim)
      (dotimes (j x-dim)
        (setf (aref new-image i j)
          (aref map (aref raw-image i j)))))
    new-image))

(defun diagnosis (patient-data)
   (find 'diagnosis (rest patient-data) :key #'first))

(defstruct person ()
  name birthdate telephone email)

(defstruct (patient (:include person))
  diagnosis appointments)

(defstruct (provider (:include person))
  specialty title office patients)

(defvar *patients*)

(defvar *providers*)

(defun add-provider (name specialty title)
  (push (make-provider :name name
                       :specialty specialty
                       :title title)
        *providers*))

(defun lookup-specialty (name)
  (provider-specialty (find name *providers*
                            :key #'person-name
                            :test #'string-equal)))

(defun update-patient-phone (name phone)
  (let ((record (find name *patients*
                      :key #'person-name
                      :test #'string-equal)))
    (setf (person-telephone record) phone)))

(defstruct heart ()
   size beat-rate x y z)

(defstruct kidney ()
   side x y z)

(defstruct tumor ()
   size grade tissue-type x y z)

(defmethod draw ((obj t) (v t))
  "DRAW (obj t) (v t)
This is a default or stub method so we can build and use the various
functions without crashing on not yet implemented draw calls."
  (format t "No DRAW method for class ~A in ~A~%"
          (class-name (class-of obj))
          (class-name (class-of v))))

(defclass heart ()
   ((size :accessor size)
    (beat-rate :accessor beat-rate)
    (x :accessor x)
    (y :accessor y)
    (z :accessor z)
    ))

(defclass kidney ()
   ((side :accessor side)
    (x :accessor x)
    (y :accessor y)
    (z :accessor z)
    ))

(defclass tumor ()
   ((size :accessor size)
    (grade :accessor grade)
    (tissue-type :accessor tissue-type)
    (x :accessor x)
    (y :accessor y)
    (z :accessor z)
    ))

(defclass patient ()
   ((name :accessor name :initarg :name)
    (hospital-id :accessor hospital-id :initarg :hospital-id)
    (age :accessor age :initarg :age)
    (address :accessor address :initarg :address)
    (diagnosis :accessor diagnosis :initarg :diagnosis)
    (lab-tests :accessor lab-tests :initarg :lab-tests)
    ))

(defclass address ()
   ((number :accessor number :initarg :number)
    (street :accessor street :initarg :street)
    (city :accessor city :initarg :city)
    (zip-code :accessor zip-code :initarg :zip-code)))

(defclass diagnosis ()
   ((name :accessor name :initarg :name)
    (evidence :accessor evidence :initarg :evidence)
    ))

(defmethod print-object ((obj patient) strm)
   (format strm "(Patient ~%")
   (format strm "  :name ~S~%" (name obj))
   (format strm "  :hospital-id ~S~%" (hospital-id obj))
   (format strm " ...etc. ) ~%"))

(defun read-object (stream)
   (apply #'make-instance (read stream)))

(defun read-object (stream)
   (eval (read stream)))

(defun slot-names (obj)
   (mapcar #'slot-definition-name
      (class-slots (class-of obj))))

(defun get-object-basic (in-stream)
  (let* ((current-key (read in-stream))
         (object (make-instance current-key)))
     (loop
        (setq current-key (read in-stream))
        (if (eq current-key :end)    ;; no more slots?
            (return object)
          (setf (slot-value object current-key)
              (read in-stream))))))

(defun put-object-basic (object out-stream &optional (tab 0))
   (tab-print (class-name (class-of object)) out-stream tab t)
   (dolist (slotname (slot-names object))
      (when (slot-boundp object slotname)
         (tab-print slotname out-stream (+ 2 tab))
         (tab-print (slot-value object slotname)
                    out-stream 0 t)))
   (tab-print :end out-stream tab t))

(defun tab-print (item stream tab &optional (new-line nil))
  (format stream "~A~S "
        (make-string tab :initial-element #\space)
        item)
  (when new-line (format stream "~%")))

(defun get-object (in-stream)
  (let* ((current-key (read in-stream))
         (object (if (eq current-key :end)
                     nil ;; end of object list
                   (make-instance current-key))))
     (loop
        (setq current-key (read in-stream))
        (if (eq current-key :end)    ;; no more slots?
            (return object)
          (setf (slot-value object current-key)
             (case (slot-type object current-key)
               (:simple (read in-stream))
               (:object (get-object in-stream))
               (:object-list
                  (let ((slotlist '())
                        (next-object nil))
                     (loop
                        (setq next-object
                          (get-object in-stream :parent object))
                        (if next-object
                            (push next-object slotlist)
                          (return (nreverse slotlist))))))))))))

(defun put-object (object out-stream &optional (tab 0))
   (tab-print (class-name (class-of object)) out-stream tab t)
   (dolist (slotname (slot-names object))
      (when (slot-boundp object slotname)
         (tab-print slotname out-stream (+ 2 tab))
         (case (slot-type object slotname)
           (:simple
               (tab-print (slot-value object slotname)
                          out-stream 0 t))
           (:object
               (fresh-line out-stream)
               (put-object (slot-value object slotname)
                           out-stream (+ 4 tab)))
           (:object-list
               (fresh-line out-stream)
               (dolist (obj (slot-value object slotname))
                  (put-object obj out-stream (+ 4 tab)))
               (tab-print :end out-stream (+ 2 tab) t)))))
   (tab-print :end out-stream tab t))

(defmethod slot-type ((object t) slotname)
  :simple)

(defmethod slot-type ((object patient) slotname)
  (case slotname
    ((address diagnosis) :object)
    (otherwise :simple)))

(defmethod slot-type ((object patient) slotname)
  (case slotname
    ((address diagnosis) :object)
    (otherwise (call-next-method))))

(defun get-all-objects (filename)
  (with-open-file (stream filename
                          :direction :input
                          :if-does-not-exist nil)
    (when (streamp stream)
      (let ((object-list '()))
        (loop
          (cond ((eq (peek-char t stream nil :eof) :eof)
                 (return object-list))
                (t (push (get-object stream) object-list))))))))

(defun put-all-objects (object-list filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :new-version)
    (dolist (obj object-list)
      (put-object obj stream))))

(defclass image ()
  ((uid :type string :accessor uid :initarg :uid)
   (patient-id :accessor patient-id :initarg :patient-id
               :documentation "The patient id of the
               patient this image belongs to.")
   (image-set-id :accessor image-set-id :initarg :image-set-id
                 :documentation "The image set id of the
                 primary image set the image belongs to.")
   (position :type string
             :accessor position :initarg :position
             :documentation "String, one of HFP, HFS,
             FFP, FFS, etc. describing patient position as
             scanned (Head/Feet-First Prone/Supine, etc).")
   (description :type string 
                :accessor description :initarg :description)
   (origin :type (vector single-float 3)
           :accessor origin :initarg :origin
           :documentation "Origin refers to the location in
           patient space of the corner of the image as defined
           by the point at pixel array reference 0 0 or voxel
           array reference 0 0 0.")
   (size :type list     ;; of two or three elements, x y z
         :accessor size :initarg :size
         :documentation "The size slot refers to the physical
         size of the image in each dimension, measured in
         centimeters in patient space.")
   ;; ...other slots
   )
  (:default-initargs :id 0 :uid "" :patient-id 0
                     :image-set-id 0 :position "HFS"
                     :description "")
  (:documentation "The basic information common to all types of
                   images, including 2-D images, 3-D images."))

(defclass image-2d (image)
  ((thickness :type single-float
              :accessor thickness :initarg :thickness)
   (x-orient :type (vector single-float 3)
             :accessor x-orient :initarg :x-orient
             :documentation "A vector in patient space defining
             the orientation of the X axis of the image in the
             patient coordinate system.")
   (y-orient :type (vector single-float 3)
             :accessor y-orient :initarg :y-orient
             :documentation "See x-orient.")
   (pix-per-cm :type single-float
               :accessor pix-per-cm :initarg :pix-per-cm)
   (pixels :type (simple-array (unsigned-byte 16) 2)
           :accessor pixels :initarg :pixels
           :documentation "The array of image data itself.")))

(defclass image-3d (image)
  ((voxels :type (simple-array (unsigned-byte 16) 3)
	   :accessor voxels
	   :initarg :voxels
	   :documentation "a 3-D array of image data values"))
  (:documentation "An image-3D depicts some 3-D rectangular
solid region of a patient's anatomy."))

(defun read-bin-array (filename size)
   (let ((bin-array (make-array (list size)
                      :element-type '(unsigned-byte 16))))
     (with-open-file (infile filename :direction :input
                                      :element-type
                                      '(unsigned-byte 16))
        (read-sequence bin-array infile))
     bin-array))

(defun read-bin-array (filename bin-array)
   (with-open-file (infile filename :direction :input
                                      :element-type
                                      '(unsigned-byte 16))
      (read-sequence bin-array infile)))

(defun read-bin-array (filename dimensions)
   (let* ((bin-array (make-array dimensions
                                 :element-type '(unsigned-byte 16)))
          (disp-array (make-array (array-total-size bin-array)
                                  :element-type '(unsigned-byte 16)
                                  :displaced-to bin-array)))
     (with-open-file (infile filename :direction :input
                             :element-type '(unsigned-byte 16))
        (read-sequence disp-array infile))
     bin-array))

(defun read-bin-array (filename bin-array)
   (let ((disp-array (make-array (array-total-size bin-array)
                                 :element-type '(unsigned-byte 16)
                                 :displaced-to bin-array)))
     (with-open-file (infile filename :direction :input
                             :element-type '(unsigned-byte 16))
        (read-sequence disp-array infile))))

(defun get-object (in-stream)
  (let* ((current-key (read in-stream))
         (object (if (eq current-key :end)
                     nil ;; end of object list
                   (make-instance current-key))))
     (loop
        (setq current-key (read in-stream))
        (if (eq current-key :end)    ;; no more slots?
            (return object)
          (setf (slot-value object current-key)
             (case (slot-type object current-key)
               (:simple (read in-stream))
               (:object (get-object in-stream))
               (:object-list
                  (let ((slotlist '())
                        (next-object nil))
                     (loop
                        (let ((obj (get-object in-stream
                                          :parent object)))
                          (if obj (push obj slotlist)
                            (return (nreverse slotlist)))))))
               (:bin-array
                  (let ((bin-info (read in-stream)))
                     (read-bin-array (first bin-info)
                                     (rest bin-info))))
               ))))))

(defun put-object-xml (object out-stream &optional (tab 0))
  (let ((tag (class-name (class-of object))))
    (print-xml-tag tag out-stream tab)
    (mapc #'(lambda (slotname)
              (when (slot-boundp object slotname)
                (print-xml-tag slotname out-stream (+ 2 tab))
                (case (slot-type object slotname)
                  (:simple (tab-print (slot-value object slotname)
                                      out-stream 0 t))
                  (:object (fresh-line out-stream)
                           (put-object-xml
                             (slot-value object slotname)
                             out-stream (+ 4 tab)))
                  (:object-list
                   (fresh-line out-stream)
                   (mapc #'(lambda (obj)
                             (put-object-xml obj out-stream
                                             (+ 4 tab)))
                         (slot-value object slotname))))
                ;; closing tag for each slot regardless of content
                (print-xml-end-tag slotname out-stream (+ 2 tab))
                ))
          (set-difference (slot-names object) (not-saved object)))
    (print-xml-end-tag tag out-stream tab))) ; terminates object

(defun print-xml-tag (tag stream tab)
  (format stream "~a<~a>~%"
          (make-string tab :initial-element #\space)
          tag))

(defun print-xml-end-tag (tag stream tab)
  (format stream "~a</~a>~%"
          (make-string tab :initial-element #\space)
          tag))

(defun make-xml-tag (tag stream tab &optional close)
  (format stream (if close "~a</~a>~%"
                     "~a<~a>~%")
          (make-string tab :initial-element #\space)
          tag))

(defun put-objects-xml (object-list filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :new-version)
    (dolist (obj object-list)
      (put-object-xml obj stream))))

(defmethod print-dom-node ((node dom1-text))
  (format t "Value: ~A~%" (dom-node-value node)))

(defmethod print-dom-node ((node dom1-element))
  (let ((name (dom-node-name node)))
    (format t "Element name: ~A~%" name)
    (mapcar #'print-dom-node
            (dom-child-node-list node))
    (format t "End of ~A~%" name)))

(defmethod print-dom-node ((node dom1-document))
  (format t "DOCUMENT ")
  (print-dom-node (dom-document-element node)))

;;;-----------------------------------------------------
;;; Chapter 2
;;;-----------------------------------------------------

(defvar *rules* (make-hash-table) "The so-called knowledge base")

(defun <-fn (consequent &optional antecedent)
  (push antecedent (gethash consequent *rules*)))

(defmacro <- (consequent &optional antecedent)
  (list 'push antecedent (list 'gethash consequent '*rules*)))

(defmacro <- (consequent &optional antecedent)
  (list 'push (list 'quote antecedent)
        (list 'gethash (list 'quote consequent) '*rules*)))

(defmacro <- (consequent &optional antecedent)
  `(push ',antecedent (gethash ',consequent *rules*)))

(defmacro <- (consequent &optional antecedent)
  "adds antecedent to the hash table entry for consequent, even if
  antecedent is nil"
  `(length (push ',antecedent (gethash ',consequent *rules*))))

(defun prove-simple (pred)
  "checks if an entry is present, and succeeds if there is a nil for
  simple assertion, or an expression that itself can be proved"
  (multiple-value-bind (ants found) (gethash pred *rules*)
    (cond ((not found) nil)
          ((member nil ants) t) ;; find won't work here!
          (t (some #'prove ants)))))

(defun prove (expr)
  (if (listp expr)
      (case (first expr)
        (and (every #'prove (reverse (rest expr))))
        (or  (some #'prove (rest expr)))
        (not (not (prove (second expr)))))
    (prove-simple expr)))

(defun printhash (hashtable)
  "prints out the contents of hashtable"
  (maphash #'(lambda (key val)
               (format t "Key: ~S   Value: ~S~%" key val))
           hashtable))

(defvar *clauses* nil
  "The cumulative list of clauses, aka knowledge base.")

(defstruct clause
  ants con count)

(defmacro -> (antecedents consequent)
  `(let* ((ants ',antecedents) ;; to avoid multiple eval
          (con ',consequent)
          (clause (make-clause :ants ants
                               :con con
                               :count (length ants))))
     (dolist (pred ants)
       (push clause (get pred 'on-clauses)))
     (push clause *clauses*)
     clause))

(defun init-stack (clauses)
  (let (stack)
    (dolist (clause clauses)
      (if (zerop (clause-count clause))
          (push (clause-con clause) stack)))
    stack))

(defun forward-chain (clauses)
  (labels ((fc-aux (stack results)
             (if (null stack) results
               (let ((prop (first stack))
                     (newprops nil))
                 (format t "Current prop: ~A~%" prop)
                 (dolist (clause (get prop 'on-clauses))
                   (format t "Clause: ~S~%" clause)
                   (if (zerop (decf (clause-count clause)))
                       (let ((concl (clause-con clause)))
                         (format t "Concl: ~A~%" concl)
                         (if (null concl) (return-from fc-aux 'fail)
                           (unless (find concl results)
                             (push concl newprops))))))
                 (fc-aux (append newprops (rest stack))
                         (cons prop results))))))
    (fc-aux (init-stack clauses) nil)))

(defmacro <- (con &optional ant)
  "adds ant to the hash table entry for con, even if ant is nil"
  `(length (push (cons (rest ',con) ',ant)
                 (gethash (first ',con) *rules*))))

(defun prove (expr &optional binds)
  (case (first expr)
    (and (prove-and (reverse (rest expr)) binds))
    (or  (prove-or (rest expr) binds))
    (not (prove-not (first (rest expr)) binds))
    (t   (prove-simple (first expr) (rest expr) binds))))

(defun prove-simple (pred args binds)
  (mapcan #'(lambda (r)
              (multiple-value-bind (b2 yes) 
                  (match args (first r) binds)
                (when yes
                  (if (rest r) (prove (rest r) b2) 
                    (list b2)))))
          (mapcar #'change-vars 
                  (gethash pred *rules*))))

(defun change-vars (r)
  (sublis (mapcar #'(lambda (v) (cons v (gensym "?")))
                  (vars-in r))
          r))

(defun var? (x)
  (and (symbolp x) 
       (eql (char (symbol-name x) 0) #\?)))

(defun vars-in (expr)
  (if (atom expr)
      (if (var? expr) (list expr))
    (union (vars-in (first expr))
           (vars-in (rest expr)))))

(defun match (x y &optional binds)
  (cond 
   ((eql x y) (values binds t))
   ((assoc x binds) (match (binding x binds) y binds))
   ((assoc y binds) (match x (binding y binds) binds))
   ((var? x) (values (cons (cons x y) binds) t))
   ((var? y) (values (cons (cons y x) binds) t))
   (t
    (when (and (consp x) (consp y))
      (multiple-value-bind (b2 yes) 
          (match (first x) (first y) binds)
        (and yes (match (rest x) (rest y) b2)))))))

(defun binding (x binds)
  (let ((b (assoc x binds)))
    (if b
        (or (binding (rest b) binds)
            (rest b)))))

(defun prove-and (clauses binds)
  (if (null clauses)
      (list binds)
    (mapcan #'(lambda (b)
                 (prove (first clauses) b))
            (prove-and (rest clauses) binds))))

(defun prove-or (clauses binds)
  (mapcan #'(lambda (c) (prove c binds))
          clauses))

(defun prove-not (clause binds)
  (unless (prove clause binds)
    (list binds)))

(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (prove ',query))
       (let ,(mapcar #'(lambda (v)
                         `(,v (binding ',v ,binds)))
                     (vars-in query))
         ,@body))))

(defclass frame ()
  ((id :reader id
       :initform (gentemp "frame-"))
   (name :accessor name
         :initarg :name
         :initform nil)
   (instance-of :accessor instance-of
                :initarg :instance-of
                :initform nil)
   (superclasses :accessor superclasses
                 :initarg :superclasses
                 :initform nil)
   (template-slots :accessor template-slots
                   :initarg :template-slots
                   :initform nil)
   (own-slots :accessor own-slots
              :initarg :own-slots
              :initform nil)
   (instances :accessor instances
              :initform nil)
   ))

(defvar *frames* nil "The global list of all frames")

(defun find-frame-by-id (frm-id)
  (find frm-id *frames* :key #'id))

(defun find-frame-by-name (frm-name)
  (find frm-name *frames* :key #'name :test #'equal))

(defun add-frame (frm)
  (push fr *frames*))

(defun remove-frame (frm)
  (setf *frames* (remove frm *frames*)))

(defun save-frame-kb (filename)
  (put-all-objects *frames* filename))

(defun restore-frame-kb (filename)
  (setq *frames* (get-all-objects filename)))

(defun slot-name (slot) (first slot))

(defun contents (slot) (second slot))

(defun (setf contents) (newval slot)
  (setf (second slot) newval))

(defun get-slot (fr name)
  (find name (own-slots fr) :key #'slot-name))

(defun slot-list (fr)
  (mapcar #'slot-name (own-slots fr)))

(defun slot-data (fr slot-name)
   (contents (get-slot fr slot-name)))

(defun (setf slot-data) (newval fr slot-name)
  (setf (contents (get-slot fr slot-name))
    newval))

(defun frame-type (fr)
  (let ((parent (instance-of fr)))
    (if parent (name (find-frame-by-id parent)))))

(defun all-superclasses (fr)
  "returns the frame-ids of all the frames that are superclasses of
  frame fr all the way up the class hierarchy"
  (let ((direct-sup-ids (superclasses fr)))
    (apply #'append
           direct-sup-ids
           (mapcar #'(lambda (frm-id)
                       (all-superclasses
                        (find-frame-by-id frm-id)))
                   direct-sup-ids))))

(defun all-template-slots (fr)
  (remove-duplicates
   (apply #'append
          (template-slots fr)
          (mapcar #'(lambda (id)
                      (template-slots (find-frame-by-id id)))
                  (all-superclasses fr)))))

(defun make-pairs (x)
  (if (oddp (length x)) (error "list length not even")
    (if x (cons (list (first x) (second x))
                (make-pairs (rest (rest x)))))))

(defun initialize-slot (slotname inits)
  (or (assoc slotname inits)
      (list slotname nil)))

(defun make-frame (name &key class superclasses template-slots
                        &rest slot-inits)
  (let* ((parent (if class (find-frame-by-name class)))
         (super-ids (mapcar #'(lambda (x)
                                (id (find-frame-by-name x)))
                            superclasses))
         (fr (make-instance 'frame
               :name name
               :instance-of (if parent (id parent))
               :superclasses super-ids
               :template-slots template-slots
               :own-slots
               (if parent
                   (mapcar #'(lambda (name)
                               (initialize-slot name
                                  (make-pairs slot-inits)))
                           (all-template-slots parent)))
               )))
    (if parent (push fr (instances parent)))
    (add-frame fr)
    fr))

(defun disease-lookup (drug-inst)
  (let ((drug-type (find-frame-by-id (slot-data drug-inst
                                                'instance-of))))
    (append (slot-data drug-inst 'diseases)
            (slot-data drug-type 'diseases)
            (mapcar #'(lambda (x) (slot-data
                                   (find-frame-by-id x)
                                   'diseases))
                    (all-superclasses drug-type)))))

(defun subsumed-by (fr1 fr2)
  (let ((id-list (superclasses fr1)))
    (cond ((null id-list) nil)
          ((find (frame-id fr2) id-list) t)
          (some #'(lambda (x)
                    (subsumed-by (find-frame-by-id x) fr2))
                id-list))))

(defun part-of (fr)
  (slot-data fr 'part-of))

(defun is-part-of (fr1 fr2)
  (let ((id-list (part-of fr1)))
    (cond ((null id-list) nil)
          ((find (frame-id fr2) id-list) t)
          (some #'(lambda (x)
                    (is-part-of (find-frame-by-id x) fr2))
                id-list))))

(defun connected-upward (fr1 fr2 link-fn)
   (let ((id-list (funcall link-fn fr1)))
     (cond ((null id-list) nil)
           ((find (frame-id fr2) id-list) t)
           ((some #'(lambda (x)
                      (connected-upward
                        (find-frame-by-id x) fr2 link-fn))
                  id-list)))))

(defun get-all-children (fr frame-kb)
  "searches the entire frame knowledge base to collect all the
   subclass-of descendants of frame fr."
  (let ((children nil))
    (dolist (entry frame-kb children)
      (if (subsumed-by entry fr)
          (push entry children)))))

(defun get-all-parts (fr frame-kb)
  "searches for items in the part-of subtree"
    (let ((parts nil))
      (dolist (entry frame-kb parts)
        (if (is-part-of entry fr)
            (push entry parts)))))

(defun slot-data (fr slot)
  "returns the contents of slot slot-name in frame fr, after
  executing any :if-needed function that might be present"
  (let* ((the-slot (get-slot fr slot))
         (if-needed-fn (second (member :if-needed the-slot))))
    (if if-needed-fn (funcall if-needed-fn fr slot))
    (second (get-slot fr slot))))

(defun (setf slot-data) (newval fr slot)
  "updates the contents of slot slot-name in frame fr, after
  executing any :if-added function that might be present"
  (let* ((the-slot (get-slot fr slot))
         (if-added-fn (second (member :if-added the-slot))))
    (if if-added-fn (funcall if-added-fn fr slot newval))
    (setf (second (get-slot fr slot)) newval)))

(defun set-attached-fn (fr slot fn key)
  "puts function fn in the slot named slot in frame fr, using key as
  a tag, e.g., :if-needed or :if-added or possibly other types of
  attached functions"
  (let* ((the-slot (get-slot fr slot))
         (length (length the-slot))
         (location (position key the-slot)))
    (if location (setf (elt the-slot (+ location 1)) fn)
      (setf (rest (last the-slot)) (list key fn)))))

(defun remove-attached-fn (fr slot key)
  "replaces any attached function for key to nil"
  (set-attached-fn fr slot nil key))

(defun simple-search (initial-state goal)
  (labels 
      ((search-inner (queue)
         (if (null queue) 'fail
           (let ((current (first queue)))
             (if (eql current goal)
                 'success
               (search-inner (append (successors current)
                                     (rest queue))))))))
    (search-inner (list initial-state))))

(defun better-search (initial-state goal? successors)
  (labels 
      ((search-inner (queue)
         (if (null queue) 'fail
           (let ((current (first queue)))
             (if (funcall goal? current) 'success
               (search-inner (append (funcall successors
                                              current)
                                     (rest queue))))))))
    (search-inner (list initial-state))))

(defun multi-search (initial-state goal? enough? successors)
  (labels 
      ((search-inner (queue wins)
         (if (null queue) wins
           (let ((current (first queue))
                 (remains (rest queue)))
             (cond ((funcall goal? current)
                    (setq wins (cons current wins))
                    (if (or (eq enough? t)
                            (and (null enough?)
                                 (null remains))
                            (and enough?
                                 (funcall enough? wins)))
                        wins
                      (search-inner remains wins)))
                   (t (search-inner (append (funcall successors
                                                     current)
                                            remains))))))))
    (search-inner (list initial-state) '())))

(defun gsearch (initial-state goal? enough? successors merge)
  (labels
      ((search-inner (queue wins)
         (if (null queue) wins
           (let ((current (first queue))
                 (remains (rest queue)))
             (cond ((funcall goal? current)
                    (setq wins (funcall merge (list current)
                                              wins))
                    (if (or (eq enough? t)
                            (and (null enough?)
                                 (null remains))
                            (and enough?
                                 (funcall enough? wins)))
                        (values wins remains)
                      (search-inner remains wins)))
                   (t
                    (search-inner (funcall merge
                                           (funcall successors
                                                    current)
                                           remains)
                                  wins)))))))
    (search-inner (list initial-state) '())))

(defun depth-first-search (initial-state goal? enough? successors)
  (gsearch initial-state goal? enough? successors #'append))

(defun breadth-first-search (initial-state goal? enough? successors)
  (gsearch initial-state goal? enough? successors
    #'(lambda (new-states queue) (append queue new-states))))

(defun hill-climb-search (initial-state goal? enough?
                          successors estim-fn)
  (gsearch initial-state goal? enough? successors
          #'(lambda (new-states queue)
              (append (sort new-states 
                            #'(lambda (s1 s2) 
                                (< (funcall estim-fn s1) 
                                   (funcall estim-fn s2))))
                      queue))))

(defun priority-merge (a b val-fn same?)
  (cond ((null a) b)
        ((null b) a)
        ((and same? (funcall same? (first a) (first b)))
         (cons (if (< (funcall val-fn (first a))
                      (funcall val-fn (first b)))
                   (first a)
                 (first b))
               (priority-merge (rest a) (rest b) val-fn same?)))
        ((< (funcall val-fn (first a))
            (funcall val-fn (first b)))
         (cons (first a)
               (priority-merge (rest a) b val-fn same?)))
        (t (cons (first b)
                 (priority-merge a (rest b) val-fn same?)))))

(defun best-first-search (initial-state goal? enough?
                          successors estim-fn same?)
  (gsearch initial-state goal? enough? successors
           #'(lambda (new-states queue)
               (priority-merge (sort new-states 
                                     #'(lambda (s1 s2) 
                                         (< (funcall estim-fn s1)
                                            (funcall estim-fn s2)))) 
                               queue estim-fn same?))))

(defun a*-search (initial-state goal? enough? successors g h* same?)
  (labels ((estim-fn (state)
             (+ (funcall g state) (funcall h* state))))
    (gsearch initial-state goal? enough? successors
             #'(lambda (new-states queue)
                 (priority-merge (sort new-states 
                                       #'(lambda (s1 s2) 
                                           (< (estim-fn s1)
                                              (estim-fn s2)))) 
                                 queue #'estim-fn same?)))))

(defun extend-path (path successors extender)
  (mapcar #'(lambda (new) (funcall extender new path))
          (funcall successors path)))

(defun path-search (start goal? enough? successors extender merge)
  (gsearch (list start) ;; makes start into a path of length 1
           goal? ;; keep goal parametrization
           enough? ;; and when to stop
           #'(lambda (current)
               (extend-path current successors extender))
           merge)) ;; keeps parametrization of control strategy

(defun all-paths-goal (current successors)
  (null (funcall successors current)))

;;;-----------------------------------------------------
;;; Chapter 3
;;;-----------------------------------------------------

(defun mean (m)
  (let ((n (1- (array-dimension m 0))))
    (/ (do ((i 0 (1+ i))
            (stop n)
            (result 0))
           ((> i stop) result)
         (incf result (aref m i)))
       n)))

(defmacro sum (var start stop &rest body)
  (let ((gstop (gensym))
        (gresult (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop)
          (,gresult 0))
         ((> ,var ,gstop) ,gresult)
       (incf ,gresult
             (progn ,@body)))))

(defun mean (m)
  (let ((n (1- (array-dimension m 0))))
    (/ (sum i 0 n (aref m i))
       n)))

(defun variance (m mu)
  (let* ((n (1- (array-dimension m 0)))
         (sigma-squared
           (/ (sum i 0 n (expt (- (aref m i) mu) 2))
              n)))
    (values sigma-squared (sqrt sigma-squared))))

(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string maxword))
          (pos 0))
      (do ((c (read-char s nil :eof) 
              (read-char s nil :eof)))
          ((eql c :eof))
        (if (or (alpha-char-p c) (char= c #\'))
            (progn
              (setf (aref buffer pos) c)
              (incf pos))
          (progn
            (unless (zerop pos)
              (see (intern (string-downcase 
                             (subseq buffer 0 pos))))
              (setf pos 0))
            (let ((p (punc c)))
              (if p (see p)))))))))

(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\; '|;|) 
    (#\! '|!|) (#\? '|?|) ))

(let ((prev `|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*))))
      (if (null pair)
          (push (cons symb 1) (gethash prev *words*))
        (incf (cdr pair))))
    (setf prev symb)))

(defun generate-text (n &optional (prev '|.|))
  (if (zerop n) (terpri)
      (let ((next (random-next prev)))
        (format t "~A " next)
        (generate-text (1- n) next))))

(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))

(defun entropy (probs)
  (- (apply #'+
       (mapcar #'(lambda (p)
                   (* p (log p 2)))
               probs))))

;;;-----------------------------------------------------
;;; Chapter 4
;;;-----------------------------------------------------

(defun boole-or-query (query doc)
  (some #'(lambda (x y) (plusp (logand x y)))
        query doc))

(defun boole-and-query (query doc)
  (every #'(lambda (x y) (if (plusp x) (plusp y) t))
         query doc))

(defun norm (v)
  (sqrt (sum i 0 (array-dimension v 0)
             (expt (aref v i) 2))))

(defun similarity (v1 v2)
  (/ (sum i 0 (array-dimension v1 0)
          (* (aref v1 i) (aref v2 i)))
     (* (norm v1) (norm v2))))

(defun item-count-hashed (stream)
  (let ((results (make-hash-table))
        (tmp (read stream nil :eof)))
    (until (eql tmp :eof)
      (if (gethash tmp results) (incf (gethash tmp results))
        (setf (gethash tmp results) 1))
      (setq tmp (read stream nil :eof)))
    results))

(defmacro until (test &rest body)
  `(do ()
       (,test)
     ,@body))

(require :aserve)
(require :pxml)

(defparameter +eutils-host+ "eutils.ncbi.nlm.nih.gov")
(defparameter +entrez-query-url+ "/entrez/eutils/esearch.fcgi")

(defun pubmed-query (searchstr &key start maximum)
  (let ((query-alist `(("db" . "m")
                       ("term" . ,searchstr)
                       ("mode" . "xml"))))
    (when maximum (push (cons "dispmax" maximum) query-alist))
    (when start (push (cons "dispstart" start) query-alist))
    (net.aserve.client:do-http-request
     (format nil "http://~a~a" +eutils-host+ +entrez-query-url+)
     :method :get
     :query query-alist)))

(defun whitespace-char-p (ch)
  "Our own definition of which characters are whitespace"
  (find ch '(#\Space #\Newline #\Tab #\Linefeed #\Return
             #\Page #\Null)
        :test #'char=))

(defun strip-blanks (terms)
  "terms is a nested list possibly containing blank strings at
  multiple levels.  This function returns a new list with the same
  items, each with blank strings recursively removed"
  (labels ((strip-aux (accum terms)
             (if (null terms) (reverse accum)
               (strip-aux
                  (let ((term (first terms)))
                    (typecase term
                      (string (if (every #'whitespace-char-p term)
                                  accum
                                (cons term accum)))
                      (list (cons (strip-aux nil term)
                                  accum))
                      (t (cons term accum))))
                  (rest terms)))))
    (strip-aux nil terms)))

(defparameter +entrez-fetch-url+ "/entrez/eutils/efetch.fcgi")

(defun pubmed-fetch (pmid)
  "Gets the XML for a single entry given the PubMed ID of the entry"
  (net.aserve.client:do-http-request
      (format nil "http://~a~a" +eutils-host+ +entrez-fetch-url+)
    :method :get
    :query
    `(("db" . "PubMed") ("report" . "xml") ("mode" . "text")
      ("id" . ,(format nil "~A" pmid)))))

(defun path-query (tree path)
  "returns the first match to path in tree, where path is a list of
  tags referring to nested lists"
  (cond ((null tree) nil)
        ((null path) tree)
        (t (let ((next (assoc (first path) tree)))
             (if next (path-query (rest next)
                                  (rest path))
               nil)))))

;;;-----------------------------------------------------
;;; Chapter 5
;;;-----------------------------------------------------

(defun read-fasta (strm allowed-chars accum)
  (let ((char (read-char strm nil :eof)))
    (cond ((eql char :eof) accum)
          ((eql char #\>)
           (if (null accum) ;; at beginning of sequence
               (progn
                 (read-line strm) ;; skip description and cont.
                 (read-fasta strm allowed-chars accum))
             (reverse accum)))
          ((member char allowed-chars)
           (read-fasta strm allowed-chars
                       (cons (intern (string-upcase (string char)))
                             accum)))
          (t (read-fasta strm allowed-chars accum)))))

(defconstant +aa-codes+
    (let ((valid-letters "ACDEFGHIKLMNPQRSTVWY"))
      (append (map 'list #'identity valid-letters)
	      (map 'list #'char-downcase valid-letters))))

(defconstant +base-codes+ 
    (let ((valid-letters "GCAT"))
      (append (map 'list #'char-downcase valid-letters)
	      (map 'list #'identity valid-letters))))

(defun read-first-protein (filename codes)
  "returns the first protein sequence in filename"
  (with-open-file (strm filename)
    (read-fasta strm +aa-codes+ nil)))

(defun read-fasta (strm &optional (allowed-chars +aa-codes+)
                                  (ac-parser #'sp-parser))
  (labels
      ((scan (accum)
         (let ((char (read-char strm nil :eof)))
           (cond ((eql char :eof) (reverse accum))
                 ((eql char #\>) (unread-char char strm)
                                 (reverse accum))
                 ((member char allowed-chars)
                  (scan (cons (intern (string-upcase (string char)))
                              accum)))
                 (t (scan accum))))))
    ;; the ac-parser call will return nil if end of file is reached
    (let ((accession-number (funcall ac-parser strm)))
      (if (null accession-number)
          nil
        (list accession-number (scan nil))))))

(defun sp-parser (strm)
  (let ((ac (make-string 6))
        (ch (read-char strm nil :eof))) ;; get rid of the >
    (if (eql ch :eof) nil ;; at end of file!
      (progn
        (dotimes (i 6)
          (setq ch (read-char strm nil :eof))
          (if (eql ch :eof) ;; shouldn't happen!
              (return-from sp-parser nil)
            (setf (aref ac i) ch)))
        (read-line strm nil :eof)
        ac))))

(defun read-proteins (filename n)
  "returns the first n protein sequences in filename"
  (let ((sequences nil))
    (with-open-file (strm filename)
      (dotimes (i n (reverse sequences))
        (push (read-fasta strm)
              sequences)))))

(defparameter alphabet '(a c t g))

(defparameter mutation-penalties
  '((a           (c . 0.3) (g . 0.4) (t . 0.3))
    (c (a . 0.4)           (g . 0.2) (t . 0.3))
    (g (a . 0.1) (c . 0.3)           (t . 0.2))
    (t (a . 0.3) (c . 0.4) (g . 0.1)          )))

(defconstant infinity 10000000.0)
(defconstant omit-penalty 0.5)
(defconstant insert-penalty 0.7)

(defun mutation-penalty (from to)
  (if (eql from to) 0.0
    (let ((from-entry (assoc from mutation-penalties)))
      (if from-entry
          (let ((to-entry (assoc to (rest from-entry))))
            (if to-entry (rest to-entry)
              infinity))
        infinity))))

(defstruct ms
   seq1 seq2 score history)

(defun ms-goal? (ms)
  (and (null (ms-seq1 ms))
       (null (ms-seq2 ms))))

(defun ms-same? (ms1 ms2)
  (and (eql (ms-seq1 ms1) (ms-seq1 ms2))
       (eql (ms-seq2 ms1) (ms-seq2 ms2))))

(defun generate-successors (ms)
  (let ((x (ms-seq1 ms))
        (y (ms-seq2 ms))
        (sc (ms-score ms))
        (hx (ms-history ms)))
    (if (not (null x))
        (if (not (null y))
            (if (eql (first x) (first y))
                (list (make-ms :seq1 (rest x) :seq2 (rest y)
                               :score sc 
                               :history (cons (list 'match
                                                    (first x))
                                              hx)))
              (list (make-ms :seq1 (rest x) :seq2 (rest y)
                             :score (+ sc (mutation-penalty
                                           (first x)
                                           (first y)))
                             :history (cons (list 'mutate
                                                  (first x)
                                                  (first y))
                                            hx))
                    (make-ms :seq1 x :seq2 (rest y)
                             :score (+ sc omit-penalty)
                             :history (cons (list 'omit (first y))
                                            hx))
                    (make-ms :seq1 (rest x) :seq2 y
                             :score (+ sc insert-penalty) 
                             :history (cons (list 'insert (first x))
                                            hx))))
          (list (make-ms :seq1 (rest x) :seq2 y
                         :score (+ sc insert-penalty) 
                         :history (cons (list 'insert (first x))
                                        hx))))
      (list (make-ms :seq1 x :seq2 (rest y)
                     :score (+ sc omit-penalty) 
                     :history (cons (list 'omit (first y))
                                    hx))))))

(defun found-one? (wins)
  (not (null wins)))

(defun match-hc (one two)
  (hill-climb-search
   (make-ms :seq1 one :seq2 two :score 0.0 :history nil)
   #'ms-goal?
   #'found-one?
   #'generate-successors
   #'ms-score))

(defun match-bf (one two)
  (best-first-search
   (make-ms :seq1 one :seq2 two :score 0.0 :history nil)
   #'ms-goal?
   #'found-one?
   #'generate-successors
   #'ms-score
   #'ms-same?))

(defun match-a* (one two)
  (a*-search
   (make-ms :seq1 one :seq2 two :score 0.0 :history nil)
   #'ms-goal?
   #'found-one?
   #'generate-successors
   #'ms-score ;; f
   #'(lambda (s) 0.0) ;; g*
   #'ms-same?))

(defparameter s1 '(a a t c t g c c t a t t g t c g a c g c))
(defparameter s2 '(a a t c a g c a g c t c a t c g a c g g))
(defparameter s3 '(a g a t c a g c a c t c a t c g a c g g))

(defun dna-to-rna-base (base)
  "for any dna base, returns the corresponding rna base"
  (if (eql base 't) 'u base))

(defun dna-to-rna (seq)
  "produces an rna sequence corresponding to the dna sequence seq"
  (mapcar #'dna-to-rna-base seq))

(defconstant +rna-to-amino-table+
    (make-array '(4 4 4) :initial-contents
                '(((gly gly gly gly)
                   (glu glu asp asp)
                   (ala ala ala ala)
                   (val val val val))
                  ((arg arg ser ser)
                   (lys lys asn asn)
                   (thr thr thr thr)
                   (met ile ile ile))
                  ((arg arg arg arg)
                   (glu glu his his)
                   (pro pro pro pro)
                   (leu leu leu leu))
                  ((trp STOP cys cys)
                   (STOP STOP tyr tyr)
                   (ser ser ser ser)
                   (leu leu phe phe))))
  "The genetic code table")

(defconstant +rna-indices+ '((g 0) (a 1) (c 2) (u 3)))

(defun rna-index (base)
  (second (assoc base +rna-indices+)))

(defun rna-to-amino (b1 b2 b3)
  "takes three bases and returns the corresponding amino acid"
  (aref +rna-to-amino-table+
        (rna-index b1)
        (rna-index b2)
        (rna-index b3)))

(defun rna-translate (mrna-seq)
  "takes an mrna sequence and converts it to a polypeptide,
   a sequence of amino acids"
  (cond ((null mrna-seq) nil)
        ((< (length mrna-seq) 3) nil)
        (t (cons (rna-to-amino (first mrna-seq)
                               (second mrna-seq)
                               (third mrna-seq))
                 (rna-translate (nthcdr 3 mrna-seq))))))

(defun is-stop (b1 b2 b3)
  (eql (rna-to-amino b1 b2 b3) 'stop))

(defun find-stops (mrna-seq)
  (labels ((fs-local (seq pos accum)
             (if (< (length seq) 3)
                 (reverse accum) ;; no complete codon left so done
               (cond ((is-stop (first seq) (second seq) (third seq))
                      (fs-local (rest seq) (1+ pos)
                                (cons (list pos (mod pos 3))
                                      accum)))
                     (t (fs-local (rest seq) (1+ pos) accum))))))
    (fs-local mrna-seq 0 nil)))

(defclass amino-acid ()
  ((name :accessor name
         :initarg :name
         :documentation "Symbol, the full name")
   (abr1 :accessor abr1
         :initarg :abr1
         :documentation "Symbol, single letter abbreviation")
   (abr3 :accessor abr3
         :initarg :abr3
         :documentation "Symbol, three letter abbreviation")
   (mass :accessor mass
         :initarg :mass
         :documentation "Average molecular mass in Daltons")
   (volume :accessor volume
           :initarg :volume)
   (surface-area :accessor surface-area
                 :initarg :surface-area)
   (part-spec-vol :accessor part-spec-vol
                  :initarg :part-spec-vol)
   (pk :accessor pk
       :initarg :pk
       :documentation "Ionization constant")
   (hydrophobicity :accessor hydrophobicity
                   :initarg :hydrophobicity)))

(defconstant +amino-acids+
    (list
     (make-instance 'amino-acid
       :name 'Alanine :abr1 'A :abr3 'ala
       :mass 71 :volume 88.6 :surface-area 115
       :part-spec-vol .748 :hydrophobicity .5)
     (make-instance 'amino-acid
       :name 'Arginine :abr1 'R :abr3 'arg
       :mass 156 :volume 173.4 :surface-area 225
       :part-spec-vol .666 :hydrophobicity -11.2)
     (make-instance 'amino-acid
       :name 'Asparagine :abr1 'n :abr3 'Asn
       :mass 114 :volume 117.7 :surface-area 160
       :part-spec-vol .619 :hydrophobicity -.2)
     (make-instance 'amino-acid
       :name 'Aspartate :abr1 'd :abr3 'asp
       :mass 115 :volume 111.1 :surface-area 150
       :part-spec-vol .579 :hydrophobicity -7.4)
     (make-instance 'amino-acid
       :name 'Cysteine :abr1 'c :abr3 'cys
       :mass 103 :volume 108.5 :surface-area 135
       :part-spec-vol .631 :hydrophobicity -2.8)
     (make-instance 'amino-acid
       :name 'Glutamine :abr1 'Q :abr3 'Gln
       :mass 128 :volume 143.9 :surface-area 180
       :part-spec-vol .674 :hydrophobicity -.3)
     (make-instance 'amino-acid
       :name 'Glutamate :abr1 'E :abr3 'glu
       :mass 129 :volume 138.4 :surface-area 190
       :part-spec-vol .643 :hydrophobicity -9.9)
     (make-instance 'amino-acid
       :name 'Glycine :abr1 'G :abr3 'gly
       :mass 57 :volume 60.1 :surface-area 75
       :part-spec-vol .632 :hydrophobicity 0)
     (make-instance 'amino-acid
       :name 'histidine :abr1 'H :abr3 'his
       :mass 137 :volume 153.2 :surface-area 195
       :part-spec-vol .67 :hydrophobicity .5)
     (make-instance 'amino-acid
       :name 'Isoleucine :abr1 'I :abr3 'ile
       :mass 113 :volume 166.7 :surface-area 175
       :part-spec-vol .884 :hydrophobicity 2.5)
     (make-instance 'amino-acid
       :name 'Leucine :abr1 'L :abr3 'leu
       :mass 113 :volume 166.7 :surface-area 170
       :part-spec-vol .884 :hydrophobicity 1.8)
     (make-instance 'amino-acid
       :name 'Lysine :abr1 'K :abr3 'lys
       :mass 128 :volume 168.6 :surface-area 200
       :part-spec-vol .789 :hydrophobicity -4.2)
     (make-instance 'amino-acid
       :name 'Methionine :abr1 'M :abr3 'met
       :mass 131 :volume 162.9 :surface-area 185
       :part-spec-vol .745 :hydrophobicity 1.3)
     (make-instance 'amino-acid
       :name 'Phenylalanine :abr1 'F :abr3 'phe
       :mass 147 :volume 189.9 :surface-area 210
       :part-spec-vol .774 :hydrophobicity  2.5)
     (make-instance 'amino-acid
       :name 'Proline :abr1 'P :abr3 'pro
       :mass 97 :volume 122.7 :surface-area 145
       :part-spec-vol .758 :hydrophobicity -3.3)
     (make-instance 'amino-acid
       :name 'Serine :abr1 'S :abr3 'ser
       :mass 87 :volume 89.0 :surface-area 115
       :part-spec-vol .613 :hydrophobicity -.3)
     (make-instance 'amino-acid
       :name 'Threonine :abr1 'T :abr3 'thr
       :mass 101 :volume 116.1 :surface-area 140
       :part-spec-vol .689 :hydrophobicity .4)
     (make-instance 'amino-acid
       :name 'Trypophan :abr1 'W :abr3 'trp
       :mass 186.21 :volume 227.8 :surface-area 255
       :part-spec-vol .734 :hydrophobicity 3.4)
     (make-instance 'amino-acid
       :name 'Tyrosine :abr1 'Y :abr3 'tyr
       :mass 163 :volume 193.6 :surface-area 230
       :part-spec-vol .712 :hydrophobicity 2.3)
     (make-instance 'amino-acid
       :name 'Valine :abr1 'V :abr3 'val
       :mass 99 :volume 140.0 :surface-area 155
       :part-spec-vol .847 :hydrophobicity 1.5)
     ))

(let ((amino-acid-table (make-hash-table)))
  (dolist (amino-acid +amino-acids+ 'done)
    (setf (gethash (name amino-acid) amino-acid-table)
      amino-acid)
    (setf (gethash (abr1 amino-acid) amino-acid-table)
      amino-acid)
    (setf (gethash (abr3 amino-acid) amino-acid-table)
      amino-acid))
  (defun lookup-amino-acid (aa-name)
    (gethash (if (stringp aa-name)
                 (read-from-string aa-name nil nil)
               aa-name)
             amino-acid-table)))

(defconstant +mass-water+ 18.0)

(defmethod mass ((pp polypeptide))
  (+ (mass (seq pp)) +mass-water+))
  
(defmethod mass ((seq list))
  (apply #'+
    (mapcar #'(lambda (x) (mass (lookup-amino-acid x)))
            seq)))

(defconstant ext-tyr 1490)
(defconstant ext-trp 5500)
(defconstant ext-cys 125) ;; this is actually cystine, not cysteine

(defun extinction-coeff (seq)
  (+ (* (count 'tyr seq) ext-tyr)
     (* (count 'trp seq) ext-trp)
     (* (count 'cys seq) ext-cys)))

(defun absorbance (seq)
  (/ (extinction-coeff seq) (mass seq)))

(defun profile (seq)
  "returns a profile table for any sequence of items - each table
  entry has the item, the occurrence count and the corresponding
  percentage or frequency."
  (let ((size (length seq)))
    (mapcar #'(lambda (x)
                (list (first x) ;; the name
                      (second x) ;; the count
                      (/ (second x) size))) ;; the percentage
            (item-count seq))))

(defun sort-profile-by-percent (profile)
  (sort (copy-list profile) #'< :key #'third))

(defun sort-profile-by-name (profile)
  (sort profile #'string-lessp
        :key #'(lambda (x) (symbol-name (first x)))))

(defun sum-square-diff (profile-1 profile-2)
  "Each profile is an alist sorted the same way.  The sum of the
  squared differences of the values is returned."
  (apply #'+ (mapcar #'(lambda (x y)
                         (expt (- (third x) (third y)) 2))
                     profile-1 profile-2)))

(defconstant +aa-symbols+
    (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'k 'l 'm
          'n 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z))

(defun make-full-profile (profile &optional (names +aa-symbols+))
  "adds any not found in sequence and sorts by name"
  (let ((full-profile (copy-list profile)))
    (dolist (name names)
      (unless (find name profile :key #'first)
        (push (list name 0 0) full-profile)))
    (sort-profile-by-name full-profile)))

(defun fasta-profiler (filename &optional (codes +aa-codes+)
                                          (ac-parser #'sp-parser))
  (let ((profiles '())
        (seq nil)
        (n 0))
    (with-open-file (strm filename)
      (loop
        (setq seq (read-fasta strm codes nil ac-parser))
        (if seq
            (progn
              (format t "~&Record ~A accession number ~A~%"
                      (incf n) (first seq))
              (push (list (first seq)
                          (profile (second seq)))
                    profiles))
          (return profiles))))))

(defun fasta-profiler-to-file (&optional (infile *standard-input*)
                                         (outfile *standard-output*)
                                         (codes +aa-codes+)
                                         (ac-parser #'sp-parser))
  (let ((seq nil)
        (n 0))
    (with-open-file (in-strm infile :direction :input)
      (with-open-file (out-strm outfile :direction :output)
        (loop
          (setq seq (read-fasta in-strm codes nil ac-parser))
          (if seq
              (progn
                (format t "~&Record ~A accession number ~A~%"
                        (incf n) (first seq))
                (pprint
                 (list (first seq)
                       (profile (second seq)))
                 out-strm))
            (return n)))))))

(defun compare-profiles-from-file (p filename)
  (let ((p2 (make-full-profile (second p))))
    (pprint p)
    (pprint p2)
    (with-open-file (strm filename)
      (do ((profile (read strm nil :eof) (read strm nil :eof))
           (results '())
           (n 1 (1+ n)))
          ((eql profile :eof) (sort results #'< :key #'second))
        (format t "~&Processing profile ~A~%" n)
        (push (list (first profile)
                    (sum-square-diff p2 (make-full-profile
                                          (second profile))))
              results)))))

(defun sum-square-diff (profile-1 profile-2)
  "Each profile is an alist sorted the same way.  The sum of the
  squared differences of the values is returned."
  (apply #'+ (mapcar #'(lambda (x y)
                         (expt (- (coerce (third x) 'single-float)
                                  (coerce (third y) 'single-float))
                               2))
                     profile-1 profile-2)))

(defun display-rank (data &optional n)
  (do* ((temp data (rest temp))
        (d (first temp) (first temp))
        (i 0 (1+ i)))
       ((if n (= i n) (endp temp)))
    (format t "~&~A  ~10,7F~%" (first d) (second d))))

(defun find-frame-by-name (name)
  (find name *frame-kb* :key #'name :test #'string-equal))

(defmacro is? (name1 connector name2)
  `(subsumed-by (find-frame-by-name ,name1)
                (find-frame-by-name ,name2)))

(defstruct reaction
      reactants  ;; Lefthand side
      products	 ;; Righthand side, significant products
      other-products) ;; non-significant products

(defvar *reactions* nil)

(defmacro add-reaction (reactants products &optional other-products)
  `(push (make-reaction :reactants ',reactants
                        :products ',products
                        :other-products ',other-products)
         *reactions*))

(defun make-reactions ()
  (add-reaction (fru) (f1p))
  (add-reaction (f1p) (glh dap))
  (add-reaction (glh) (g3p))
  (add-reaction (glu atp) (g6p) (adp))
  (add-reaction (g6p) (f6p))
  (add-reaction (f6p atp) (fbp) (adp))
  (add-reaction (fbp) (dap g3p))
  (add-reaction (dap) (g3p))
  (add-reaction (P NAD+ g3p) (bpg) (NADH H+))
  (add-reaction (bpg adp) (3pg) (atp))
  (add-reaction (3pg) (2pg))
  (add-reaction (2pg) (pep) (H2O))
  (add-reaction (pep atp) (pyr) (adp))
  (add-reaction (pyr NAD+ coa) (aca) (NADH H+ CO2))
  (add-reaction (cit) (ict))
  (add-reaction (ict NAD+) (akg) (NADH H+ CO2))
  (add-reaction (akg NAD+ coa) (sca) (NADH H+ CO2))
  (add-reaction (sca gdp P) (suc coa) (gtp))
  (add-reaction (suc FAD) (fum) (FADH2))
  (add-reaction (fum H2O) (mal))
  (add-reaction (mal NAD+) (oxa) (NADH H+)))	

(defvar *reactant-table* (make-hash-table))

(defun lookup-by-reactants (prod)
  (gethash prod *reactant-table*))

(defun init-reactant-table (rxns)
  (dolist (rxn rxns)
    (let ((reactants (reaction-reactants rxn)))
      (dolist (mol reactants)
        (pushnew rxn (gethash mol *reactant-table*))))))

(defun applicable-rxns (mols env)
  (apply #'append
         (mapcar #'(lambda (mol)
                     (remove-if-not
                      #'(lambda (x) (every #'(lambda (y)
                                               (find y env))
                                           (reaction-reactants x)))
                      (lookup-by-reactants mol)))
                 mols)))

(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

(defun applicable-rxns (mols env)
  (mappend #'(lambda (mol)
               (remove-if-not #'(lambda (x)
                                  (every #'(lambda (y)
                                             (find y env))
                                         (reaction-reactants x)))
                              (lookup-by-reactants mol)))
           mols))

(defun apply-rxn (rxn env)
  "returns a new environment with the products of rxn added to the
  current environment env.  This function uses but does not modify
  env."
  (labels ((add-prods (prods accum)
             (if (null prods) accum
               (if (find (first prods) accum)
                   (add-prods (rest prods) accum)
                 (add-prods (rest prods)
                            (cons (first prods) accum))))))
    ;; note we want to add the small molecules too if new, e.g., ADP
    (add-prods (reaction-other-products rxn)
               (add-prods (reaction-products rxn) env))))

(defstruct node rxn env)

(defun next-nodes (path)
  "returns a list of next nodes in the pathway graph from a given
  path, where path is a list of nodes, most recently added first."
  (let* ((current (first path))
         (last-rxn (node-rxn current))
         (env (node-env current))
         (rxns (applicable-rxns (reaction-products last-rxn)
                                env)))
    (mapcar #'(lambda (rxn)
                (make-node :rxn rxn :env (apply-rxn rxn env)))
            rxns)))

(defun metabolic-paths (end-product start-env max-paths)
  (path-search start-env
               ;; check if end-product is in the current environment
               #'(lambda (current) ;; current is a path
                   (find end-product (node-env (first current))))
               #'(lambda (wins) ;; no more than max-paths
                   (>= (length wins) max-paths))
               #'next-nodes ;; see above
               #'cons ;; just put new nodes on front
               #'(lambda (x y) (append y x)))) ;; breadth-first

(defun initial-nodes (mols env)
  (mapcar #'(lambda (rxn)
              (make-node :rxn rxn :env (apply-rxn rxn env)))
          (applicable-rxns mols env)))

(defun pprint-reaction (rxn)
  (format nil "~A --> ~A + ~A"
          (reaction-reactants rxn)
          (reaction-products rxn)
          (reaction-other-products rxn)))

(defun print-path (path)
  (let ((nice-rxns (mapcar #'(lambda (node)
                               (pprint-reaction (node-rxn node)))
                           path)))
    (dolist (rxn nice-rxns)
      (format t "~A~%" rxn))))

(defvar *init-env* '(atp adp gdp gtp fru glu NAD+ FAD P))

(defvar *init-mols* '(glu fru))

(defun init ()
  (make-reactions)
  (init-reactant-table *reactions*)
  (initial-nodes *init-mols* *init-env*))

(defun test (init-node n)
  (mapcar #'reverse ;; or make metabolic-paths reverse stuff
          (metabolic-paths 'pyr init-node n)))

(defun test2 (init-node n)
  (mapcar #'reverse
          (metabolic-paths 'mal init-node n)))

(defun make-state-table (&optional (size 100))
  (make-hash-table :size size))

(defun add-variable (table state-var initial-val)
  (setf (gethash state-var table) initial-val))

(defun next-state (table trans-fn history)
  "updates the state table in parallel, by computing the new state
  values, then adding them after doing all of them."
  (let (new-states)
    (maphash #'(lambda (var val)
                 (push (list var
                             (funcall trans-fn var table))
                       new-states))
             table)
    (mapc #'(lambda (new)
              (if history
                  (push (second new)
                        (gethash (first new) table))
                (setf (gethash (first new) table)
                  (second new))))
          new-states)))

(defun run-state-machine (table trans-fn
                          &key history (cycles 1))
  (dotimes (i cycles table)
    (next-state table trans-fn history)))

(defun box-transition (var box)
  (let ((switch-state (gethash 'switch box))
        (hand-state (gethash 'hand box))
        (lid-state (gethash 'lid box)))
    (case var
      (switch (if (and (eql switch-state 'on)
                       (eql hand-state 'out))
                  'off
                switch-state))
      (lid (if (and (eql switch-state 'on)
                    (eql lid-state 'closed))
               'open
             (if (and (eql switch-state 'off)
                      (eql lid-state 'open)
                      (eql hand-state 'in))
                 'closed
               lid-state)))
      (hand (if (and (eql switch-state 'on)
                     (eql hand-state 'in)
                     (eql lid-state 'open))
                'out
              (if (and (eql switch-state 'off)
                       (eql hand-state 'out))
                  'in
                hand-state))))))

(defparameter *model-graph*	;; (fig 1b)
    '((size + cln3)
      (cln3 + mbf)
      (cln3 + sbf)
      (sbf + cln12)
      (cln12 - sic1)
      (cln12 - cdh1)
      (sic1 - clb56)
      (sic1 - clb12)
      (mbf + clb56)
      (clb56 + mcm1/sff)
      (clb56 - sic1)
      (clb56 + clb12)
      (clb56 - cdh1)
      (cdh1 - clb12)
      (clb12 - cdh1)
      (clb12 + mcm1/sff)
      (clb12 + cdc20&14)
      (clb12 - sic1)
      (clb12 - swi5)
      (clb12 - mbf)
      (clb12 - sbf)
      (mcm1/sff + cdc20&14)
      (mcm1/sff + swi5)
      (mcm1/sff + clb12)
      (cdc20&14 + swi5)
      (cdc20&14 + cdh1)
      (cdc20&14 - clb12)
      (cdc20&14 + sic1)
      (cdc20&14 - clb56)
      (swi5 + sic1)
      ))

(defun make-graph-hash (graph)
  "returns a new hash table indexed by the target proteins,
  containing the influencing protein and its sign."
  (let ((graph-table (make-hash-table)))
    (dolist (arc graph graph-table)
      (push (butlast arc)
            (gethash (third arc) graph-table)))))

(defun set-proteins (proteins state table)
  (dolist (p proteins)
    (push (list state 0) (gethash p table))))

(defun graph-node-list (graph)
  (let (nodes)
    (dolist (arc graph nodes)
      (pushnew (first arc) nodes)
      (pushnew (third arc) nodes))))

(defun init-cell-cycle (graph initially-on-list)
  (let* ((nodes (graph-node-list graph))
         (state-table (make-state-table (length nodes))))
    (set-proteins nodes 0 state-table)
    (set-proteins initially-on-list 1 state-table)
    state-table))

(defparameter *self-degrading-proteins*
    '(cln3 cln12 mcm1/sff swi5 cdc20&14))

(defparameter *degrade-time* 5)

(defun self-degrading (p)
  (find p *self-degrading-proteins*))

(defun self-degradation (p table)
  (let ((history (gethash p table)))
    (and (self-degrading p)
         (>= (length history) *degrade-time*)
         (every #'(lambda (state)
                    (equal state '(1 0)))
                (subseq history 0 *degrade-time*)))))

(defun new-protein-state (p state-table graph-hash)
  "returns a list containing a 1 or 0 for the next state and the
  influence sum, for a particular protein, p, based on the current
  state.  If p is a self degrading protein, its history is checked
  and if it is time to degrade, its state becomes 0."
  (let ((influence-sum 0))
    (dolist (p2-arc (gethash p graph-hash))
      (let* ((p2 (first p2-arc))
             (p2-state (first (first (gethash p2 state-table)))))
        (case (second p2-arc)
          (+ (incf influence-sum p2-state))
          (- (decf influence-sum p2-state)))))
    (list
     (cond ((> influence-sum 0) 1)
           ((< influence-sum 0) 0)
           (t (if (self-degradation p state-table) 0
                (first (first (gethash p state-table))))))
     influence-sum)))

(defun run-cell-cycle (graph initially-on n)
  (let ((state-table (init-cell-cycle graph initially-on))
        (graph-hash (make-graph-hash graph)))
    (run-state-machine state-table
                       #'(lambda (var table)
                           (new-protein-state var table
                                              graph-hash))
                       :history t :cycles 1)
    (set-proteins initially-on 0 state-table)
    (run-state-machine state-table
                       #'(lambda (var table)
                           (new-protein-state var table
                                              graph-hash))
                       :history t :cycles n)))

;;;-----------------------------------------------------
;;; Chapter 6
;;;-----------------------------------------------------

(defconstant *data-flag* 4)
(defconstant *end-flag* 3)
(defconstant *fms-host* "fma.biostr.washington.edu")
(defconstant *fms-port* 8098)

#+lispworks (require "comm")

(defvar *fms-socket* nil "open socket to the fms")

(defun fms-connect (&optional (host *fms-host*) (port *fms-port*))  
  (let ((stream #+allegro (socket:make-socket :remote-host host
                                              :remote-port port)
                #+lispworks (comm:open-tcp-stream host port)
                #+cmu (system:make-fd-stream
                       (extensions:connect-to-inet-socket host port)
                       :input t :output t :element-type 'character)
                ))
    (format t "~A~%" (fms-readback stream))
    (setq *fms-socket* stream)))

(defun fms-readback (stream) 
  (let (item data)
    (loop
      (setq item (read stream nil :eof))
      (cond ((eql item :eof) (return data))
            ((eql item *data-flag*) (read-line stream)
             (setq data (read stream)))
            ((eql item *end-flag*)
             (read-line stream) (return data))
            (t (setq data item))))))

(defun fms-query (expr)
  (if (listp expr)
      (setq expr ;; convert to string with embedded " around terms
         (format nil "(~A~{ \"~A\"~})" (first expr) (rest expr))))
  (write-line expr *fms-socket*)
  (finish-output *fms-socket*)
  (fms-readback *fms-socket*))

(defun fms-disconnect (&optional (stream *fms-socket*))
  (fms-query stream "(quit)")
  (close stream)
  (format t "~%FMS Connection closed~%"))

(defun get-children (term relation)
  (fms-query (list 'fms-get-children term relation)))

(defun const-parts (term)
  (get-children term "constitutional part"))

(defun get-attributes (term)
  (fms-query (list 'fms-get-attributes term)))

(defun get-parents (term relation)
  (fms-query (list 'fms-get-parents term relation)))

(defun get-hierarchies (&optional term)
  (fms-query (if term (list 'fms-get-hierarchies term)
               (list 'fms-get-hierarchies))))

(defun all-subclasses (term)
  (let ((subs (get-children term ":DIRECT-SUBCLASSES")))
    (cond ((null subs) nil)
          (t (append subs
                     (mappend #'all-subclasses
                              subs))))))

(defun lymphatic-drainage (term)
  (get-children term "lymphatic drainage"))

(defun regional-parts (term)
  (get-children term "regional part"))

(defun efferent-to (term)
  (get-children term "efferent to"))

(defun afferent-to (term)
  (get-children term "afferent to"))

(defun find-all-paths (start successors extender merge)
  (path-search start
               #'(lambda (current) ;; goal - stop when no more nexts
                   (null (funcall successors current)))
               nil ;; get all the paths
               successors extender merge))

(defun lymphatic-paths (site)
  (mappend #'(lambda (start)
               (find-all-paths start
                               #'(lambda (path)
                                   (efferent-to (first path)))
                               #'cons #'append))
           (lymphatic-drainage site)))

(defun get-efferents (terms)
  (mapcar #'(lambda (x)
              (list x (efferent-to x)))
          terms))  

(defun check-efferents (termslots allowed)
  "termslots is a list of chains or vessels and their efferent to
   slot values. allowed is the complete list of allowed values"
  (let (good bad not-done)
    (dolist (chain termslots)
      (let ((efferents (second chain)))
        (cond ((null efferents) (push chain not-done))
              ((every #'(lambda (x)
                          (find x allowed :test #'string-equal))
                      efferents)
               (push chain good))
              (t (push chain bad)))))
    (list good bad not-done)))

;;;-----------------------------------------------------
;;; Chapter 7
;;;-----------------------------------------------------

(defclass drug-label ()
  ((name :documentation "The generic name of the drug")
   description
   (clinical-pharm :documentation "The pharmacologic action")
   (indications :documentation "List of uses of the drug")
   (contraindications :documentation "Conditions that indicate the
                                      drug should not be used")
   (precautions :documentation "Interactions usually included here")
   (dosage :documentation
           "Recommended dosage for various situations")
   ))

(defclass drug()
  ((generic-name :accessor generic-name
                 :documentation "Commonly used name")
   (drug-class :accessor drug-class  
               :documentation "the therepeutic class of the drug")
   (dosage :accessor dosage
           :documentation "a list of approved dosages")
   (form :accessor form
         :documentation "oral, transdermal, or iv")
   (indications :accessor indications
                :documentation "what is the drug supposed to treat,
                                i.e., a list of ICD-9 codes.")
   (side-effects :accessor side-effects
                 :documentation "a list of symbols naming side
                                 effects")
   (substrate-of :accessor substrate-of
                 :documentation "list of enzymes this drug is a
                                 substrate of")
   (inhibits :accessor inhibits
             :documentation "list of enzymes drug inhibits, with
                             mechanism, strength")
   (induces :accessor induces
            :documentation "list of enzymes this drug induces")
   (narrow-ther-index :accessor narrow-ther-index
                      :documentation "yes or no")
   (level-of-first-pass :accessor level-of-first-pass
                        :documentation "unknown or [enzyme level]")
   (prim-first-pass-enzyme :accessor prim-first-pass-enzyme)
   (prim-clearance-mechanism :accessor prim-clearance-mechanism
                             :documentation "metabolic or renal")
   (contraindications :accessor contraindications)
   (prodrug :accessor prodrug
            :documentation "is it a prodrug, yes or no?")
   ))

(defun recursive-lookup (drug-inst slotname)
  (let ((drug-type (find-frame-by-id (slot-data drug-inst
                                                'instance-of))))
    (append (slot-data drug-inst slotname)
            (slot-data drug-type slotname)
            (mapcar #'(lambda (x) (slot-data
                                   (find-frame-by-id x)
                                   slotname))
                    (all-superclasses drug-type)))))

(defun inhibition-interaction (precip obj)
   (null (set-difference (substrate-of obj)
                         (inhibits precip))))

(defun induction-interaction (precip obj)
   (null (set-difference (substrate-of obj)
                         (induces precip))))

(defun inhibition-interaction-3 (precip-1 precip-2 obj)
   (null (set-difference (substrate-of obj)
                         (append (inhibits precip-1)
                                 (inhibits precip-2)))))

(defun induction-interaction-3 (precip-1 precip-2 obj)
   (null (set-difference (substrate-of obj)
                         (append (induces precip-1)
                                 (induces precip-2)))))

(defun inhibition-interaction-n (obj &rest precips)
   (null (set-difference (substrate-of obj)
                         (mappend #'inhibits precips))))

(defun induction-interaction-n (obj &rest precips)
   (null (set-difference (substrate-of obj)
                         (mappend #'induces precips))))

(defun lookup (fr-name slotname)
  (slot-data (find-frame-by-name fr-name) slotname))

(defun inhibits (drugname)
  (lookup drugname 'inhibits))

(defun induces (drugname)
  (lookup drugname 'induces))

(defun substrate-of (drugname)
  (lookup drugname 'substrate-of))

(<- (metabolic-inhibit-interact ?precip ?object ?enz)
    (and (inhibits-primary-clearance-enzyme ?precip ?object ?enz)
         (narrow-ther-index ?object yes)))

(<- (inhibits-primary-clearance-enzyme ?precip ?object ?enz)
    (and (inhibits-partial-clearance ?precip ?object ?enz)
         (major-pathway ?object ?enz)))

(<- (inhibits-partial-clearance ?precip ?object ?enz)
    (and (inhibits-effectively ?precip ?enz)
         (substrate-of ?object ?enz)
         (primary-clearance-mechanism ?object metabolic)))
 
(<- (inhibits-effectively ?drug ?enz)
    (and (inhibits ?drug ?enz)
         (or (inhibit-strength ?drug ?enz strong)
             (inhibit-strength ?drug ?enz moderate))))

(defun interacts (drug-a drug-b)
   (or
      (intersection (effects drug-a)
                    (contraindications drug-b))
      (intersection (effects drug-b)
                    (contraindications drug-a))))

(defun effects (entryname)
   (recursive-lookup (find-frame-by-name entryname) 'effects))

(defun contraindications (entryname)
   (recursive-lookup (find-frame-by-name entryname)
                     'contraindications))

;;;-----------------------------------------------------
;;; Chapter 8
;;;-----------------------------------------------------

(defun tcp-server (applic-fn port)
   (let ((socket (make-socket :connect :passive
                              :address-family :internet
                              :type :stream
                              :local-port port))
         (stream nil))
      (unwind-protect
         (loop
            (setq stream (accept-connection socket :wait t))
            (funcall applic-fn stream)
            (close stream)))
      (close socket))) ;; only here if severe error occurs

(defun tcp-client (applic-fn port)
   (let ((stream (make-socket :connect :active
                              :address-family :internet
                              :type :stream
                              :remote-host host
                              :remote-port port)))
      (unwind-protect
          (funcall applic-fn stream))
      (close stream)))

(defun next-token (delimiter text start)
  (let ((end (position delimiter text :start start)))
    (values
     (subseq text start end) end)))

(defun scan-hl7 (delimiter text)
  "returns a list of tokens from text that are demarcated
   by delimiter"
  (labels ((scan-aux (delim txt start)
             (multiple-value-bind (token end)
                 (next-token delim txt start)
               (if end ;; non-null so more string is left
                   (cons token
                         (scan-aux delim txt (1+ end)))
                 ;; no divisions - just listify it
                 (list token)))))
    (scan-aux delimiter text 0)))

(defun delimiters (msg)
  "returns the 5 characters that are used as the field, component,
  subcomponent, etc. delimiters in the HL7 message msg as multiple
  values"
  (if (string= (subseq msg 0 3) "MSH")
      (values (elt msg 3) (elt msg 4) (elt msg 5)
              (elt msg 6) (elt msg 7))
    ;; this is probably a little severe...
    (error "Wrong first segment type")))

(defun hl7-message-scan (msg)
  "returns a list of segments, with each segment represented as a
  list of fields, and each field a list of components.  Assumes that
  segments are separated by <cr> but uses the embedded delimiters in
  the message for the fields and components."
  (multiple-value-bind (fld cmp rep esc subcmp) (delimiters msg)
    (mapcar #'(lambda (seg)
                (mapcar #'(lambda (field)
                            ;; separate each field into components
                            (scan-hl7 cmp field))
                        ;; separate each segment into fields
                        (scan-hl7 fld seg)))
            ;; separate msg into segments
            (butlast (scan-hl7 #\return msg)))))

(defun dicom-server (port)
   (let ((socket (make-socket :connect :passive
                              :address-family :internet
                              :type :stream
                              :local-port port)))
      (unwind-protect
            (dicom-state-machine socket :server nil))
      (close socket))) ;; only here if severe error occurs

(defun dicom-client (host port)
  (let ((env (list (list 'remote-host-name host)
                   (list 'remote-port port))))
    (unwind-protect
        (dicom-state-machine nil :client env))))

(defun path-find (path env)
  (cond ((null path) nil)
        ((null env) nil)
        ((null (rest path)) (assoc (first path) env))
        (t (path-find (rest path)
                      (rest (assoc (first path) env))))))

(defvar *event* nil)

(defun dicom-state-machine-simple (socket mode env)
  (let ((tcp-stream nil)
        (state 'state-01)
        (trans-data nil)
        (action-fn nil))
    (loop
      (setq trans-data (get-transition-data state *event*)
            action-fn (first trans-data)
            env (funcall action-fn env tcp-stream)
            state (second trans-data)))))

(defvar *event* nil)

(defun dicom-state-machine (socket mode env)
  (let ((tcp-stream nil)
        (state 'state-01)
        (trans-data nil)
        (action-fn nil)
        (iteration 0))
    (loop
      (case state
        (state-01 (if (eq mode :server)
                      (setq tcp-stream
                            (accept-connection socket :wait t)
                            *event* 'event-05)
                    (if (= iteration 0)
                        (setq *event* 'event-01)
                      (return))))
        ((state-02 state-05 state-07 state-10 state-11 state-13)
         (setq env (read-pdu tcp-stream env))))
      (setq trans-data (get-transition-data state *event*)
            action-fn (first trans-data)
            env (funcall action-fn env tcp-stream)
            state (second trans-data)
            iteration (1+ iteration)))))

(defun get-transition-data (state event)
  (let* ((state-entry (find state *state-table* :key #'first))
         (evlist (rest (rest state-entry)))
         (ev-entry (find event evlist :test #'member :key #'first)))
     (rest ev-entry)))

(defun ae-05 (env tcp-stream)
   "Issue connection open message"
   nil) ;; return a fresh, empty environment

(defun ae-06 (env tcp-stream)
   "Determine acceptability of A-ASSOCIATE-RQ"
   (cond ((and (remote-id-ok env tcp-stream)
               (SOP-Class-UID-OK env))
          (setq *event* 'event-07)
          (setq *presentation-contexts*
                (check-presentation-contexts env)))
         (t (setq *event* 'event-08)
            (setq *reject-reasons* (reject-reasons env))))
   env) ;; return env unchanged

(defun ae-07 (env tcp-strm)
  "Issue A-Associate-AC message and send associated PDU"
  (let ((limit (path-find '(:A-Associate-RQ
                            :User-Information-Item
                            :Max-DataField-Len-Item
                            Max-DataField-Len)
                           env)))
    (setq *max-datafield-len*
       (if (typep limit 'fixnum) (min limit #.PDU-Bufsize)
          #.PDU-Bufsize))
    (apply #'send-pdu :A-Associate-AC env tcp-strm *args*)
    (setq *args* nil)))

(defun ae-08 (env tcp-strm)
  "Issue A-Associate-RJ message and send associated PDU"
  (apply #'send-pdu :A-Associate-RJ env tcp-strm *args*)
  (setq *args* nil))

(defun ar-01 (env tcp-strm)
  "Send A-Release-RQ PDU"
  (send-pdu :A-Release-RQ env tcp-strm)
  nil)

(defun ar-02 (env tcp-strm)
  "Issue A-Release message"
  (setq *event* 'event-14)
  nil)

(defun ar-03 (env tcp-strm)
  "Issue A-Release confirmation message, close connection"
  (close-connection tcp-strm)
  nil)

(defun ar-04 (env tcp-strm)
  "Send A-Release-RSP PDU"
  (send-pdu :A-Release-RSP env tcp-strm)
  nil)

(defun aa-01 (env tcp-strm)
  "Error detected -- send A-Abort PDU (Service-User-Initiated)"
  (cond ((consp *args*)
         (apply #'send-pdu :A-Abort env tcp-strm *args*)
         (setq *args* nil))
        (t (send-pdu :A-Abort env tcp-strm
                     'Abort-Source 0 'Abort-Diagnostic 2)
           nil)))

(defun aa-02 (env tcp-strm)
  "ARTIM timeout"
  (when (eq *mode* :Client) (close-connection tcp-strm))
  nil)

(defun ae-01 (env tcp-strm)
  (open-connection (path-find '(remote-hostname) env)
                   (path-find '(remote-port) env))
  (setq *event* 'event-02)
  env)

(defun ae-02 (env tcp-strm)
  (send-pdu :A-Associate-RQ env *tcp-buffer* tcp-strm)
  env)

(defun read-pdu (tcp-stream env)
  (let ((tail 6) ;; read only 6 bytes to start
        (pdu-end 0) ;; will be set when PDU length is known
        (connection-reset nil)
        (timeout nil)
        (eof? nil))
    (unless
      (ignore-errors
        (cond ((= tail (read-sequence *tcp-buffer* tcp-strm
                                      :start 0 :end tail))
               (setq pdu-end ;; encoded PDU length + 6 bytes
                 (+ (decode-fixnum *tcp-buffer* 2 4) tail))
               (setq tail (read-sequence *tcp-buffer* tcp-strm
                                         :start tail :end pdu-end))
               t)
              (t (setq eof? t))))
      (setq connection-reset t))
    (setq *event*
      (cond
        (connection-reset 'event-17)
        (timeout 'event-18)
        (eof? 'event-17)
        ((< tail pdu-end) 'event-17)
        (t (let ((rule (get-rule (aref *tcp-buffer* head) :parser)))
              (if rule ;; try to parse the message
                  (multiple-value-bind (next-byte new-env)
                        (rule-based-parser rule
                              env *tcp-buffer* (+ head 6) pdu-end)
                     (cond
                       ((eql new-env :Fail) 'event-19)
                       ((= next-byte pdu-end)
                        (setq env new-env)
                        (case (first rule)
                          (:A-Associate-AC 'event-03)
                          (:A-Associate-RJ 'event-04)
                          (:A-Associate-RQ 'event-06)
                          (:P-Data-TF 'event-10)
                          (:A-Release-RQ (if (eq *mode* :Client)
                                             'event-12A
                                           'event-12B))
                          (:A-Release-RSP 'event-13)
                          (:A-Abort 'event-16)))
                       (t 'event-15) ;; inconsistent length PDU
                       ))
                'event-19))))) ;; unrecognized or invalid PDU
    env))

(defun rule-based-parser (rule env buffer head tail)
  (let ((init-head head)
        (init-env env))
    (dolist (term (rest rule))
      (multiple-value-bind (next-byte new-env)
           (parse-term term env buffer head tail) 
         (if (eq new-env :Fail)
             (return (values init-head :Fail))
           (setq head next-byte env new-env))))
    ;; If nothing added to environment, return it unchanged.
    ;; If anything added, package items added during parse
    ;; into a tagged structure and add it at front.
    (unless (eq env init-env)
      (do ((item env (rest item))
           (next (rest env) (rest next)))
          ((eq next init-env)
           (setf (rest item) nil)
           (setq env
              (cons (first rule) (nreverse env)))
           (setq env
              ;; If environment additions duplicate
              ;; items already there, ignore them.
              (if (equal env (first init-env)) init-env
                ;; Otherwise prepend new material.
                (cons env init-env))))))
    (values head env)))

(defun parse-term (term env tcp-buffer head tail)
  (let ((init-head head))
    (cond
      ((typep term 'fixnum) ;; required fixed byte value
       (cond ((>= head tail) (setq env :Fail))
             ((= term (aref tcp-buffer head))
              (setq head (1+ head)))
             (t (setq env :Fail))))
      ((eq term '=ignored-byte)
       (when (> (setq head (1+ head)) tail)
         (setq head init-head env :Fail)))
      ((keywordp term) ;; a sub-item with own rule, call parser
       (cond ((>= head tail) (setq env :Fail))
             (t (multiple-value-setq (head env)
                   (rule-based-parser
                       (get-rule item :parser)
                       env tcp-buffer head tail)))))
      ((eq (first term) '>decode-var)
       ;; ...
       )
      ;; ...more clauses
      )))

(defun parse-command (env *tcp-buffer* head tail)
  (dolist (msgtype *Message-Type-List*)
    (multiple-value-bind (next-byte new-env)
        (rule-based-parser (get-rule msgtype :parser)
                           env *tcp-buffer* head tail)
      (unless (eq new-env :Fail)
        (return (values msgtype new-env))))))

;;;-----------------------------------------------------
;;; Chapter 9
;;;-----------------------------------------------------

(defun all-nodes (fms site)
  (list (lymphatic-paths fms site)
        (let ((parts (get-children fms site "regional part")))
          (append (list site 'parts)
                  (mapcar #'(lambda (part) (all-nodes fms part))
                          parts)))))

(defconstant *clinical-regions*
  '(("Deep cervical lymphatic chain"                         "Va" "Vb")
    ("Deep parotid lymphatic chain"                          "P")
    ("Inferior deep lateral cervical lymphatic chain"        "IV")
    ("Jugular lymphatic chain"                               "VI" "IV")
    ("Jugular lymphatic trunk"                               "VI" "IV")
    ("Jugulo-omohyoid lymphatic chain"                       "III")
    ("Jugulodigastric lymphatic chain"                       "IIa")
    ("Left deep cervical lymphatic chain"                    "Va" "Vb")
    ("Left inferior deep lateral cervical lymphatic chain"   "IV")
    ("Left jugular lymphatic tree"                           "IV") ;VI?
    ("Left jugular lymphatic trunk"                          "VI" "IV")
    ("Left retropharyngeal lymphatic chain"                  "RP")
    ("Left submandibular lymphatic chain"                    "Ib")
    ("Left superficial cervical lymphatic chain"             )
    ("Left superior deep lateral cervical lymphatic chain"   "Va")
    ("Right deep cervical lymphatic chain"                   "Va" "Vb")
    ("Right inferior deep lateral cervical lymphatic chain"  "IV")
    ("Right jugular lymphatic tree"                          "IV") ;VI?
    ("Right jugular lymphatic trunk"                         "VI" "IV")
    ("Right retropharyngeal lymphatic chain"                 "RP")
    ("Right submandibular lymphatic chain"                   "Ib")
    ("Right superficial cervical lymphatic chain"            )
    ("Right superior deep lateral cervical lymphatic chain"  "Va")
    ("Submandibular lymph node"                              "Ib")
    ("Submandibular lymphatic chain"                         "Ib")
    ("Submental lymphatic chain"                             "Ia")
    ("Superficial cervical lymphatic chain"                  )
    ("Superior deep lateral cervical lymphatic chain"
                                            "IIa" "III" "IV")))

(defclass tumor-site ()
  ((name :initarg :name :accessor name)
   (part-of :initarg :part-of :accessor part-of)
   (parts :initarg :parts :accessor parts
          :documentation "A list of symbols naming
                          daughter nodes"))
  (:documentation "Each instance represents a single anatomic
                   site or larger anatomic region.  The tree
                   structure implied by the parts and part-of
                   slots describes anatomic relationships of
                   tumor sites and groups of tumor sites.")
  (:default-initargs :name nil :part-of nil :parts nil))

(defmethod initialize-instance :after ((site tumor-site)
                                       &rest initargs)
  "This method makes the site instance available by name"
  (set (name site) site))

(defun print-tree (node &key (indent 0) (stream t))
  (when node
    (tab-print (name node) stream indent t)
    (mapc #'(lambda (x) (print-tree (symbol-value x) 
                                    :indent (+ indent 3)
                                    :stream stream))
          (parts node))))

(defun assert-value (pred obj &optional val)
  "converts an object value pair to an assertion named pred"
  (if val (eval `(<- (,pred ,obj ,val)))
    (eval `(<- (,pred ,obj)))))

(defmethod initialize-instance :after ((site tumor-site)
                                       &rest initargs)
  "This method makes the site instance available by name
   and registers a rule for the part-of slot."
  (set (name site) site)
  (assert-value 'part-of (name site) (part-of site)))

(<- (within ?x ?x))

(<- (within ?x ?y) (and (part-of ?x ?z)
                        (within ?z ?y)))

(<- (pt-movement ?x (0.1 0.1 0.1))
    (and (location ?x ?y)
         (within ?y head-and-neck)
         (immob-dev ?x mask)))

(<- (setup-error ?x (0.5 0.5 0.5))
    (and (location ?x ?y)
         (within ?y head-and-neck)
         (immob-dev ?x mask)))

(<- (pt-movement ?x (0.3 0.3 0.3))
    (and (location ?x ?y)
         (within ?y head-and-neck)
         (immob-dev ?x none)))

(<- (setup-error ?x (0.8 0.8 0.8))
    (and (location ?x ?y)
         (within ?y head-and-neck)
         (immob-dev ?x none)))

(<- (setup-error ?x (0.6 0.6 0.6))
    (and (location ?x ?y)
         (within ?y lung)
         (immob-dev ?x alpha-cradle)))

(<- (pt-movement ?x (0.2 0.2 0.2))
    (and (location ?x ?y)
         (within ?y lung)
         (immob-dev ?x alpha-cradle)))

(<- (tumor-movement ?x (0.0 0.0 0.0))
    (and (location ?x ?y)
         (within ?y nasopharynx)))

(<- (tumor-movement ?x (0.8 0.0 0.0))
    (and (location ?x ?y)
         (within ?y lung)
         (region ?x mediastinum)))

(<- (tumor-movement ?x (0.5 0.5 1.0))
    (and (location ?x ?y)
         (within ?y lung)
         (region ?x lower-lobe)))

(defconstant *chi-sq-factor* 1.88)

(defun target-volume (tumor immob)
  (assert-value 'location tumor (site tumor))
  (assert-value 'immob-dev tumor immob)
  (let* ((setup-m (with-answer (setup-error ?y ?x)
                    (if (eql ?y tumor) (return ?x))))
         (tumor-m (with-answer (tumor-movement ?y ?x)
                    (if (eql ?y tumor) (return ?x))))
         (pt-m (with-answer (pt-movement ?y ?x)
                    (if (eql ?y tumor) (return ?x))))
         ;; prob-m is a list of the x, y and z margins
         (prob-m (mapcar #'(lambda (m) (* m *chi-sq-factor*))
                         (rms setup-m tumor-m pt-m))))
    (make-instance 'target
      :contours (expand-volume tumor prob-m))))

(defun rms (list-1 list-2 list-3)
  (mapcar #'(lambda (a b c)
              (sqrt (+ (* a a) (* b b) (* c c))))
          list-1 list-2 list-3))

;;;-----------------------------------------------------
;;; Chapter 10
;;;-----------------------------------------------------

(defvar beam 'off "Allowed values: off on")

(defvar current 'high "Allowed values: high low")

(defvar target 'in "Allowed values: in out")

(defun electron-mode ()
   (and (eql current 'low) (eql target 'out)))

(defun xray-mode ()
   (and (eql current 'high) (eql target 'in)))

(defun safe-mode ()
   (or (eql beam 'off) ;; safe for sure
       (eql target 'in) ;; safe even with current high
       (eql current 'low) ;; safe even with target out
       ))

(defun beam-on ()
   (if (eql beam 'off) (setf beam 'on)))

(defun beam-off ()
   (if (eql beam 'on) (setf beam 'off)))

(defun select-electrons ()
   (if (eql beam 'off)
       (setf current 'low target 'out)))

(defun select-xrays ()
   (if (eql beam 'off)
       (setf target 'in current 'high)))

(defun safe-mode ()
   (or (electron-mode)
       (xray-mode)
       ))

(defun target-in ()
   (if (eql target 'out) (setf target 'in)))

(defun target-out ()
   (if (eql target 'in) (setf target 'out)))

(defun beam-on-with-guard ()
   (if (and (eql beam 'off)
            (or (electron-mode) (xray-mode)))
       (setf beam 'on)))

(defvar accum 0.0)

(defvar prescribed 0.0)

(defvar tolerance 0.1)

(defun safe-dose ()
  (< accum (+ prescribed tolerance)))

(defun safe-linac ()
  (and (safe-mode) (safe-dose)))

(defun deliver-dose ()
  (if (eql beam 'on)
      (incf accum delta)))

(defun beam-off-dosim ()
  (if (and (eql beam 'on)
           (>= accum prescribed))
      (setf beam 'off)))

(defun beam-on-dose-guard ()
   (if (and (eql beam 'off)
            (or (electron-mode) (xray-mode))
            (and (< accum prescribed)))
       (setf beam 'on)))

;;;-----------------------------------------------------
;;; Appendix
;;;-----------------------------------------------------

(defmethod combine ((x number) (y number))
  (+ x y))

(defmethod combine ((x string) (y string))
  (concatenate 'string x y))

(defmethod combine (x y)
  (list x y))

(defmethod combine ((x ice-cream) (y topping))
  (format nil "~A ice cream with ~A topping." (name x) (name y)))

(defun map-image-fast (raw-image window level range)
  (declare (fixnum window level range)
           (type (simple-array (unsigned-byte 16) 2) raw-image))
  (let* ((x-dim (array-dimension raw-image 1))
         (y-dim (array-dimension raw-image 0))
         (new-image (make-array (list y-dim x-dim)
                                :element-type '(unsigned-byte 8)))
         (map (make-graymap window level range)))
    (declare (type fixnum x-dim y-dim))
    (declare (type (simple-array (unsigned-byte 8) 2) new-image))
    (declare (type (simple-array (unsigned-byte 8)) map))
    (dotimes (j y-dim)
      (declare (fixnum j))
      (dotimes (i x-dim)
        (declare (fixnum i))
        (setf (aref new-image j i)
          (aref map (aref raw-image j i)))))
    new-image))

;;;-----------------------------------------------------
;;; End of book code.
