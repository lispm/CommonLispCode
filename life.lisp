;;;; Game of Life

;;; Clojure Version

;; http://programmablelife.blogspot.de/2012/08/conways-game-of-life-in-clojure.html
;; http://dl.dropbox.com/u/84194941/Code/Clojure/conways_game_of_life.clj

#||

(ns conways-game-of-life.core)

(defn create-world
  "Creates rectangular world with the specified width and height.
  Optionally takes coordinates of living cells."
  [w h & living-cells]
  (vec (for [y (range w)]
         (vec (for [x (range w)]
                (if (contains? (first living-cells) [y x]) "X" " "))))))

(defn neighbours
  "Determines all the neighbours of a given coordinate"
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn stepper
  "Returns a step function for Life-like cell automata.
   neighbours takes a location and return a sequential collection
   of locations. survive? and birth? are predicates on the number
   of living neighbours."
  [neighbours birth? survive?]
  (fn [cells]
    (set (for [[loc n] (frequencies (mapcat neighbours cells))
               :when (if (cells loc) (survive? n) (birth? n))]
           loc))))

; patterns
(def glider #{[2 0] [2 1] [2 2] [1 2] [0 1]})   
(def light-spaceship #{[2 0] [4 0] [1 1] [1 2] [1 3] [4 3] [1 4] [2 4] [3 4]})

; steppers
(def conway-stepper (stepper neighbours #{3} #{2 3}))

(defn conway
  "Generates world of given size with initial pattern in specified generation"
  [[w h] pattern iterations]
   (->> (iterate conway-stepper pattern)
        (drop iterations)
        first 
        (create-world w h)
        (map println)))

;; sample queries
;(conway [5 15] light-spaceship 4)
;(map (comp println #(conway [5 15] light-spaceship %)) (range 5))
;(map (comp println #(conway [4 4] glider %)) (range 5))

||#

;;; Common Lisp versions
;;; Copyright Rainer Joswig, joswig@lisp.de , 2012

#||

(defun create-world (size
                       &optional living-cells
                       &aux (world (make-array size :initial-element nil)))
  "Creates rectangular world with the specified width and height.
  Optionally takes coordinates of living cells."
  (loop for (x y) in living-cells do (setf (aref world x y) t))
  world)

(defun print-world (world)
  (loop for x below (array-dimension world 0) do
        (terpri)
        (loop for y below (array-dimension world 1)
              do (write-string (if (aref world x y) " X " "  ")))))

(defun neighbours (pos)
  "Determines all the neighbours of a given coordinate"
  (destructuring-bind (x y) pos
    (loop for dx in '(-1 0 1)
          append (loop for dy in '(-1 0 1)
                       when (not (= 0 dx dy))
                       collect (list (+ dx x) (+ dy y))))))

(defun frequency (items &aux (hmap (make-hash-table :test #'equal)))
  (loop for item in items do (incf (gethash item hmap 0)))
  (loop for k being each hash-key in hmap using (hash-value v)
        collect (list v k)))

(defun stepper (neighbours birth survive)
  "Returns a step function for Life-like cell automata.
   neighbours takes a location and return a sequential collection
   of locations. survive? and birth? are predicates on the number
   of living neighbours."
  (lambda (cells)
    (loop for (n item) in (frequency (mapcan neighbours cells))
          when (if (member item cells :test #'equal)
                   (member n survive)
                 (member n birth))
          collect item)))

(defvar *glider* '((2 0) (2 1) (2 2) (1 2) (0 1)))
(defvar *light-spaceship* '((2 0) (4 0) (1 1) (1 2) (1 3) (4 3) (1 4) (2 4) (3 4)))

(defun conway (size pattern iterations)
  "Generates world of given size with initial pattern in specified generation"
  (let ((conway-stepper (stepper #'neighbours '(3) '(2 3))))
    (loop repeat iterations
          for cells = pattern then (funcall conway-stepper cells)
          finally do (print-world (create-world size cells))
          (terpri))))

(defun ex1 (&optional (n 10))
  (conway  '(5 15) *light-spaceship* n))

||#


;;; ================================================================
;;; Slightly more advanced version of above Game of Life

; utils

(defun frequency (items &key (test 'eql) &aux (hmap (make-hash-table :test test)))
  "return a hashtable with the item frequencies"
  (loop for item in items do (incf (gethash item hmap 0)))
  hmap)

; cells and worlds

(defun add-cell (world x y)
  "add a cell to the world"
  (setf (gethash (list x y) world) t)
  world)

(defun cell-occupied-p (world x y)
  "is the cell occupied in the world?"
  (gethash (list x y) world))

(defun make-world (&optional living-cells &aux (world (make-hash-table :test #'equal)))
  "Returns a world, implemented as a hash-table"
  (loop for (x y) in living-cells do (add-cell world x y))
  world)

(defun print-world (world w h)
  (loop for x below w do
        (loop for y below h do
              (write-string (if (cell-occupied-p world x y) " X " "   ")))
        (terpri)))

(defun iterate-cells-loc (world fn)
  "Iterates function fn over the cells of the world"
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall fn k))
           world))

; engine

(defun neighbours (pos)
  "Determines all the neighbours of a given coordinate"
  (destructuring-bind (x y) pos
    (loop for dx in '(-1 0 1)
          append (loop for dy in '(-1 0 1)
                       when (not (= 0 dx dy))
                       collect (list (+ dx x) (+ dy y))))))

(defun all-neighbour-locations (world neighbours &aux (locs '()))
  "returns a list of all neighbour locations in the world"
  (iterate-cells-loc world
                     (lambda (loc)
                       (loop for loc1 in (funcall neighbours loc)
                             do (push loc1 locs))))
  locs)

(defun stepper (neighbours birth survive)
  "Returns a step function for Life-like cell automata.
   neighbours takes a location and return a sequential collection
   of locations. survive and birth are lists of numbers."
  (lambda (world new-world)
    (loop for (x y) being each hash-key
          in (frequency (all-neighbour-locations world neighbours) :test #'equal)
          using (hash-value n)
          when (if (cell-occupied-p world x y)
                   (member n survive)
                 (member n birth))
          do (add-cell new-world x y))
    new-world))

; conway

(defun conway (pattern iterations w h)
  "Generates world with initial pattern in specified generation.
Prints the result in a give size w and h."
  (let ((conway-stepper (stepper #'neighbours '(3) '(2 3))))
    (loop repeat iterations
          for world = (make-world pattern) then (funcall conway-stepper world (make-world))
          do (print-world world w h))))

; example

(defvar *glider*
  '((2 0) (2 1) (2 2) (1 2) (0 1))
  "a glider pattern for the game of life")

(defvar *light-spaceship*
  '((2 0) (4 0) (1 1) (1 2) (1 3) (4 3) (1 4) (2 4) (3 4))
  "a light-spaceship pattern for the game of life")

(defun ex1 (&optional (n 10))
  "example of the game of life"
  (conway *light-spaceship* n 20 10))



