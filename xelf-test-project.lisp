;;;; xelf-test-project.lisp

;;(in-package #:xelf-test-project)
(in-package :plong)
;;; "xelf-test-project" goes here. Hacks and glory await!

(defparameter *unit* 16)
(defun units (n) (* *unit* n))
(defparameter *width* 640)
(defparameter *height* 480)

(defclass ball (node)
  ((height :initform (units 1))
   (width :initform (units 1))
   (color :initform "white")
   (speed :initform 6)
   (heading :initform (direction-heading :downright))))

(defmethod update ((ball ball))
  (with-slots (heading speed) ball
    (move ball heading speed)))

(defmethod collide :after ((ball ball) (node node))
  (play-sample "bip.wav"))

(defparameter *brick-width* (units 2))
(defparameter *brick-height* (units 1.2))

(defclass brick (node)
  ((color :initform "gray60")
   (height :initform *brick-height*)
   (width :initform *brick-width*)))

(defmethod initialize-instance :after ((brick brick) &key color)
  (when color
    (setf (slot-value brick 'color) color)))

(defmethod collide ((ball ball) (brick brick))
  (with-slots (heading) ball
    (destroy brick)
    (incf heading (radian-angle 90))))

(defun ball () (slot-value (current-buffer) 'ball))
(defun paddle () (slot-value (current-buffer) 'paddle))

(defclass paddle (node)
  ((direction :initform nil)
   (height :initform (units 1))
   (width :initform (units 8))
   (color :initform "white")))

(defparameter *paddle-speed* 3)

(defun holding-left-arrow ()
  (or (keyboard-down-p :kp4)
      (keyboard-down-p :left)))

(defun holding-right-arrow ()
  (or (keyboard-down-p :kp6)
      (keyboard-down-p :right)))

(defun find-joystick-direction ()
  (let ((heading (when (left-analog-stick-ressed-p)
                   (left-analog-stick-heading))))
    (when heading
      (if (and (> heading (/ pi 2))
               (< heading (* 3 (/ pi 2 ))))
          :lef
          :right))))

(defun find-direction ()
  (or (when (plusp (number-of-joysticks))
        (find-joystick-direction))
      (cond ((holding-left-arrow) :left)
            ((holding-right-arrow) :right))))

(defmethod update ((paddle paddle))
  (with-slots (direction) paddle
    (setf direction (find-direction))
    (when direction
      (move paddle (direction-heading direction) *paddle-speed*))))

(defmethod collide ((paddle paddle) (wall wall))
  (with-slots (direction) paddle
    (setf direction (opposite-direction direction))
    (move paddle (direction-heading direction) *paddle-speed*)))

(defmethod english ((paddle paddle))
  (with-slots (direction) paddle
    (case direction
      (:left (direction-heading :upleft))
      (:right (direction-heading :upright))
      (otherwise (+ (slot-value (ball) 'heading)
                    (radian-angle 90))))))


(defmethod collide ((ball ball) (paddle paddle))
  (with-slots (heading speed) ball
    (setf heading (english paddle))
    (move ball heading speed)))

(defun make-wall (x y width height)
  (let ((wall (make-instance 'wall)))
    (resize wall width height)
    (move-to wall x y)
    wall))

(defun make-border (x y width height)
  (let ((left x)
        (top y)
        (right (+ x width))
        (bottom (+ y height)))
    (with-new-buffer
      (insert (make-wall left top (- right left) (units 1)))
      (insert (make-wall left top (units 1) (- bottom top (units -1))))
;;      (insert (make-wall left top (- bottom top) (units 1)))
      (insert (make-wall right top (units 1) (- bottom top (units -1))))
      (current-buffer))))

(defparameter *row-colors*
  '("dark orchid" "medium orchid" "orchid" "dark orange" "orange" "gold"))

(defun row-color (row)
  (nth (mod row (length *row-colors*))
       *row-colors*))

(defun make-puzzle ()
  (with-new-buffer
    (dotimes (row 6)
      (dotimes (column 17)
        (add-node (current-buffer)
                  (make-instance 'brick :color (row-color row))
                  (+ 50 (* column *brick-width*))
                  (+ 50 (* row *brick-height*)))))))

(defclass plong (buffer)
  ((paddle :initform (make-instance 'paddle))
   (ball :initform (make-instance 'ball))
   (background-color :initform "black")
   (width :initform *width*)
   (height :initform *height*)))

(defmethod initialize-instance :after ((plong plong) &key)
  (bind-event plong '(:r :control) 'start-game))

(defmethod start-game ((plong plong))
  (with-slots (ball paddle) plong
    (with-buffer plong
      (insert ball)
      (insert paddle)
      (move-to ball 80 280)
      (move-to paddle 110 400)
      (paste plong (make-border 0 0 (- *width* (units 1)) (- *height* (units 2))))
      (paste plong (make-puzzle)))))

(defun plong ()
  (setf *screen-height* *height*)
  (setf *screen-width* *width*)
  (setf *resizable* t)
  (setf *scale-output-to-window* t)
  (with-session
    (open-project :plong)
    (index-pending-resources)
    (let ((plong (make-instance 'plong)))
      (switch-to-buffer plong)
      (start-game plong))))

(plong)
