#+quicklisp (eval-when (:execute :load-toplevel)
              (ql:quickload :trivial-gamekit))

(defpackage #:trivial-snake
  (:use #:cl #:cl-user #:gamekit)
  (:export #:main))

(in-package :trivial-snake)

(defparameter *width* 800)
(defparameter *heigth* 600)
(defparameter *resolution* '(width height))
(defparameter *key-bag* nil)
(defparameter *title* "Trivial Snake")
(defparameter *red* (vec4 255 0 0 1))
(defparameter *green* (vec4 0 255 0 1))
(defparameter *cyan* (vec4 0 255 255 1))
(defparameter *black* (vec4 0 0 0 1))
(defparameter *snake-face* :space)
(defparameter *snake-body* (list (vec2 100 200)
                                 (vec2 120 200)
                                 (vec2 140 200)))
(defparameter *snake-size* 20)
(defparameter *snake-color* *black*)
(defparameter *snake-head* *cyan*)
(defparameter *frame-counter* 1)
(defparameter *apple* (vec2 200 200))
(defparameter *delay* 5)
(defparameter *game-over* nil)


;; game initialization
(gamekit:defgame trivial-snake () ()
  (:viewport-width *width*)
  (:viewport-height *heigth*)
  (:viewport-title *title*))

(defun restart-game ()
  (defparameter *snake-body* (list (vec2 100 200)
                                   (vec2 120 200)
                                   (vec2 140 200)))
  (defparameter *apple* (sane-random-apple))
  (defparameter *snake-face* :space))

(defun debug-draw ()
  (gamekit:draw-text (format nil "key pressed: ~a" *key-bag*)
                     (gamekit:vec2 10 10))
  (gamekit:draw-text (format nil "snake-face: ~a " *snake-face*)
                     (gamekit:vec2 550 10))
  (gamekit:draw-text (format nil "snake-cells: ~a" (length *snake-body*))
                     (gamekit:vec2 10 (- *heigth* 20))))


(defun add-button (button)
  (gamekit:bind-button button :pressed ;Add each cycle
                       (lambda ()
                         (push button *key-bag*)))
  (gamekit:bind-button button :released ;Clean up each cycle
                       (lambda ()
                         (setf *key-bag* (delete button *key-bag*)))))


(defun direction-vector (&optional (scale *snake-size*))
  (mult
   (case *snake-face*
     (:up (vec2 0 1))
     (:down (vec2 0 -1))
     (:right (vec2 1 0))
     (:left (vec2 -1 0))
     (:space (vec2 0 0)))
   scale))

(defun boundary-transport (vec)
  (cond ((>= (x vec) *width*)
         (add vec (vec2 (- *width*) 0)))
        ((>= (y vec) *heigth*)
         (add vec (vec2 0 (- *heigth*))))
        ((< (x vec) 0)
         (add vec (vec2 *width* 0)))
        ((< (y vec) 0)
         (add vec (vec2 0 *heigth*)))
        (t vec)))


(defun update-head (head)
  (boundary-transport (add head (direction-vector))))


(defun update-snake (snake)
  (let ((head (car snake))) ;; butlast function very useful!
    (cons (update-head head)
          (cons head (butlast snake 2)))))


(defun invalid-move (new-key old-key)
  (loop for (k1 k2) in '((:up :down) (:left :right)
                         (:down :up) (:right :left))
          thereis (and (eq new-key k1)
                       (eq old-key k2))))

;; hypothenusa
(defun point-distance (x y)
  (sqrt (+ (* x x)
           (* y y))))


(defun vector-collision (u v &optional (range *snake-size*))
  (let* ((x1 (x u))
         (x2 (x v))
         (y1 (y u))
         (y2 (y v))
         (d (point-distance (- x2 x1)
                            (- y2 y1))))
    (< d range)))


(defun random-apple ()
  (let ((x (random (/ *width* *snake-size*)))
        (y (random (/ *heigth* *snake-size*))))
    (mult (vec2 x y) *snake-size*)))


(defun sane-random-apple ()
  ;; without collision with snake-body
  (loop with apple = (random-apple)
        while (loop for cell in *snake-body*
                    thereis (vector-collision cell apple))
        do (setq apple (random-apple))
        finally (return apple)))


(defun snake-draw ()
  (gamekit:draw-rect (add (car *snake-body*) (vec2 -2 -2))
                     (+ 4 *snake-size*) (+ 4 *snake-size*)
                     :fill-paint *snake-head*)
  (loop for cell in (cdr *snake-body*)
        do (gamekit:draw-rect
            cell
            *snake-size* *snake-size*
            :fill-paint *snake-color*)))


(defun apple-draw ()
  (gamekit:draw-rect *apple*
                     *snake-size* *snake-size*
                     :fill-paint *red*))


(defun game-over-draw ()
  (gamekit:draw-text "GAME OVER!"
                     (vec2 (/ *width* 2) (/ *heigth* 2)))
  (gamekit:draw-text "PRESS SPACE TO RESTART."
                     (vec2 (/ *width* 2) (- (/ *heigth* 2) 30))))


(defmethod gamekit:act ((this trivial-snake))
  ;; update *snake-face* by keyboards events
  (when (not *game-over*)
    (let ((new-key (car *key-bag*)))
      (when new-key
        (unless (invalid-move new-key *snake-face*)
          (setq *snake-face* (car *key-bag*)))))

    ;; update *snake-body*
    (unless (eq *snake-face* :space)
      (when (= (mod *frame-counter* *delay*) 0)
        (setf *snake-body* (update-snake *snake-body*))))

    ;; check collision with current apple
    ;; if collide: eat and generate new apple
    (let ((head (car *snake-body*)))
      (when (vector-collision *apple* head)
        ;; eat
        (setf *snake-body* (cons (update-head *apple*) *snake-body*))
        ;; generate new apple
        (setf *apple* (sane-random-apple))))

    ;; check if game over (collision with itself)
    (when (loop for cell in (cdr *snake-body*)
                thereis (vector-collision (car *snake-body*) cell))
      (setq *game-over* t)))

  (when (and *game-over* (member :space *key-bag*))
    (restart-game)
    (setq *game-over* nil))

  (incf *frame-counter*))


(defmethod gamekit:post-initialize ((this trivial-snake))
  ;; binding buttons actions to each event
  (loop for key in '(:up :down :left :right :space)
        do (add-button key))
  ;; stop events
  (loop :for key :in '(:escape :q)
        :do (gamekit:bind-button key :released #'gamekit:stop)))


(defmethod gamekit:draw ((this trivial-snake))
  (debug-draw)
  (snake-draw)
  (apple-draw)
  (when *game-over*
    (game-over-draw)))


(defun main ()
  (gamekit:start 'trivial-snake))


(eval-when (:load-toplevel :execute)
  (main))
