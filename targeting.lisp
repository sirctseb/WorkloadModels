;; true iff the last request to the manual system was a move
(defvar *move-last* nil)
;; true iff a button was clicked more recently than a manual request finished
(defvar *button-clicked* nil)
;; log file
(defvar *log-file* nil)
;; hook handle
(defvar *hook-handle*)
;; the default screen size
(defvar *default-screen-size* 400)
;; the default button size
(defvar *default-button-size* 40)
;; the underlying colors of each button
(defvar *button-colors* (make-hash-table))
;; the number of targets hit
(defvar *hit-counter* 0)
;; the number of misses
(defvar *miss-counter* 0)
;; true if the targets should move by default
(defvar *default-moving* nil)
;; true if the model should run in real time by default
(defvar *real-time* nil)
;; the number of times the friend target was hovered
(defvar *friend-hovers* 0)
;; the number of times the cursor never even got onto the target we were moving to
(defvar *whiff-counter* 0)
;; the number of times we whiff so bad that we actually bail out on the target and re-search
(defvar *total-whiff-counter* 0)
;; the number of times the visual-location request had an error
(defvar *vis-fails* 0)
;; true if we should stop running when we encounter a miss after a hover
(defvar *break-on-hover-miss* nil)
;; the number of times we avoid a friend
(defvar *friend-avoids* 0)
;; the first time the friend was hovered 
(defvar *friend-order* -1)
;; the current number of targets checked, used for calculating friend-order
(defvar *check-order* 0)
;; the target projection factor
(defparameter *target-projection* 57)
;; the number of ticks to wait after whiffing to give up on a target
(defparameter *whiff-wait-time* 20)

(defun open-log-file ()
  (unless *log-file*
    (setf *log-file* (open "log.txt" :direction :output :if-exists :append
      :if-does-not-exist :create))
    )
    (dolog "time: ~a~%" (list (get-universal-time)))
  )
(defun close-log-file()
  (when *log-file*
    (close *log-file*)
    (setf *log-file* nil)
    ))
(defun dolog (msg &optional args)
  (when *log-file*
    (if args
      ;; if args are supplied, call format with them
      (apply 'format (cons *log-file* (cons msg args)))
      ;; otherwise, just call with the message string
      (format *log-file* msg)
      )
    )
)

;; declar variable for cursor marker so we can access it in the hook

(defun print-event-info (event)
      (format t "Hook sees event with time: ~S~%" (evt-time event))
      (format t "Hook sees event with action: ~S~%" (evt-action event))
      (format t "Hook sees event with params: ~S~%" (evt-params event))
      (format t "Hook sees event with model: ~S~%" (evt-model event))
      (format t "Hook sees event with module: ~S~%" (evt-module event))
      (format t "Hook sees event with destination: ~S~%" (evt-destination event))
      (format t "Hook sees event with details: ~S~%" (evt-details event))
      (format t "Hook sees event with output: ~S~%" (evt-output event))
)

;;; Event hook function
(defun hook (event)
  (when (eq (evt-module event) ':MOTOR)
    (when (eq 'OUTPUT-KEY (evt-action event))
      (setf *move-last* nil)
    )
    (when (eq 'MOVE-CURSOR-ABSOLUTE (evt-action event))
      (setf *move-last* t))
  )
  (when (and
          (eq (evt-module event) ':MOTOR)
          (eq 'FINISH-MOVEMENT (evt-action event))
          )
    ;; if we are finishing a click, check if a button was dismissed
    ;; since the last movement finish.
    (unless *move-last*
      (if *button-clicked*
        (progn
        (dolog "hit a target at ~a ~%" `(,(evt-time event)))
        (format t "hit target~%")
        (incf *hit-counter*)
        )
        (progn
        (dolog "missed a target at ~a ~%" `(,(evt-time event)))
        (format t "missed target ~%")
        (incf *miss-counter*)
        (when (and *break-on-hover-miss* (> *friend-hovers* 0)) (schedule-break-relative 0.001 :details "stopping after miss"))
        )
      )
    )
    ;; unset click var
    (setf *button-clicked* nil)
  )
)

(defun remove-button-after-delay (b)
  ;; TODO it seems that act-r crashes when you remove a button in its own
  ;; action, so instead, schedule an event to remove the button in 0.01ms
  ;; set var that indicates a click occured more recently than manual request finish
  (setf *button-clicked* t)
  ;; schedule the event for actually removing the button
  (schedule-event-relative .001 (lambda() (remove-items-from-exp-window b) (proc-display)))
)
(defun create-button (x y &optional (size *default-button-size*) &key (enemy t))
  (let ((button (add-button-to-exp-window :text "" :x x :y y :width size :height size
    ;; NOTE for some reason giving 'remove-button-after-delay directly doesn't work
    :action (lambda (button) (remove-button-after-delay button))
    ; make buttons black initially
    ;; TODO condition on hardness
    :color 'black
    )))
    ; store buttons real color
    (setf (gethash button *button-colors*) (if enemy 'red 'green))
    button
  )
)
(defun create-buttons (num &optional (size *default-button-size*) (width *default-screen-size*) (height *default-screen-size*))
  (let (buttons '())
    (dotimes (n num buttons)
      (setf buttons (cons (create-button (random 100) (+ (* n (round (/ height 3))) (random (- (round (/ height 3)) 150))) size :enemy (eq (mod n 2) 0)) buttons))
    )
  )
)
;; condition function for stopping simulation:
;; stop once we have hit two targets or 6 seconds have passed
;; TODO this does not check for friendly hits
(defun task-end-condition ()
  (or (> *hit-counter* 1) (> (get-time) 6000))
)
(defun once (&key (num-targets 3) (trials 1) (button-size 128) (width 1920) (height 1200) (moving t) (real-time t) (trace-file nil) (break-hover-miss nil) (trace nil) (show-motion t)) 
  (run-trials :num-targets num-targets :trials trials :button-size button-size :width width :height height :moving moving :real-time real-time :trace-file trace-file :break-hover-miss break-hover-miss :trace trace :show-motion show-motion)
)
(defun run-trials (&key (num-targets 3) (trials 50) (button-size 128) (width 1920) (height 1200)
                        (moving nil) (real-time nil) (trace-file nil) (break-hover-miss nil)
                        (trace nil) (show-motion nil) (visible t))
  (dotimes (n trials)
    (when 
      (and
        (third (do-targeting num-targets :button-size button-size :width width :height height :moving moving :real-time real-time :trace-file trace-file :break-hover-miss break-hover-miss :trace trace :show-motion show-motion :visible visible)
          )
        (> *friend-hovers* 0)
        )
      (return)
    )
  )
  )
(defun dt () (do-targeting 5))
(defun reset-task ()
  (setf *hit-counter* 0)
  (setf *miss-counter* 0)
  (setf *friend-hovers* 0)
  (setf *whiff-counter* 0)
  (setf *total-whiff-counter* 0)
  (setf *vis-fails* 0)
  (setf *friend-avoids* 0)
  (setf *friend-order* -1)
  (setf *check-order* 0)
)
(defun do-targeting (&optional (num-targets 3) &key (button-size *default-button-size*) (width 1920) (height 1200)
    (moving *default-moving*) (difficult t) (real-time nil) (trace-file nil) (break-hover-miss nil) (trace nil)
    (show-motion nil) (visible t))

	;; load model based on params, but only if there is not already a model
	(unless (current-model)
		(load (concatenate 'string "targeting-" (if difficult "hard" "easy") "-" (if moving "fast" "slow") ".lisp"))
	)

   ; set break on hover miss flag
   (setf *break-on-hover-miss* break-hover-miss)
   (reset-task)
   (reset)
   (if trace (sgp :v t) (sgp :v nil))
   (let* ((window (open-exp-window "Moving X" :visible visible :width width :height height))
          (buttons (create-buttons num-targets button-size width height))
          (returnvalue nil)
        )
    
      (if (not (subtypep (type-of window) 'virtual-window))
         (print-warning "This example only works correctly for virtual and visible-virtual windows because the x coordinate accessor is specific to those objects.")
      
         (progn


            (install-device window)
            (start-hand-at-mouse)
            (set-cursor-position 960 600)
            (proc-display)
            ;; schedule moves if targets should move or we need to change target colors
            (when (or moving difficult)
              (schedule-periodic-event .01 #'(lambda ()
                                            ;; Virtual dialog item specific coordinate moving
                                            ;; code.  Code for real windows is different for each
                                            ;; Lisp since the x position accessor will differ.
                                              (dolist (button buttons)
    ;                                            (format t "seeing if button ~a is visible so we can move it" button)
    ;                                              (format t "it is! moving it")
                                                  (when show-motion (remove-items-from-exp-window button))
                                                  (when moving
                                                    (setf (x-pos button) (+ 2.8 (x-pos button)))
                                                  )
                                                  (when show-motion (add-items-to-exp-window button))
                                                  ;(format t "moving target at ~a to x ~d~%" (get-time) (x-pos button))
                                                  ;(format t "cursor location: ~s" (get-mouse-coordinates (current-device)))
                                                  ;; check if mouse is within target
                                                  ;; define cursor and button locations
	                                            (let ((color-count 0))
                                                  (let* ((cursor-loc (get-mouse-coordinates (current-device)))
                                                          (cursor-x (aref cursor-loc 0))
                                                          (cursor-y (aref cursor-loc 1))
                                                          (button-x (x-pos button))
                                                          (button-y (y-pos button))
                                                          (size (width button)))
                                                    ; test if cursor within button
                                                    (if (and (> cursor-x button-x)
                                                               (< cursor-x (+ button-x size))
                                                               (> cursor-y button-y)
                                                               (< cursor-y (+ button-y size)))
                                                      ; set button color
                                                      (progn
                                                        (setf (color button) (gethash button *button-colors*))
                                                        (incf color-count)
                                                      )
                                                      (setf (color button) 'black)
                                                    )
                                                  )
	                                              (when (> color-count 1)
	                                                (dolog "two targets are colored~%")
	                                              )
	                                            )
                                              )
                                            (proc-display)
                                       :details "moving object"
                                       :initial-delay 0.5))
              )
            (cwd "/Users/sirc/Desktop/addition")
            (open-log-file)
            (if trace-file
              (with-open-file (*standard-output* trace-file :direction :output :if-exists :append :if-does-not-exist :create)
                (setf returnvalue 
                  (multiple-value-list (run-until-condition 'task-end-condition :real-time real-time)))
              )
              (setf returnvalue
                (multiple-value-list (run-until-condition 'task-end-condition :real-time real-time)))
            )
            (dolog "hits: ~a~%" `(,*hit-counter*))
            (dolog "misses: ~a~%" `(,*miss-counter*))
            (dolog "friend hovers: ~a~%" `(,*friend-hovers*))
            (dolog "completion time: ~a~%" `(,(get-time)))
            (dolog "whiffs: ~a~%" `(,*whiff-counter*))
            (dolog "vis fails: ~a~%" `(,*vis-fails*))
            (dolog "friend avoids: ~a~%" `(,*friend-avoids*))
            (dolog "friend order: ~a~%" `(,*friend-order*))
            (dolog "total-whiffs: ~a~%" `(,*total-whiff-counter*))
            (close-log-file)
            )) returnvalue ))

(clear-all)

; add event hook
(setf *hook-handle* (add-pre-event-hook 'hook))

;; function to determine if point x,y is on line defined by point x1, y1 and vector x-diff,y-diff
(defun is-on-line (x y x1 y1 x-diff y-diff)
  (let ( (left-side (* (- y y1) x-diff)) (right-side (* (- x x1) y-diff)) )
    (< (abs (- left-side right-side)) 0.1)
    )
)

