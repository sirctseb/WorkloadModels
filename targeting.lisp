;;; Implementation of the targeting task for the experiment

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
;; the current visibility of each button
(defvar *buttons-visible* (make-hash-table))
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
  ;; set var that shows this button is not visible
  (setf (gethash b *buttons-visible*) nil)
  ;; schedule the event for actually removing the button
  (schedule-event-relative .001 (lambda() (remove-items-from-exp-window b) (proc-display)))
)
(defun create-button (x y &optional (size *default-button-size*) &key (enemy t))
  (let ((button (add-button-to-exp-window :text "" :x x :y y :width size :height size
    ;; NOTE for some reason giving 'remove-button-after-delay directly doesn't work
    :action (lambda (button) (remove-button-after-delay button))
    :color (if enemy 'red 'green)
    )))
    ; store that button is visible
    (setf (gethash button *buttons-visible*) t)
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
    (moving *default-moving*) (real-time nil) (trace-file nil) (break-hover-miss nil) (trace nil)
    (show-motion nil) (visible t))
 

   ; set break on hover miss flag
   (setf *break-on-hover-miss* break-hover-miss)
   (reset-task)
   (reset)
   (if trace (sgp :v t) (sgp :v nil))
   (let* ((window (open-exp-window "Moving X" :visible visible :width width :height height))
          (buttons (create-buttons num-targets button-size width height))
          (returnvalue nil)
        )
      ;(setf *buttons-visible* (make-list (list-length buttons) t))
    
      (if (not (subtypep (type-of window) 'virtual-window))
         (print-warning "This example only works correctly for virtual and visible-virtual windows because the x coordinate accessor is specific to those objects.")
      
         (progn


            (install-device window)
            (start-hand-at-mouse)
            (set-cursor-position 960 600)
            (proc-display)
            ;; schedule moves if targets should move
              (schedule-periodic-event .01 #'(lambda ()
                                            ;; Virtual dialog item specific coordinate moving
                                            ;; code.  Code for real windows is different for each
                                            ;; Lisp since the x position accessor will differ.
                                            (dolist (button buttons)
  ;                                            (format t "seeing if button ~a is visible so we can move it" button)
                                              (when (gethash button *buttons-visible*)
  ;                                              (format t "it is! moving it")
                                                (when show-motion (remove-items-from-exp-window button))
                                                (when moving
                                                  (setf (x-pos button) (+ 2.8 (x-pos button)))
                                                )
                                                (when show-motion (add-items-to-exp-window button))
                                              )
                                            )
                                            (proc-display)
                                       :details "moving object"
                                       :initial-delay 0.5))
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

(define-model simple-tracking

   (sgp :needs-mouse nil :show-focus t :trace-detail high :cursor-noise t :vwt t :incremental-mouse-moves 0.01 :randomize-time nil
    :visual-movement-tolerance 0.5 :pixels-per-inch 96 :viewing-distance 96)
   (chunk-type targeting state target-x target-y cursor-diff-x cursor-diff-y target-location)
   (chunk-type friend-target x y)

   (add-dm (track isa chunk) (attend-letter isa chunk)
      (goal isa targeting state find-red-target))
  
   ;; adding this setting to the model will avoid the deleted chunk
   ;; warnings in the object tracking case.
   ;; (sgp :delete-visicon-chunks nil)

;; Rule to start searching for a target  
(P find-red-target
   =goal>
      ISA         targeting
      state       find-red-target
   ?visual>
      state       free
==>
   +visual-location>
      ISA         visual-location
      :attended   nil
      kind        OVAL
      color       black
   =goal>
      state       move-cursor
)

(P on-move
  =goal>
      ISA         targeting
      state       find-red-target
  ?visual-location>
      state       error
==>
  +visual-location>
      ISA         visual-location
      kind        OVAL
  !eval!          (dolog "failed to attend to target location in find black target~%")
  !eval!          (incf *vis-fails*)
)
(P on-move-move-cursor
  =goal>
    ISA           targeting
    state         move-cursor
  ?visual-location>
    state         error
==>
  +visual-location>
    ISA           visual-location
    kind          OVAL
  !eval!          (dolog "failed to attend to target location in move cursor~%")
  !eval!          (incf *vis-fails*)
)

;; rule to move cursor toward target
;; TODO can we somehow check that the imaginal buffer does NOT contain friend info?
;; TODO if we don't have such a check (like now), what happens? sometimes move-cursor goes
;; TODO even when friend info is there?
(P move-cursor
  =goal>
    ISA           targeting
    state         move-cursor

  =visual-location>
    ISA           visual-location
    kind          OVAL
;    screen-pos    =target-location

  ;; request to move cursor
  ;; TODO :cursor-noise should probably be enabled
  ;; TODO also, :default-target-width
  ;; TODO :incremental-mouse-moves?
  ;; TODO just look at all the parameters

  ;; make sure motor system is free
  ?manual>
    preparation   free
==>

  ;; request to move the cursor
  +manual>
    ISA           move-cursor
    ;; TODO i guess we don't need the comparison rules
    ;; TODO do we even need to move visual attention to cursor?
    ;; TODO I think a better model is,
    ;; 1. find target
    ;; 2. move mouse
    ;; 3. while manual busy, track cursor
    ;; 4. if cursor within target, click button
    loc           =visual-location
  ;; request to attend to visual object so that we can search for nearest when
  ;; distinguishing between friend and enemy targets
  ; TODO it may be better to just keep the visual-location buffer full
  ; and supply that when making the new request in check-target
  +visual>
    ISA           move-attention
    screen-pos    =visual-location
  =goal>
    state         check-target
)

;; re-scan for the nearest oval to get info about its color
(P check-target
  =goal>
    ISA           targeting
    state         check-target
  ;; wait until visual attention has been moved to target
  ?visual>
    state         free
==>
  ;; request visual location search for nearest oval (should be the same we found last time, but it should be colored now)
  +visual-location>
    ISA           visual-location
    ;; search for oval
    kind          OVAL
    ;; nearest the current location
    :nearest      current
  =goal>
    ;; move to the state where we distinguish between red and green targets
    state         distinguish-target
)

;; after a rescan of the target, check if the target is red and click it
(P distinguish-target-enemy
  =goal>
    ISA           targeting
    state         distinguish-target
  ;; wait until visual location is found
  =visual-location>
    ISA           visual-location
    ;; check for oval
    kind          OVAL
    ;; check for red (enemy)
    color         red
  ;; make sure visual is free so we can request to move attention
  ?visual>
    state         free
==>
  ;; TODO if visual location request fails?
  ;; go to click mouse state to wait for manual state to be free
  =goal>
    state         click-mouse
  ;; request attention move
  +visual>
    ISA           move-attention
    screen-pos    =visual-location

  !eval!          (format t "detected enemy, clicking~%")

  ;; increment the number of targets checked
  !eval!          (incf *check-order*)
)

;; if we are still trying to distinguish a target but it has stayed black through the mouse move,
;; then we missed it, so do another mouse move to it
(P distinguish-whiff
  =goal>
    ISA           targeting
    state         distinguish-target

  ;; wait until visual location found
  =visual-location>
    ISA           visual-location
    ;; check for oval
    kind          OVAL
    ;; check for black
    color         black

  ;; wait for mouse move to be over
  ?manual>
    state         free
==>
  ;; move to the same location
  +manual>
    isa           move-cursor
    loc           =visual-location

  ;; log that we did this
  !eval!          (incf *whiff-counter*)
  !eval!          (format t "wiffed too long, moving ~%")
  !eval!          (incf *total-whiff-counter*)
)

;; after a rescan of the target, check if the target is green and go back to finding black targets
(P distinguish-target-friend
  =goal>
    ISA           targeting
    state         distinguish-target
  ;; wait until visual location is found
  =visual-location>
    ISA           visual-location
    ;; check for oval
    kind          OVAL
    ;; check for green (friend)
    color         green
    ;; get location values
    screen-x      =sx
    screen-y      =sy
  ;; TODO make sure imaginal module is free so we can remember where friend is?
  ;; TODO if it is not free? we should probably skip the remember
==>
  ;; store location of friend target
  +imaginal>
    isa           friend-target
    x             =sx
    y             =sy
    
  ;; increment the number of times the friend target was hovered
  !eval!          (incf *friend-hovers*)
  !eval!          (format t "detected friend~%")

  ;; set the order in which the friend was checked if it hasn't been set yet
  !eval!          (when (eq -1 *friend-order*) (setf *friend-order* *check-order*))
  ;; search for black targets again
  =goal>
    state         find-red-target
)

;; after a rescan of the target, check if the target is still black and keep rescanning
(P distinguish-target-black
  =goal>
    ISA           targeting
    state         distinguish-target
  ;; wait until visual location is found
  =visual-location>
    ISA           visual-location
    ;; check for oval
    kind          OVAL
    ;; check for black
    color         black
  ;; only loop when the move is not complete
  ?manual>
    state         busy
==>
  ;; request visual location search for nearest oval (should be the same we found last time, but it should be colored now)
  +visual-location>
    ISA           visual-location
    ;; search for oval
    kind          OVAL
    ;; nearest the current location
    :nearest      current
  =goal>
    ;; move to the state where we distinguish between red and green targets
    state         distinguish-target
)

; request a mouse click
(P click-mouse
  =goal>
    ISA           targeting
    state         click-mouse

  ;; wait until we attended the target
  =visual>
    ISA           OVAL
  ;; make sure motor module is free
  ?manual>
    state         free
==>
  
  ;; submit click request
  +manual>
    ISA           click-mouse

  =goal>
    state         find-red-target
)

(P after-click
  =goal>
    ISA           targeting
    state         after-click
  ?manual>
    state         free
==>
  !stop!
)

(goal-focus goal)
)
