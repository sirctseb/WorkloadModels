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
;; the number of times the visual-location request had an error
(defvar *vis-fails* 0)
;; true if we should stop running when we encounter a miss after a hover
(defvar *break-on-hover-miss* nil)

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
        (incf *hit-counter*)
        )
        (progn
        (dolog "missed a target at ~a ~%" `(,(evt-time event)))
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
    ; make buttons black initially
    ;; TODO condition on hardness
    :color 'black
    )))
    ; store buttons real color
    (setf (gethash button *button-colors*) (if enemy 'red 'green))
    ; store that button is visible
    (setf (gethash button *buttons-visible*) t)
    button
  )
)
(defun create-buttons (num &optional (size *default-button-size*) (screen-size *default-screen-size*))
  (let (buttons '())
    (dotimes (n num buttons)
      (setf buttons (cons (create-button (random 100) (random 1200) size :enemy (eq (mod n 2) 0)) buttons))
    )
  )
)
;; condition function for stopping simulation:
;; stop once we have hit two targets or 6 seconds have passed
;; TODO this does not check for friendly hits
(defun task-end-condition ()
  (or (> *hit-counter* 1) (> (get-time) 6000))
)
(defun run-trials (&key (num-targets 3) (trials 50) (button-size 128) (screen-size 800) (moving nil) (real-time nil) (trace-file nil) (break-hover-miss nil) (trace nil))
  (dotimes (n trials)
    (when 
      (and
        (third (do-targeting num-targets :button-size button-size :screen-size screen-size :moving moving :real-time real-time :trace-file trace-file :break-hover-miss break-hover-miss :trace trace)
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
  (setf *vis-fails* 0)
)
(defun do-targeting (&optional (num-targets 3) &key (button-size *default-button-size*) (screen-size *default-screen-size*)
    (moving *default-moving*) (real-time *default-real-time*) (trace-file nil) (break-hover-miss nil) (trace nil)) ;; old style with a screen object
 

   ; set break on hover miss flag
   (setf *break-on-hover-miss* break-hover-miss)
   (reset-task)
   (reset)
   (if trace (sgp :v t) (sgp :v nil))
   (let* ((window (open-exp-window "Moving X" :visible t :width 1920 :height 1200))
          (buttons (create-buttons num-targets button-size screen-size))
          (returnvalue nil)
        )
      ;(setf *buttons-visible* (make-list (list-length buttons) t))
    
      (if (not (subtypep (type-of window) 'virtual-window))
         (print-warning "This example only works correctly for virtual and visible-virtual windows because the x coordinate accessor is specific to those objects.")
      
         (progn


            (install-device window)
            (start-hand-at-mouse)
            (set-cursor-position 20 30)
            (proc-display)
            ;; schedule moves if targets should move
              (schedule-periodic-event .01 #'(lambda ()
                                            ;; Virtual dialog item specific coordinate moving
                                            ;; code.  Code for real windows is different for each
                                            ;; Lisp since the x position accessor will differ.
                                            (let ((color-count 0))
                                              (dolist (button buttons)
    ;                                            (format t "seeing if button ~a is visible so we can move it" button)
                                                (when (gethash button *buttons-visible*)
    ;                                              (format t "it is! moving it")
                                                  ;(remove-items-from-exp-window button)
                                                  (when moving
                                                    (setf (x-pos button) (+ 2.8 (x-pos button)))
                                                  )
                                                  ;(add-items-to-exp-window button)
                                                  ;(format t "moving target at ~a to x ~d~%" (get-time) (x-pos button))
                                                  ;(format t "cursor location: ~s" (get-mouse-coordinates (current-device)))
                                                  ;; check if mouse is within target
                                                  ;; define cursor and button locations
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
                                                )
                                              )
                                              (when (> color-count 1)
                                                (dolog "two targets are colored~%")
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
      (goal isa targeting state find-black-target))
  
   ;; adding this setting to the model will avoid the deleted chunk
   ;; warnings in the object tracking case.
   ;; (sgp :delete-visicon-chunks nil)

;; Rule to start searching for a target  
(P find-black-target
   =goal>
      ISA         targeting
      state       find-black-target
   ?visual>
      state       free
==>
   +visual-location>
      ISA         visual-location
      :attended   nil
      kind        OVAL
      color       black
   =goal>
      state       cap-first-location
)

;; Rule to capture the location of a target
(P cap-first-location
  =goal>
    ISA           targeting
    state         cap-first-location

  ;; find vis loc
  =visual-location>
    ISA           visual-location
    screen-x      =tx
    screen-y      =ty

  ;; make sure visual is free so we can request move-attention
  ?visual>
    state         free
==>
  ;; store location in goal
  =goal>
    target-x      =tx
    target-y      =ty
    state         lead-target

  ;; move attention to location
  +visual>
    ISA           move-attention
    screen-pos    =visual-location

  ;; reset timer
  ;; TODO we could do this in find-black-target and get 0.005 or so more on the timer
  +temporal>
    ISA           time
)

;; Rule to capture second location of the target after moving attention
(P lead-target
  =goal>
    ISA           targeting
    state         lead-target

  ;; wait until attention is moved to target
  =visual>
    ISA           OVAL
    screen-pos    =sp

==>
  ;; keep the contents of the visual buffer
  =visual>
    screen-pos    =sp

  ;; update the visual location buffer to the newest location values
  +visual-location>
    ISA           visual-location
    :nearest      current
  ;; change state to get the new vis-loc
  =goal>
    state         lead-target-2
)
(P lead-target-2
  =goal>
    ISA           targeting
    state         lead-target-2
    target-x      =tx
    target-y      =ty
  ;; get the new location
  =visual-location>
    ISA           visual-location
    screen-x      =sx
    screen-y      =sy
==>
  ;; calculate x difference
  !bind!          =x-diff (- =sx =tx)
  !bind!          =y-diff (- =sy =ty)
  !eval!          (format t "x: ~a, y: ~a~%" =x-diff =y-diff)
)

(P on-move
  =goal>
      ISA         targeting
      state       find-black-target
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

;; rule to check the visual location against a remembered
;; location of a friend target and go to a different one
(P avoid-friend
  =goal>
    ISA           targeting
    state         move-cursor

  ;; check for friend info in the imaginal buffer
  =imaginal>
    ISA           friend-target
    ;; get friend location
    x             =fx
    y             =fy

  =visual-location>
    ISA           visual-location
    ;; check if new location is at the remembered friend location
    screen-x      =fx
    screen-y      =fy

  ;; make sure visual is free so we can move attention
  ?visual>
    state         free
==>
  =goal>
    state         find-black-target

  ;; move attention to friend target so that find-target-black searches for the other
  +visual>
    isa           move-attention
    screen-pos    =visual-location

  ;; prevent imaginal buffer from being harvested by setting it to the same values
  ;; TODO an alternative is to attempt to retrive the friend-target chunk from declarative
  ;; TODO if it's not in the imaginal buffer. that may be more robust in dual-task cases because
  ;; TODO something else might fill the imaginal buffer and then we'll never get it back
  ;; NOTE this is different than +imaginal> x =fx which makes the imaginal module busy while it sets the value
  =imaginal>
    x             =fx
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
  ;; search for black targets again
  =goal>
    state         find-black-target
  ;; increment the number of times the friend target was hovered
  !eval!          (incf *friend-hovers*)
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
    state         find-black-target
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
