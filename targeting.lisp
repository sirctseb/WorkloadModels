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

  (when (and
          (eq (evt-module event) ':MOTOR)
          (eq 'FINISH-MOVEMENT (evt-action event))
          )
    (format t "finished moving or clicking mouse")
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
  (format t "removing button~%")
  ;; set var that indicates a click occured more recently than manual request finish
  (setf *button-clicked* t)
  ;; set var that shows this button is not visible
  (setf (gethash b *buttons-visible*) nil)
  ;; schedule the event for actually removing the button
  (schedule-event-relative .001 (lambda() (remove-items-from-exp-window b) (proc-display)))
)
(defun create-button (x y &optional (size *default-button-size*) &key (enemy t))
  (let ((button (add-button-to-exp-window :text "x" :x x :y y :width size :height size
    ;; NOTE for some reason giving 'remove-button-after-delay directly doesn't work
    :action (lambda (button) (remove-button-after-delay button))
    ;; color based on eneminess
    :color (if enemy 'red 'green)
    )))
    (setf (gethash button *buttons-visible*) t)
    button
  )
)
(defun create-buttons (num &optional (size *default-button-size*) (screen-size *default-screen-size*))
  (let (buttons '())
    (dotimes (n num buttons)
      (setf buttons (cons (create-button (random screen-size) (random screen-size) size :enemy (eq (mod n 2) 0)) buttons))
    )
  )
)
(defun run-trials (&key (num-targets 3) (trials 50) (button-size 128) (screen-size 800) (moving nil) (real-time nil) (trace-file nil))
  (dotimes (n trials)
    (do-targeting num-targets :button-size button-size :screen-size screen-size :moving moving :real-time real-time :trace-file trace-file)
    )
  )
(defun dt () (do-targeting 5))
(defun reset-task ()
  (setf *hit-counter* 0)
  (setf *miss-counter* 0)
)
(defun do-targeting (&optional (num-targets 3) &key (button-size *default-button-size*) (screen-size *default-screen-size*)
    (moving *default-moving*) (real-time *default-real-time*) (trace-file nil)) ;; old style with a screen object
  
   (reset-task)
   (reset)
   (let* ((window (open-exp-window "Moving X" :visible t :width screen-size :height screen-size))
          (buttons (create-buttons num-targets button-size screen-size))
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
            (when moving
              (schedule-periodic-event .05 #'(lambda ()

                                            ;; Virtual dialog item specific coordinate moving
                                            ;; code.  Code for real windows is different for each
                                            ;; Lisp since the x position accessor will differ.
                                            (dolist (button buttons)
  ;                                            (format t "seeing if button ~a is visible so we can move it" button)
                                              (when (gethash button *buttons-visible*)
  ;                                              (format t "it is! moving it")
                                                (format t "moving target at ~a~%" (get-time))
                                                (remove-items-from-exp-window button)
                                                (setf (x-pos button) (+ 1 (x-pos button)))
                                                (add-items-to-exp-window button)

                                              )
                                            )
                                            (proc-display)
                                       :details "moving object"
                                       :initial-delay 0.5))
            )
            (cwd "/Users/sirc/Desktop/addition")
            (open-log-file)
            (if trace-file
              (with-open-file (*standard-output* trace-file :direction :output :if-exists :supersede)
                (run 10 :real-time real-time)
              )
              (run 10 :real-time real-time)
            )
            (dolog "hits: ~a~%" `(,*hit-counter*))
            (dolog "misses: ~a~%" `(,*miss-counter*))
            (close-log-file)
            ))))

(clear-all)

; add event hook
(setf *hook-handle* (add-pre-event-hook 'hook))

(define-model simple-tracking

   (sgp :v t :needs-mouse nil :show-focus t :trace-detail high :cursor-noise nil :vwt t :incremental-mouse-moves t)
   (chunk-type targeting state target-x target-y cursor-diff-x cursor-diff-y target-location)

   (add-dm (track isa chunk) (attend-letter isa chunk)
      (goal isa targeting state find-target))
  
   ;; adding this setting to the model will avoid the deleted chunk
   ;; warnings in the object tracking case.
   ;; (sgp :delete-visicon-chunks nil)

;; Rule to start searching for a target  
(P find-target
   =goal>
      ISA         targeting
      state       find-target
   ?visual>
      state       free
==>
   +visual-location>
      ISA         visual-location
;      :attended   nil
      kind        OVAL
      color       red
   =goal>
      state       move-cursor
)

(P on-move
  =goal>
      ISA         targeting
      state       find-target
  ?visual>
      state       error
==>
  +visual>
      ISA         clear
  !eval!          (dolog "failed to attend to target location~%")
)

;; rule to move cursor toward target
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
    state         free
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
  !eval!        (setf *move-last* t)
  =goal>
    state         click-mouse
)

; request a mouse click
(P click-mouse
  =goal>
    ISA           targeting
    state         click-mouse

  ;; make sure motor module is free
  ?manual>
    state         free
==>
  
  ;; submit click request
  +manual>
    ISA           click-mouse
  !eval!        (setf *move-last* nil)

  =goal>
    state         find-target
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
