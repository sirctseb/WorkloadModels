;;; Implementation of the targeting task for the experiment

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
  )
)

(defun remove-button-after-delay (b)
  ;; TODO it seems that act-r crashes when you remove a button in its own
  ;; action, so instead, schedule an event to remove the button in 0.01ms
  (format t "removing button")
  (schedule-event-relative .001 (lambda() (remove-items-from-exp-window b)))
)
(defun create-button (x y)
  (add-button-to-exp-window :text "x" :x x :y y :width 40 :height 40
    ;; NOTE for some reason giving 'remove-button-after-delay directly doesn't work
    :action (lambda (button) (remove-button-after-delay button))
  )
)
(defun create-buttons (num)
  (let (buttons '())
    (dotimes (n num buttons)
      (cons (create-button (random 400) (random 400)) buttons)
    )
  )
)

(defun do-targeting (&optional (num-targets 1)) ;; old style with a screen object
  
   (reset)
   (let* ((window (open-exp-window "Moving X" :visible t :width 400 :height 400))
          (buttons (create-buttons num-targets))
        )
    
      (if (not (subtypep (type-of window) 'virtual-window))
         (print-warning "This example only works correctly for virtual and visible-virtual windows because the x coordinate accessor is specific to those objects.")
      
         (progn


            (install-device window)
            (start-hand-at-mouse)
            (set-cursor-position 20 30)
            (proc-display)
            ;(schedule-periodic-event .5 #'(lambda () 
            ;                             
            ;                            ;; Virtual dialog item specific coordinate moving
            ;                            ;; code.  Code for real windows is different for each
            ;                            ;; Lisp since the x position accessor will differ.
            ;                            
            ;                            (setf (x-pos button) (+ 10 (x-pos button)))
            ;                            
            ;                            (proc-display))
            ;                       :details "moving object"
            ;                       :initial-delay 0.5)
        
            (run 3 )))))

(clear-all)

; add event hook
(add-pre-event-hook 'hook)

(define-model simple-tracking

   (sgp :v t :needs-mouse nil :show-focus t :trace-detail high)
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
      :attended   nil
      kind        TEXT
      ;; TODO does this do anything?
      value       "x"
   =goal>
      state       attend-target
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
)

;; rule to register a location and ask to attend to it
(P found-target
   =goal>
      ISA         targeting
      state       attend-target
   =visual-location>
      ISA         visual-location
      screen-x    =sx
   
   ?visual>
      state        free
   
==>
   +visual>
      ISA         move-attention
      screen-pos  =visual-location
   =goal>
      state       move-cursor
   ;; maintain visual location info
;   =visual-location>
;      screen-x    =sx
)

;; rule to move cursor toward target
(P move-cursor
  =goal>
    ISA           targeting
    state         move-cursor

  =visual>
    ISA           text
    value         "x"
    screen-pos    =target-location

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
    loc           =target-location
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
