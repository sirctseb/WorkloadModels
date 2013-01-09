;;; Implementation of the targeting task for the experiment

;;; Examples of visual tracking using the old style with an object
;;; based visicon (the currently provided devices) and with a custom
;;; device that uses a chunk based visicon (without having explicit
;;; objects set for the items).

(defun do-targeting () ;; old style with a screen object
  
   (reset)
   (let* ((window (open-exp-window "Moving X" :visible t))
          (letter (add-text-to-exp-window :text "x" :x 10 :y 150))
          (cursor-marker (add-text-to-exp-window :text "+" :x 20 :y 30)))
    
      (if (not (subtypep (type-of window) 'virtual-window))
         (print-warning "This example only works correctly for virtual and visible-virtual windows because the x coordinate accessor is specific to those objects.")
      
         (progn
            (install-device window)
            (start-hand-at-mouse)
            (set-cursor-position 20 30)
            (proc-display)
            (schedule-periodic-event .5 #'(lambda () 
                                         
                                        ;; Virtual dialog item specific coordinate moving
                                        ;; code.  Code for real windows is different for each
                                        ;; Lisp since the x position accessor will differ.
                                        
                                        (setf (x-pos letter) (+ 10 (x-pos letter)))
                                        
                                        (proc-display))
                                   :details "moving object"
                                   :initial-delay 0.5)
        
            (run 3 )))))

(clear-all)

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
   =visual-location>
      ISA         visual-location
==>
   +visual-location>
      ISA         visual-location
      :attended   nil
      kind        TEXT
   =goal>
      state       attend-target
   +manual>
      ISA         move-cursor
      loc         =visual-location
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
      state       remember-target
   ;; maintain visual location info
   =visual-location>
      screen-x    =sx
)

;; rule to store the location of a target
(P store-target
  =goal>
    ISA           targeting
    state         remember-target
  ;; get location from where system
  =visual-location>
    ISA          visual-location
    screen-x     =x-location
    screen-y     =y-location
  ;; make sure we are looking at a target
  ;; TODO what to do on non-target?
  =visual>
    ISA          text
    value        "x"
==>
  ;; store the target location in the goal
  =goal>
    target-x    =x-location
    target-y    =y-location
    state       find-cursor
)

;; rule to find the cursor
(P find-cursor
  =goal>
    ISA         targeting
    state       find-cursor

  ;; TODO does this have to be free?
  ?visual>
    state       free
==>
  ;; ask to look for location where cursor might be
  +visual-location>
    ISA         visual-location
    ;; TODO is this going to be unattended?
;;    :attended   nil
    ;; TODO look near old remembered cursor location?
    kind        TEXT
    value       "+"
  =goal>
    state       found-cursor
)

;; rule to register cursor location and ask to attend it
(P found-cursor
  =goal>
    ISA         targeting
    state       found-cursor

  =visual-location>
    ISA         visual-location
    screen-x    =sx

  ?visual>
    state       free
==>
  ;; request to attend location
  +visual>
    ISA         move-attention
    screen-pos  =visual-location
  ;; maintain location info
  =visual-location>
    screen-x    =sx
  =goal>
    state       compare-cursor-target
)

;; rule to compare cursor and target location
(P compare-cursor
  =goal>
    ISA         targeting
    state       compare-cursor-target
    target-x    =target-x
    target-y    =target-y

  ;; get the cursor x,y from the location
  =visual-location>
    ISA         visual-location
    screen-x    =cursor-x
    screen-y    =cursor-y

  ;; make sure we're looking at the cursor
  ;; TODO what if not?
  =visual>
    ISA         cursor

==>
  =goal>
    state       test-cursor
    ;; store difference between cursor and target
    cursor-diff-x   (- target-x cursor-x)
    cursor-diff-y   (- target-y cursor-y)
)

;; perform the actual test to see if the cursor is close enough to the target
(P test-cursor-click
  =goal>
    ISA             targeting
    state           test-cursor
    cursor-diff-x   <10
    cursor-diff-y   <10
==>
  =goal>
    state           click-mouse
)
;; test rule for when cursor is not close enough
(P test-cursor-move
  =goal>
    ISA             targeting
    state           test-cursor
    cursor-diff-x   >=10
    cursor-diff-y   >=10
==>
  =goal>
    state           move-cursor
)

;; rule to move cursor toward target
(P move-cursor
  =goal>
    ISA           targeting
    state         move-cursor
    cursor-diff-x =cdx
    cursor-diff-y =cdy
    target-location =target-location

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
    state         wait-for-move
)

;; wait for the manual system to be free, meaning the mouse move is complete,
;; then check if it is close enough to the target
(P wait-for-move
  =goal>
    ISA           targeting
    state         wait-for-move

  ;; wait for motor system to be free
  ?manual>
    state         free
==>
  =goal>
    state         find-cursor
)

(goal-focus goal)
)
