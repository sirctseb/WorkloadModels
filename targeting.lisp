;;; Implementation of the targeting task for the experiment

;;; Examples of visual tracking using the old style with an object
;;; based visicon (the currently provided devices) and with a custom
;;; device that uses a chunk based visicon (without having explicit
;;; objects set for the items).

(defun do-targeting () ;; old style with a screen object
  
   (reset)
   (let* ((window (open-exp-window "Moving X" :visible t))
          (letter (add-text-to-exp-window :text "x" :x 10 :y 150)))
    
      (if (not (subtypep (type-of window) 'virtual-window))
         (print-warning "This example only works correctly for virtual and visible-virtual windows because the x coordinate accessor is specific to those objects.")
      
         (progn
            (install-device window)
            (proc-display)
            (schedule-periodic-event .5 #'(lambda () 
                                         
                                        ;; Virtual dialog item specific coordinate moving
                                        ;; code.  Code for real windows is different for each
                                        ;; Lisp since the x position accessor will differ.
                                        
                                        (setf (x-pos letter) (+ 10 (x-pos letter)))
                                        
                                        (proc-display))
                                   :details "moving object"
                                   :initial-delay 0.5)
        
            (run 3 :real-time t)))))

(clear-all)

(define-model simple-tracking

   (sgp :v t :needs-mouse nil :show-focus t :trace-detail high)
   (chunk-type targeting state target-x target-y)

   (add-dm (track isa chunk) (attend-letter isa chunk)
      (goal isa targeting state track))
  
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
      ISA         visual-location
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
    attended    :nil
    ;; TODO look near old remembered cursor location?
  =goal>
    state       found-cursor
)

(goal-focus goal)
)
