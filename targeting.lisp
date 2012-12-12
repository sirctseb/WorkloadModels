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
   (chunk-type targeting state)

   (add-dm (track isa chunk) (attend-letter isa chunk)
      (goal isa targeting state track))
  
   ;; adding this setting to the model will avoid the deleted chunk
   ;; warnings in the object tracking case.
   ;; (sgp :delete-visicon-chunks nil)
  
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
      state       attend-letter
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

(P found-letter
   =goal>
      ISA         targeting
      state       attend-letter
   =visual-location>
      ISA         visual-location
   
   ?visual>
      state        free
   
==>
   +visual>
      ISA         move-attention
      screen-pos  =visual-location
   =goal>
      state       find-target
   ;; maintain visual location info
   =visual-location>
      ISA         visual-location
)

(goal-focus goal)
)
