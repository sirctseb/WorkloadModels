;;; Implementation of the targeting task for the experiment in the hard - slow condition
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
      state       move-cursor
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

  !eval!          (format t "avoiding friend~%")
  ;; increment number of times avoided friend
  !eval!          (incf *friend-avoids*)
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
    state         find-black-target
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
