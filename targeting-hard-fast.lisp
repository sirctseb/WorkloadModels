;;; Implementation of the targeting task for the experiment in the hard - fast condition
(define-model targeting-hard-fast

  ;; sgp section
  (sgp :needs-mouse nil
    :show-focus t
    :trace-detail high
    :cursor-noise t
    :vwt t
    :incremental-mouse-moves 0.01
    :randomize-time nil
    :visual-movement-tolerance 10
    :pixels-per-inch 96
    :viewing-distance 96)

  ;; chunk types
  (chunk-type targeting state target-x target-y projected-x projected-y)
  (chunk-type friend-target x y x-diff y-diff)

  ;; dms
  (suppress-warnings
    (add-dm (track isa chunk) (attend-letter isa chunk)
      (goal isa targeting state find-black-target))
    )

  ;; goal focus
  (goal-focus goal)

  ;; Productions

  ;; Rule to start searching for a target  
  (P find-black-target
    =goal>
      ISA         targeting
      state       find-black-target
  ==>
    +visual-location>
      ISA         visual-location
      :attended   nil
      kind        OVAL
      color       black
    =goal>
      state       cap-first-location
    ;; reset timer
    ;; TODO we could do this in find-black-target and get 0.005 or so more on the timer
    +temporal>
      ISA           time
  )

  ;; rule to check the visual location against a remembered
  ;; location of a friend target and go to a different one
  (P avoid-friend
    =goal>
      ISA           targeting
      state         cap-first-location

    ;; check for friend info in the imaginal buffer
    =imaginal>
      ISA           friend-target
      ;; get friend location and motion
      x             =fx
      y             =fy
      x-diff        =x-diff
      y-diff        =y-diff
    ?imaginal>
      state         free

    =visual-location>
      ISA           visual-location
      ;; check if new location is at the remembered friend location
      screen-x      =tx
      screen-y      =ty

    ;; test if target is on friend line
    !bind!          =on-line (is-on-line =tx =ty =fx =fy =x-diff =y-diff)

  ==>
    =goal>
      state         find-black-target

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

  ;; Rule to capture the location of a target when there is no friend info
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

    ;; check that there is nothing in imaginal
    ?imaginal>
      buffer        empty
      state         free
  ==>
    ;; store location in goal
    =goal>
      target-x      =tx
      target-y      =ty
      state         lead-target

    !eval!          (format t "storing first target location: ~a, ~a~%" =tx =ty)

    ;; search for same location
    +visual-location>
      ISA           visual-location
      :nearest      =visual-location
  )

  ;; Rule to capture the location of a target when it doesn't match friend info
  ;; TODO failure to to cap-first-location when there is something in imaginal but it is
  ;; TODO not a friend-target chunk
  (P cap-first-location-no-friend
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

    ;; check that there is nothing in imaginal
    =imaginal>
      isa           friend-target
      x             =fx
      y             =fy
      x-diff        =x-diff
      y-diff        =y-diff
      
    ?imaginal>
      state         free

    ;; determine that target is not friend
    !bind!          =on-line (not (is-on-line =tx =ty =fx =fy =x-diff =y-diff))
  ==>
    ;; keep imaginal
    =imaginal>
    ;; store location in goal
    =goal>
      target-x      =tx
      target-y      =ty
      state         lead-target

    !eval!          (format t "storing first target location: ~a, ~a~%" =tx =ty)

    ;; search for same location
    +visual-location>
      ISA           visual-location
      :nearest      =visual-location
  )

  ;; Rule to capture second location of the target after moving attention
  (P lead-target
    =goal>
      ISA           targeting
      state         lead-target
      target-x      =tx
      target-y      =ty
    ;; get the new location
    =visual-location>
      ISA           visual-location
      screen-x      =sx
      screen-y      =sy
    ;; get elapsed time
    =temporal>
      ISA           time
      ticks         =elapsed-ticks
  ==>
    !eval!          (format t "second target location: ~a, ~a~%" =sx =sy)
    ;; calculate x difference
    !bind!          =x-diff (- =sx =tx)
    !bind!          =y-diff (- =sy =ty)
    ;; project location
    !bind!          =projected-x (+ =tx (* *target-projection* (/ =x-diff =elapsed-ticks)))
    !bind!          =projected-y (+ =ty (* *target-projection* (/ =y-diff =elapsed-ticks)))
    !eval!          (format t "x-diff: ~a~%" =x-diff)
    !eval!          (format t "speed: ~a~%" (/ =x-diff =elapsed-ticks))
    !eval!          (format t "projecting move from ~a to ~a by ~a ~%" =tx =projected-x (* *target-projection* (/ =x-diff =elapsed-ticks)))
    !eval!          (format t "projecting at x: ~a y: ~a, ticks: ~a~%" =projected-x =projected-y =elapsed-ticks)
    ;; store projected location in visual location buffer
    =visual-location>
      screen-x      =projected-x
      screen-y      =projected-y
    ;; and move to next state
    ;; TODO move move request here to speed up?
    =goal>
      state         move-cursor

    ;; clear timer
    +temporal>
      ISA           clear
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


  ;; rule to move cursor toward target
  (P move-cursor
    =goal>
      ISA           targeting
      state         move-cursor

    =visual-location>
      ISA           visual-location
      kind          OVAL

    ;; make sure motor system is free
    ?manual>
      preparation   free

    ;; make sure visual is free
    ?visual>
      state         free
  ==>

    ;; request to move the cursor
    +manual>
      ISA           move-cursor
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

  ;; prepare a click while checking the target
  (P prepare-click
    =goal>
      ISA           targeting
      state         distinguish-target

    ;; wait until manual preparation is free and last command was a move (we didn't already prepare click)
    ?manual>
      last-command  move-cursor
      preparation   free
  ==>
    ;; prepare the mouse-click
    ;; TODO there is no prepare mouse-click, hopefully manually doing the punch right index works
    +manual>
      ISA           prepare
      style         punch
      hand          right
      finger        index
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

    ;; let prepare-click go first
    ;; TODO this is not a semantic test. it only exists to allow prepare-click to go first
    ;; TODO there should be a better way to let prepare-click to have priority
    ;; TODO we could just put a flag in goal
    ?manual>
      last-command  prepare
      state         free

    ;; wait for visual to be free so we can clear it
    ?visual>
      state         free
  ==>
    =goal>
      state         find-black-target

    ;; submit click request
    +manual>
      ;ISA           click-mouse
      ISA           execute

    ;; clear visual buffer so that it doesn't keep re-encoding and slowing down future searches
    +visual>
      ISA           clear

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

    ?temporal>
      buffer        empty

    ;; wait for mouse move to be over
    ?manual>
      state         free

    ;; wait until visual location found
    =visual-location>
      ISA           visual-location
      ;; check for oval
      kind          OVAL
      ;; check for black
      color         black
  ==>
    ;; wait to see if target moves under cursor
    +temporal>
      ISA           time

    ;; request visual location search for nearest oval (should be the same we found last time, but it should be colored now)
    +visual-location>
      ISA           visual-location
      ;; search for oval
      kind          OVAL
      ;; nearest the current location
      :nearest      current

    ;; log that we did this
    ;; TODO we should do this in move-after-whiff to only count when whiffs really happen
    !eval!          (incf *whiff-counter*)
    !eval!          (format t "detected whiff~%")
  )

  ;; if we got a whiff, but we haven't bailed yet, keep scanning 
  (P whiff-spin
    =goal>
      ISA           targeting
      state         distinguish-target

    ;; check that the timer exists but time is not up yet
    !bind!          =whiff-wait-time *whiff-wait-time*
    =temporal>
      ISA           time
      <= ticks      =whiff-wait-time

    ;; check that target is still black
    =visual-location>
      ISA           visual-location
      kind          OVAL
      color         black
  ==>
    ;; recan vis-loc
    +visual-location>
      ISA           visual-location
      :nearest      current
  )

  (P move-after-whiff
    =goal>
      ISA           targeting
      state         distinguish-target

    ;; wait until 5 ticks have gone by
    !bind!          =whiff-wait-time *whiff-wait-time*
    =temporal>
      ISA           time
      > ticks       =whiff-wait-time
  ==>
    =goal>
      state         find-black-target
    ;; clear temporal
    +temporal>
      isa           clear
    !eval!          (format t "wiffed too long, moving ~%")
    !eval!          (incf *total-whiff-counter*)
  )

  ;; detect friend when we've already seen it
  (P distinguish-target-friend-remembered
    =goal>
      ISA           targeting
      state         distinguish-target
    =visual-location>
      ISA           visual-location
      ;; wait until visual location is found
      ;; check for oval
      kind            OVAL
      ;; check for green
      color           green
    =imaginal>
      ISA           friend-target
  ==>
    ;; keep imaginal
    =imaginal>
    ;; go back to finding black target
    =goal>
      state         find-black-target
  )
  ;; after a rescan of the target, check if the target is green
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

    ;; only do the remembering if imaginal is empty
    ?imaginal>
      buffer        empty
  ==>
    ;; store location of friend target
    +imaginal>
      isa           friend-target
      x             =sx
      y             =sy
    ;; scan for same location
    +visual-location>
      ISA           visual-location
      :nearest      =visual-location
    ;; remember motion
    =goal>
      state         remember-friend-motion
    ;; increment the number of times the friend target was hovered
    !eval!          (incf *friend-hovers*)
    !eval!          (format t "detected friend~%")

    ;; set the order in which the friend was checked if it hasn't been set yet
    !eval!          (when (eq -1 *friend-order*) (setf *friend-order* *check-order*))
  )
  ;; get motion of friend target and store
  (P remember-friend-motion
    =goal>
      ISA           targeting
      state         remember-friend-motion

    ;; wait until imaginal is ready
    ?imaginal>
      state         free

    ;; get current imaginal contents
    =imaginal>
      isa           friend-target
      x             =fx
      y             =fy

    ;; get new vis-loc
    =visual-location>
      isa           visual-location
      ;; check for oval
      kind          OVAL
      ;; TODO check for friend?
      ;color        green
      ;; get location values
      screen-x      =sx
      screen-y      =sy
  ==>
    ;; compute motion params
    !bind!          =x-diff (- =sx =fx)
    !bind!          =y-diff (- =sy =fy)

    ;; store motion of friend target
    +imaginal>
      ISA           friend-target
      x             =fx
      y             =fy
      x-diff        =x-diff
      y-diff        =y-diff

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
    ;; let prepare-click go first
    ;; TODO this is not a semantic test. it only exists to allow prepare-click to go first
    ;; TODO there should be a better way to let prepare-click to have priority
    ;; TODO we could just put a flag in goal
    ?manual>
      last-command  prepare
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

  (P after-click
    =goal>
      ISA           targeting
      state         after-click
    ?manual>
      state         free
  ==>
    !stop!
  )
)
