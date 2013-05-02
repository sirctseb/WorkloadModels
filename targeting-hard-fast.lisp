;;; Implementation of the targeting task for the experiment in the hard - fast condition
(define-model targeting-hard-fast

  ;; sgp section
  (sgp
    :needs-mouse nil
    :show-focus t
    :trace-detail high
    :cursor-noise t
    :vwt t
    :incremental-mouse-moves 0.01
    :randomize-time t
    :visual-movement-tolerance 10
    :pixels-per-inch 96
    :viewing-distance 96)
  (sgp
    :esc t
    :lf .2
    :le 10
    )
  (sgp :blc 0.1)
  (sgp :ans 0.05)
  (sgp :rt -.45)
  ; (sgp :rt -.45 :esc t :ans 0.05 :mp 16)
  (sgp :er t)
  ;; fast and randomized imaginal
  (sgp :imaginal-delay 0.05
       :vidt t)
  ;; we'll count this as sgp
  ;; set the default visloc chunk to something that will never match
  ;; the effect is to disable buffer stuffing
  (set-visloc-default isa visual-location color does-not-exist)
  (start-hand-at-mouse)
  (set-cursor-position 960 600)

  ;; chunk types
  (chunk-type targeting state target-x target-y projected-x projected-y target-location)
  (chunk-type friend-target x y x-diff y-diff)
  (chunk-type response color action)

  ;; dms
  (add-dm (track isa chunk) (attend-letter isa chunk)
    (enemy-response isa response color red action shoot)
    (friend-response isa response color green action oh-no-dont-shoot)
    (goal isa targeting state find-black-target))

  ;; goal focus
  (goal-focus goal)

  ;; Productions

  ;; Rule to start searching for a target  
  (P find-black-target
    =goal>
      ISA         targeting
      state       find-black-target

    ;; check for empty vis-loc
    ?visual-location>
      buffer      empty

    ;; check for empty temporal
    ?temporal>
      buffer      empty
  ==>
    ;; do visual search for black target
    +visual-location>
      ISA         visual-location
      :attended   nil
      kind        OVAL
      color       black

    ;; update goal
    =goal>
      state       cap-first-location

    ;; reset timer
    +temporal>
      ISA           time
  )

  ;; it can happen that a vis-loc is requested when the cursor is over any remaining targets
  ;; so that they are all colored and the vis-loc fails in this case we can just reset until one becomes black
  ;; TODO consider adding shortcut rule that checks if we are over red and can just click
  ;; TODO check for other times vis-loc can fail
  (P on-move
    =goal>
        ISA         targeting
        state       cap-first-location
    ?visual-location>
        state       error
  ==>
    +visual-location>
        ISA         visual-location
        kind        OVAL
        color       black
    !eval!          (dolog "failed to find target location in find black target~%")
    !eval!          (incf *vis-fails*)
  )

  ;; rule to check the visual location against a remembered
  ;; location of a friend target and go to a different one
  (P avoid-friend
    ;; check state
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
    ;; TODO do we need this check?
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

    ;; TODO this is very not greedy-polite in imaginal. if addition uses imaginal we will need to change it
    ;; prevent imaginal buffer from being harvested by setting it to the same values
    ;; TODO an alternative is to attempt to retrive the friend-target chunk from declarative
    ;; TODO if it's not in the imaginal buffer. that may be more robust in dual-task cases because
    ;; TODO something else might fill the imaginal buffer and then we'll never get it back
    ;; NOTE this is different than +imaginal> x =fx which makes the imaginal module busy while it sets the value
    =imaginal>

    ;; clear temporal because find-black-target started it
    ;; TODO this is not gp in temporal
    +temporal>
      ISA           clear

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

    ;; check that there is nothing in imaginal
    ?imaginal>
      buffer        empty
      ;; check state free so we will not let this rule fire until a pending imaginal goes through
      state         free
  ==>
    ;; store location in goal
    =goal>
      target-x      =tx
      target-y      =ty
      state         lead-target

    !eval!          (format t "storing first target location: ~a, ~a~%" =tx =ty)

    ;; TODO for gp this should be in a subsequent rule so vis-loc is empty between requests
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

    ;; check that friend info is in imaginal
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

    ;; TODO for gp this should be a separate rule so vis-loc is empty between requests
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
    ;; TODO is this a violation of gp?
    =visual-location>
      screen-x      =projected-x
      screen-y      =projected-y

    ;; and move to next state
    ;; could move move request here to speed up
    =goal>
      state         move-cursor

    ;; clear timer
    +temporal>
      ISA           clear
  )



  ;; rule to move cursor toward target
  (P move-cursor
    =goal>
      ISA           targeting
      state         move-cursor

    =visual-location>
      ISA           visual-location
      kind          OVAL
      screen-x      =x
      screen-y      =y

    ;; make sure motor system is free
    ?manual>
      preparation   free

    ;; make sure visual is free
    ; ?visual>
    ;   state         free
    ;   buffer        empty
  ==>

    ;; request to move the cursor
    +manual>
      ISA           move-cursor
      loc           =visual-location

    ;; request to attend to visual object so that we can search for nearest when
    ;; distinguishing between friend and enemy targets
    ; +visual>
    ;   ISA           move-attention
    ;   screen-pos    =visual-location
    =goal>
      state         check-target
      target-location =visual-location
      ; target-x      =x
      ; target-y      =y
  )

  ;; re-scan for the nearest oval to get info about its color
  (P check-target
    =goal>
      ISA           targeting
      state         check-target
      target-location =vis-loc
    ;; get visual location from visual buffer
    ; =visual>
    ;   isa           OVAL
    ;   screen-pos    =vis-loc
    ; ?visual>
    ;   state         free
    ?visual-location>
        buffer        empty
  ==>
    ;; request visual location search for nearest oval (should be the same we found last time, but it should be colored now)
    +visual-location>
      ISA           visual-location
      ;; search for oval
      kind          OVAL
      ;; nearest the stored location
      :nearest      =vis-loc

    ;; =visual auto harvests here, but may be re-encoded, so clear it
    ; +visual>
    ;   ISA           clear

    =goal>
      ;; move to the state where we distinguish between red and green targets
      state         distinguish-target
      ;; store the vis-loc of target of focus
      ; TODO now this is already stored
      ; target-location =vis-loc
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
    +manual>
      ISA           prepare
      style         punch
      hand          right
      finger        index
  )

  ;; prepare click after detect-target-color because it could happen right away
  ;; TODO this is slow in the case that it's needed. faster would be to have a separate decide-to-shoot
  ;; TODO that does the whole mouse click instead of just execute
  (P prepare-click-after-detect-target-color
    =goal>
      ISA           targeting
      state         decide-whether-to-shoot
    
    ;; wait until manual preparation is free and last command was a move (we didn't already prepare click)
    ?manual>
      last-command  move-cursor
      preparation   free
  ==>
    ;; prepare the mouse-click
    +manual>
      ISA           prepare
      style         punch
      hand          right
      finger        index
  )

  ;; check if there the nearest target is no longer black
  (P detect-target-color
    =goal>
      ISA           targeting
      state         distinguish-target
      
    ;; check for non-black target
    =visual-location>
      ISA         visual-location
      ;; check for oval
      kind        OVAL
      ;; check for not black
      - color     black
      ;; match color
      color       =color

    ;; wait for retrieval
    ?retrieval>
      state       free
      buffer      empty
  ==>
    ;; request lookup of action based on color
    +retrieval>
      ISA         response
      color       =color
    ;; update goal
    =goal>
      state       decide-whether-to-shoot
  )

  ;; after a rescan of the target, check if the target is red and click it
  (P decide-to-shoot
    =goal>
      ISA           targeting
      state         decide-whether-to-shoot

    ;; match shoot chunk
    =retrieval>
      ISA           response
      action        shoot

    ;; let prepare-click go first
    ;; TODO this is not a semantic test. it only exists to allow prepare-click to go first
    ;; TODO there should be a better way to let prepare-click to have priority
    ;; TODO we could just put a flag in goal
    ?manual>
      last-command  prepare
      state         free
  ==>
    =goal>
      state         find-black-target

    ;; submit click request
    +manual>
      ISA           execute

    ;; clear temporal in case we were running a whiff
    ;; TODO this is not gp in temporal
    +temporal>
      ISA           clear

    !eval!          (format t "detected enemy, clicking~%")

    ;; increment the number of targets checked
    !eval!          (incf *check-order*)
  )

  ;; TODO does this ever happen? waiting for imaginal in cap-first should prevent it
  ;; detect friend when we've already seen it
  (P decide-not-to-shoot-friend-remembered
    =goal>
      ISA           targeting
      state         decide-whether-to-shoot

    ;; match no-shoot chunk
    =retrieval>
      ISA           response
      action        oh-no-dont-shoot

    ;; check that there is already info
    =imaginal>
      ISA           friend-target
  ==>
    ;; TODO this is not gp in imaginal
    ;; keep imaginal
    =imaginal>
    ;; go back to finding black target
    =goal>
      state         find-black-target
    !eval!          (incf *friend-hovers*)
    ;; clear temporal in case we were running a whiff
    ;; TODO this is not gp in temporal
    +temporal>
      ISA           clear
  )

  ;; after a rescan of the target, check if the target is green
  (P decide-not-to-shoot
    =goal>
      ISA           targeting
      state         decide-whether-to-shoot
      target-x      =sx
      target-y      =sy
      target-location =target-location

    ;; match no-shoot chunk
    =retrieval>
      ISA           response
      action        oh-no-dont-shoot

    ;; TODO if it is not free? we should probably skip the remember
    ;; only do the remembering if imaginal is empty
    ?imaginal>
      state         free
      buffer        empty
  ==>
    ;; store location of friend target
    +imaginal>
      isa           friend-target
      x             =sx
      y             =sy

    ;; TODO this should be a separate rule for gp
    ;; scan for same location
    +visual-location>
      ISA           visual-location
      :nearest      =target-location

    ;; clear temporal in case we were running a whiff
    ;; TODO this is not gp in temporal
    +temporal>
      ISA           clear

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
    ;; TODO doing two consecutive imaginals like this is slow
    ;; TODO we could at least use the stored vis-loc we already have as one of them
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

  ;; if we are still trying to distinguish a target but it has stayed black through the mouse move,
  ;; then we missed it, so do another mouse move to it
  (P distinguish-whiff
    =goal>
      ISA           targeting
      state         distinguish-target
      ;; match target location for new search
      target-location =target-location

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

    ;; TODO this should be a separate rule for gp
    ;; request visual location search for nearest oval (should be the same we found last time, but it should be colored now)
    +visual-location>
      ISA           visual-location
      ;; search for oval
      kind          OVAL
      ;; nearest the current location
      :nearest      =target-location

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
      ;; match target location for new search
      target-location =target-location

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
    ;; TODO this should be a separate rule for gp
    ;; recan vis-loc
    +visual-location>
      ISA           visual-location
      :nearest      =target-location
  )

  (P move-after-whiff
    =goal>
      ISA           targeting
      state         distinguish-target

    ;; wait until a number of ticks have gone by
    !bind!          =whiff-wait-time *whiff-wait-time*
    =temporal>
      ISA           time
      > ticks       =whiff-wait-time

    ;; match visual-location so that it harvests
    =visual-location>
      ISA           visual-location
  ==>
    =goal>
      state         find-black-target
    ;; clear temporal
    +temporal>
      isa           clear
    !eval!          (format t "wiffed too long, moving ~%")
    !eval!          (incf *total-whiff-counter*)
  )

  ;; after a rescan of the target, check if the target is still black and keep rescanning
  (P distinguish-target-black
    =goal>
      ISA           targeting
      state         distinguish-target
      ;; match store location for new search
      target-location =target-location

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
      ;; nearest the stored location
      :nearest      =target-location

    ;; clear temporal in case we were running a whiff
    ;; TODO this is not gp in temporal
    +temporal>
      ISA           clear

    =goal>
      ;; move to the state where we distinguish between red and green targets
      state         distinguish-target
  )
) ; end model
