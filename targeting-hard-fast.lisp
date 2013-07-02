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
  (sgp :ans 0.08)
  (sgp :rt -.45)
  ; (sgp :rt -.45 :esc t :ans 0.05 :mp 16)
  (sgp :er t)
  ;; fast and randomized imaginal
  (sgp :imaginal-delay 0.05
       :vidt t)
  ;; fitt's law coefficient
  ; (sgp :mouse-fitts-coeff 0.15)
  ;; we'll count this as sgp
  ;; set the default visloc chunk to something that will never match
  ;; the effect is to disable buffer stuffing
  (set-visloc-default isa visual-location color does-not-exist)
  (start-hand-at-mouse)
  (set-cursor-position 960 600)

  ;; chunk types
  (chunk-type targeting
              state target-x target-y target-location
              friend-x friend-y friend-x-diff friend-y-diff
              cur-x-diff cur-y-diff ticks
              heuristic)
  (chunk-type response color action)

  ;; dms
  (add-dm (track isa chunk) (attend-letter isa chunk)
    (enemy-response isa response color red action shoot)
    (friend-response isa response color green action oh-no-dont-shoot)
    (goal isa targeting state find-black-target heuristic lowest))
  (set-base-levels (enemy-response 1) (friend-response 1))

  ;; goal focus
  (goal-focus goal)

  ;; Productions

  ;; Rule to start searching for a target  
  (P find-black-target
    =goal>
      ISA         targeting
      state       find-black-target
      ;; TODO can make this a variable and provide to search directly?
      heuristic   =heuristic

    ;; check for empty vis-loc
    ?visual-location>
      buffer      empty
  ==>
    ;; do visual search for black target
    +visual-location>
      ISA         visual-location
      :attended   nil
      kind        OVAL
      color       black
      screen-x    =heuristic

    ;; update goal
    =goal>
      state       cap-first-location
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
    =goal>
      state on-move-search
    !eval!          (dolog "failed to find target location in find black target~%")
    !eval!          (incf *vis-fails*)
  )
  (P on-move-search
    =goal>
      isa targeting
      state on-move-search
    ?visual-location>
      buffer empty
  ==>
    +visual-location>
        ISA         visual-location
        kind        OVAL
        color       black
    =goal>
      state cap-first-location
  )

  ;; rule to check the visual location against a remembered
  ;; location of a friend target and go to a different one
  (P avoid-friend-lowest
    ;; check state
    =goal>
      ISA           targeting
      state         cap-first-location
      friend-x      =fx
      friend-y      =fy
      friend-x-diff =x-diff
      friend-y-diff =y-diff
      heuristic     lowest

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
      heuristic     highest

    !eval!          (format t "avoiding friend~%")
    ;; increment number of times avoided friend
    !eval!          (incf *friend-avoids*)
  )
  (P avoid-friend-highest
    ;; check state
    =goal>
      ISA           targeting
      state         cap-first-location
      friend-x      =fx
      friend-y      =fy
      friend-x-diff =x-diff
      friend-y-diff =y-diff
      heuristic     highest

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
      heuristic     lowest

    !eval!          (format t "avoiding friend~%")
    ;; increment number of times avoided friend
    !eval!          (incf *friend-avoids*)
  )

  ;; Rule to capture the location of a target when there is no friend info
  (P cap-first-location
    =goal>
      ISA           targeting
      state         cap-first-location
      ;; test that there is no friend info
      friend-x      nil

    ;; find vis loc
    =visual-location>
      ISA           visual-location
      screen-x      =tx
      screen-y      =ty
  ==>
    ;; store location in goal
    =goal>
      target-x      =tx
      target-y      =ty
      state         cap-first-location-search
      target-location =visual-location

    !eval!          (format t "storing first target location: ~a, ~a~%" =tx =ty)
  )

  ;; Rule to capture the location of a target when it doesn't match friend info
  (P cap-first-location-no-friend
    =goal>
      ISA           targeting
      state         cap-first-location
      friend-x      =fx
      friend-y      =fy
      friend-x-diff =x-diff
      friend-y-diff =y-diff

    ;; find vis loc
    =visual-location>
      ISA           visual-location
      screen-x      =tx
      screen-y      =ty

    ;; determine that target is not friend
    !bind!          =on-line (not (is-on-line =tx =ty =fx =fy =x-diff =y-diff))
  ==>
    ;; store location in goal
    =goal>
      target-x      =tx
      target-y      =ty
      state         cap-first-location-search
      target-location =visual-location

    !eval!          (format t "storing first target location: ~a, ~a~%" =tx =ty)

  )
  (P cap-first-location-search
    =goal>
      isa targeting
      state cap-first-location-search
      target-location =target-location
    ?visual-location>
      buffer empty
  ==>
    ;; search for target again
    +visual-location>
      ISA visual-location
      kind      OVAL
      :nearest  =target-location
    =goal>
      state lead-target
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
  ==>
    !eval!          (format t "second target location: ~a, ~a~%" =sx =sy)
    ;; calculate x difference
    !bind!          =x-diff (- =sx =tx)
    !bind!          =y-diff (- =sy =ty)
    !bind!          =mag (sqrt (+ (* =x-diff =x-diff) (* =y-diff =y-diff)))
    !bind!          =x-diff-normal (/ =x-diff =mag)
    !bind!          =y-diff-normal (/ =y-diff =mag)
    ;; project location
    ; !bind!          =projected-x (+ =sx (* *target-projection* (/ =x-diff =elapsed-ticks)))
    ; !bind!          =projected-y (+ =sy (* *target-projection* (/ =y-diff =elapsed-ticks)))
    !bind!          =projected-x (+ =sx (* *target-projection* =x-diff-normal))
    !bind!          =projected-y (+ =sy (* *target-projection* =y-diff-normal))
    !eval!          (format t "x-diff: ~a~%" =x-diff)
    ; !eval!          (format t "speed: ~a~%" (/ =x-diff =elapsed-ticks))
    ; !eval!          (format t "projecting move from ~a to ~a by ~a ~%" =tx =projected-x (* *target-projection* (/ =x-diff =elapsed-ticks)))
    ; !eval!          (format t "projecting at x: ~a y: ~a, ticks: ~a~%" =projected-x =projected-y =elapsed-ticks)

    ;; store projected location in visual location buffer
    ;; violation of gp
    =visual-location>
      screen-x      =projected-x
      screen-y      =projected-y

    ;; and move to next state
    ;; could move move request here to speed up
    =goal>
      state         move-cursor
      cur-x-diff    =x-diff-normal
      cur-y-diff    =y-diff-normal
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

  ==>
    ;; request to move the cursor
    +manual>
      ISA           move-cursor
      loc           =visual-location

    =goal>
      state         check-target
      target-location =visual-location
  )

  ;; re-scan for the nearest oval to get info about its color
  (P check-target
    =goal>
      ISA           targeting
      state         check-target
      target-location =vis-loc

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

    =goal>
      ;; move to the state where we distinguish between red and green targets
      state         distinguish-target
  )

  ;; after a rescan of the target, check if the target is still black and keep rescanning
  (P distinguish-target-black
    =goal>
      ISA           targeting
      state         distinguish-target
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
      last-command  prepare
  ==>
    ;; clear temporal in case we were running a whiff
    ;; TODO this is not gp in temporal
    +temporal>
      ISA           clear

    =goal>
      state distinguish-target-black-search

  )
  ;; search for the target again
  (P distinguish-target-black-search
    =goal>
      ISA targeting
      state distinguish-target-black-search
      target-location =target-location

    ?visual-location>
      buffer empty
  ==>
    =goal>
      state distinguish-target

    ;; request visual location search for nearest oval (should be the same we found last time, but it may be colored next time
    +visual-location>
      ISA           visual-location
      ;; search for oval
      kind          OVAL
      ;; nearest the stored location
      :nearest      =target-location
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

  ;; check if there the nearest target is no longer black
  (P detect-target-color
    =goal>
      ISA           targeting
      state         distinguish-target
      target-x      =cx
      target-y      =cy
      cur-x-diff    =x-diff
      cur-y-diff    =y-diff
      
    ;; check for non-black target
    =visual-location>
      ISA         visual-location
      ;; check for oval
      kind        OVAL
      ;; check for not black
      - color     black
      ;; match color
      color       =color
      ;; match location values to check if on line
      screen-x    =sx
      screen-y    =sy

    ;; check that color target is on the original target line
    !bind!          =on-line (is-on-line =sx =sy =cx =cy =x-diff =y-diff)

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
  ;; detect flyby target
  (P detect-flyby
    =goal>
      ISA           targeting
      state         distinguish-target
      target-x      =cx
      target-y      =cy
      cur-x-diff    =x-diff
      cur-y-diff    =y-diff

    ;; check for non-black target
    =visual-location>
      ISA         visual-location
      ;; check for oval
      kind        OVAL
      ;; check for not black
      - color     black
      ;; match color
      color       =color
      ;; match location values to check if on line
      screen-x    =sx
      screen-y    =sy

    ;; check that color target is not on the original target line
    !bind!          =not-on-line (not (is-on-line =sx =sy =cx =cy =x-diff =y-diff))
  ==>
    =goal>
      state detect-flyby-search
  )
  (P detect-flyby-search
    =goal>
      isa targeting
      state detect-flyby-search
      target-location =target-location
    ?visual-location>
      buffer empty
  ==>
    ;; request visual location search for nearest oval (should be the same we found last time, but it should be colored now)
    +visual-location>
      ISA           visual-location
      ;; search for oval
      kind          OVAL
      ;; nearest the stored location
      ;; TODO do we want nearest here?
      :nearest      =target-location
    =goal>
      state distinguish-target
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

    ?manual>
      ;; match if we had time to fire the prepare-click during the move,
      ;; if not, decide-to-shoot-full-click will be selected instead
      last-command  prepare
      preparation   free
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
  (P decide-to-shoot-full-click
    =goal>
      isa           targeting
      state         decide-whether-to-shoot
    
    ;; match shoot chunk
    =retrieval>
      isa response
      action shoot

    ?manual>
      last-command move-cursor
      preparation free
  ==>
    =goal>
      state find-black-target

    ;; submit click request
    +manual>
      isa click-mouse
    ;; clear temporal in case we're running a whiff
    +temporal>
      isa clear
    !eval! (format t "detected enemy, clicking~%")
    ;; increment the number of targets checkd
    !eval! (incf *check-order*)
  )

  ;; bump TODO does this ever happen? waiting for imaginal in cap-first should prevent it
  ;; detect friend when we've already seen it
  (P decide-not-to-shoot-friend-remembered
    =goal>
      ISA           targeting
      state         decide-whether-to-shoot
      friend-x      =fx

    ;; match no-shoot chunk
    =retrieval>
      ISA           response
      action        oh-no-dont-shoot
  ==>
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
      cur-x-diff    =x-diff
      cur-y-diff    =y-diff

    ;; match no-shoot chunk
    =retrieval>
      ISA           response
      action        oh-no-dont-shoot

  ==>

    ;; clear temporal in case we were running a whiff
    ;; TODO this is not gp in temporal
    +temporal>
      ISA           clear

    ;; remember motion
    =goal>
      state         find-black-target
      friend-x      =sx
      friend-y      =sy
      friend-x-diff =x-diff
      friend-y-diff =y-diff
      heuristic     highest
    ;; increment the number of times the friend target was hovered
    !eval!          (incf *friend-hovers*)
    !eval!          (format t "detected friend~%")

    ;; set the order in which the friend was checked if it hasn't been set yet
    !eval!          (when (eq -1 *friend-order*) (setf *friend-order* *check-order*))
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
    =goal>
      state distinguish-whiff-search
    ;; log that we did this
    ;; TODO we should do this in move-after-whiff to only count when whiffs really happen
    !eval!          (incf *whiff-counter*)
    !eval!          (format t "detected whiff~%")
  )

  (P distinguish-whiff-search
    =goal>
      isa targeting
      state distinguish-whiff-search
      target-location =target-location
    ?visual-location>
      buffer empty
  ==>
    ;; TODO this should be a separate rule for gp
    ;; request visual location search for nearest oval (should be the same we found last time, but it should be colored now)
    +visual-location>
      ISA           visual-location
      ;; search for oval
      kind          OVAL
      ;; nearest the current location
      :nearest      =target-location
    =goal>
      state distinguish-target
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
    =goal>
      state whiff-spin-search
  )
  (P whiff-spin-search
    =goal>
      isa targeting
      state whiff-spin-search
      target-location =target-location
    ?visual-location>
      buffer empty
  ==>
    ;; TODO this should be a separate rule for gp
    ;; recan vis-loc
    +visual-location>
      ISA           visual-location
      kind          OVAL
      :nearest      =target-location
    =goal>
      state distinguish-target
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

) ; end model
