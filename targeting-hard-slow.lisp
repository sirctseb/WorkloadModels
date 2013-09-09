;;; Implementation of the targeting task for the experiment in the hard - slow condition
(define-model targeting-hard-slow

  ;; sgp section
  (sgp
    :needs-mouse nil
    :show-focus t
    :trace-detail high
    :cursor-noise t
    :vwt t
    :incremental-mouse-moves 0.01
    :randomize-time t
    :visual-movement-tolerance 0.5
    :pixels-per-inch 96
    :motor-feature-prep-time 0.053
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
  (sgp :seed (30 15))
  ;; fitt's law coefficient
  ; (sgp :mouse-fitts-coeff 0.15)
  ;; we'll count this as sgp
  ;; set the default visloc chunk to something that will never match
  ;; the effect is to disable buffer stuffing
  (set-visloc-default isa visual-location color does-not-exist)
  (start-hand-at-mouse)
  (set-cursor-position 960 600)

  ;; chunk types
  (chunk-type targeting state target-location target-x target-y friend-x friend-y hits)
  (chunk-type friend-target x y)
  (chunk-type response color action)

  ;; dms
  (add-dm (track isa chunk) (attend-letter isa chunk)
    (enemy-response isa response color red action shoot)
    (friend-response isa response color green action oh-no-dont-shoot)
    (goal isa targeting state find-black-target hits 0))
  (set-base-levels (enemy-response 1) (friend-response 1))

  ;; goal focus
  (goal-focus goal)

  ;; Productions

  ;; Rule to start searching for a target
  (P find-black-target
    =goal>
      ISA         targeting
      state       find-black-target
      friend-x    nil

    ;; check for empty visual-location buffer
    ?visual-location>
      buffer      empty
  ==>
    ;; search for an unattended black target
    +visual-location>
      ISA         visual-location
      kind        OVAL
      color       black

    ;; update state
    =goal>
      state       move-cursor
  )
  ;; search for a black target that isn't at the friend location, but only check the x value of the location
  (P find-black-target-friend-x
    =goal>
      ISA         targeting
      state       find-black-target
      friend-x    =fx
      friend-y    =fy

    ;; check for empty visual-location buffer
    ?visual-location>
      buffer empty
      ;; TODO this should avoid catching addition vis-loc errors in fail-find-not-friend
      -state error
  ==>
    ;; search for an unattended black target where the friend isn't
    +visual-location>
      ISA         visual-location
      kind OVAL
      ;; TODO don't actually need black filter when searching away from friend
      color black
      -screen-x =fx

    ;; update state
    =goal>
      state     move-cursor
  )

  ;; TODO possible failure case when check friend first, then click on enemy that overlaps with another

  ;; if we fail to find a target when we have no friend info, we may be over the green target,
  ;; so keep searching until targets refresh
  (P fail-find-no-friend
    =goal>
      state move-cursor
      friend-x nil
    ?visual-location>
      state error
  ==>
    +visual-location>
      ISA visual-location
      kind OVAL
      color black
  )

  ;; if we fail to find a target that is not the friend, keep searching for it until the targets refresh
  ;; this also checks against the y value, handling the case where a remaining enemy target shares an x value
  ;; with the friend
  ;; TODO this is not gp
  (P fail-find-not-friend
    =goal>
      state move-cursor
      friend-x =fx
      friend-y =fy
    ?visual-location>
      state error
  ==>
    +visual-location>
      ISA visual-location
      kind OVAL
      ; color black
      -screen-y =fy
  )

  ;; rule to check the visual location against a remembered
  ;; location of a friend target and go to a different one
  (P avoid-friend
    ;; check for move-cursor state
    =goal>
      ISA           targeting
      state         move-cursor
      ;; get friend location
      friend-x             =fx
      friend-y             =fy

    =visual-location>
      ISA           visual-location
      ;; check if new location is at the remembered friend location
      screen-x      =fx
      screen-y      =fy

  ==>
    =goal>
      state         find-black-target

    !eval!          (format t "avoiding friend~%")
    ;; increment number of times avoided friend
    !eval!          (incf *friend-avoids*)
  )

  ;; rule to move cursor toward target when there is no friend info remembered
  (P move-cursor-no-friend-info
    =goal>
      ISA           targeting
      state         move-cursor
      friend-x      nil

    ;; check for vis-loc result
    =visual-location>
      ISA           visual-location
      kind          OVAL
      screen-x      =x
      screen-y      =y

    ;; make sure motor system is free
    ?manual>
      preparation   free
  ==>

    ;; request to move the cursor
    +manual>
      ISA           move-cursor
      loc           =visual-location

    =goal>
      state         move-attention
      target-x      =x
      target-y      =y
      target-location =visual-location
  )

  ;; rule to move cursor toward target when there is friend info but it doesn't match
  (P move-cursor-not-friend
    =goal>
      ISA           targeting
      state         move-cursor
      friend-x      =fx
      friend-y      =fy

    =visual-location>
      ISA           visual-location
      kind          OVAL
      ;; check that it doesn't match friend location
      - screen-x    =fx
      - screen-y    =fy
      screen-x      =x
      screen-y      =y

    ;; make sure motor system is free
    ?manual>
      preparation   free
  ==>

    ;; request to move the cursor
    +manual>
      ISA           move-cursor
      loc           =visual-location

    =goal>
      state         move-attention
      target-x      =x
      target-y      =y
      target-location =visual-location
  )

  (P move-attention
    =goal>
      isa targeting
      state move-attention
      target-location =target-location

    ?visual>
      state free
      buffer empty
  ==>
    +visual>
      isa move-attention
      screen-pos =target-location
    =goal>
      state check-target
  )

  (P harvest-visual
    =goal>
      isa targeting
    ?visual>
      state free
    =visual>
      isa OVAL
  ==>
    +visual>
      isa clear
  )

  ;; re-scan for the nearest oval to get info about its color
  (P check-target
    =goal>
      ISA           targeting
      state         check-target
      target-x      =x
      target-y      =y
    ; check that vis-loc is empty because we will request it here
    ?visual-location>
      buffer        empty
  ==>
    ;; request visual location search for same oval (should be the same we found last time, but it should be colored now)
    +visual-location>
      ISA           visual-location
      ;; search for oval
      kind          OVAL
      ;; search for same object
      screen-x      =x
      screen-y      =y

    =goal>
      ;; move to the state where we distinguish between red and green targets
      state         distinguish-target
  )

  ;; detect if the target is no longer black
  (P detect-target-color
    =goal>
      ISA           targeting
      state         distinguish-target
      target-x      =target-x
      target-y      =target-y
    
    =visual-location>
      ISA           visual-location
      ;; check for oval
      kind          OVAL
      ;; check for not block
      - color       black
      ;; match color
      color         =color

    ;; TODO other task will have to be gp in retrieval
    ?retrieval>
      state         free
      buffer        empty
  ==>
    ;; request lookup of action based on color
    +retrieval>
      ISA           response
      color         =color

    ;; update goal
    =goal>
      state         decide-whether-to-shoot
  )

  ;; handle case where friend was not found before clicking both targets so we started moving to it
  ;; before targets refreshed
  (P distinguish-target-error
    =goal>
      ISA targeting
      state distinguish-target
    ;; TODO should check not error in check-target to avoid getting addition vis-loc errors here
    ?visual-location>
      state error
  ==>
    =goal>
      state find-black-target
      ;; reset friend location so find-black-target can fire
      friend-x nil
      friend-y nil
  )

  ;; after a rescan of the target, check if the target is red and click it
  (P decide-to-shoot
    =goal>
      ISA           targeting
      state         decide-whether-to-shoot

    ;; match shoot action from memory
    =retrieval>
      ISA           response
      action        shoot

  ==>
    ;; go to click mouse state to wait for manual state to be free
    =goal>
      state         click-mouse

    !eval!          (format t "detected enemy, clicking~%")

    ;; increment the number of targets checked
    !eval!          (incf *check-order*)
  )

  ;; after a rescan of the target, check if the target is green and go back to finding black targets
  (P decide-not-to-shoot
    =goal>
      ISA           targeting
      state         decide-whether-to-shoot
      target-x      =sx
      target-y      =sy

    =retrieval>
      ISA           response
      action        oh-no-dont-shoot

    ; =visual>
    ;   isa oval
    ;; make sure state is free so we can clear
    ; ?visual>
    ;   state free
  ==>
    ; +visual>
    ;   isa clear
      
    ;; increment the number of times the friend target was hovered
    !eval!          (incf *friend-hovers*)
    !eval!          (format t "detected friend~%")

    ;; set the order in which the friend was checked if it hasn't been set yet
    !eval!          (when (eq -1 *friend-order*) (setf *friend-order* *check-order*))
    ;; search for black targets again
    =goal>
      state         find-black-target
      ;; store location of friend target
      friend-x      =sx
      friend-y      =sy
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
    =goal>
      state distinguish-whiff-search
    ;; move to the same location
    +manual>
      isa           move-cursor
      loc           =visual-location

    ;; log that we did this
    !eval!          (incf *whiff-counter*)
    !eval!          (format t "wiffed too long, moving ~%")
    !eval!          (incf *total-whiff-counter*)
  )

  (P distinguish-whiff-search
    =goal>
      isa targeting
      state distinguish-whiff-search
      target-x      =target-x
      target-y      =target-y

    ?visual-location>
      buffer empty
  ==>
    ;; start vis-loc loop again
    +visual-location>
      ISA           visual-location
      ;; search for oval
      kind          OVAL
      ;; search for same oval
      screen-x      =target-x
      screen-y      =target-y
    =goal>
      state distinguish-target
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
    =goal>
      ;; move to the state where we distinguish between red and green targets
      state         distinguish-target-black-search
  )
  (P distinguish-target-black-search
    =goal>
      isa targeting
      state distinguish-target-black-search
      target-x      =target-x
      target-y      =target-y

    ?visual-location>
      buffer empty

  ==>
    ;; request visual location search for same oval (should be the same we found last time, but it should be colored now)
    +visual-location>
      ISA           visual-location
      ;; search for oval
      kind          OVAL
      ;; search for same object
      screen-x      =target-x
      screen-y      =target-y
    =goal>
      state distinguish-target
  )


  ; request a mouse click
  (P click-mouse
    =goal>
      ISA           targeting
      state         click-mouse
      hits          =hits

    ;; make sure motor module is free
    ;; TODO only preparation needs to be free
    ?manual>
      state         free

    ; =visual>
    ;   isa oval
    ;; make sure visual is free so we can clear it
    ; ?visual>
    ;   state free
    !bind! =hits-inc (+ =hits 1)
  ==>
    
    ;; submit click request
    +manual>
      ISA           click-mouse
    ; +visual>
    ;   isa clear

    =goal>
      state         find-black-target
      hits          =hits-inc
  )

) ; end model
