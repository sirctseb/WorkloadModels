;;; Implementation of the targeting task for the experiment in the easy - slow condition
(define-model targeting-easy-slow

  ;; sgp section
  (sgp :needs-mouse nil
    :show-focus t
    :trace-detail high
    :cursor-noise t
    :vwt t
    :incremental-mouse-moves 0.01
    :randomize-time nil
    :visual-movement-tolerance 0.5
    :pixels-per-inch 96
    :viewing-distance 96)

  ;; chunk types
  (chunk-type targeting state target-x target-y cursor-diff-x cursor-diff-y target-location)
  (chunk-type friend-target x y)

  ;; dms
  (suppress-warnings
    (add-dm (track isa chunk) (attend-letter isa chunk)
        (goal isa targeting state find-red-target))
    )

  ;; goal focus
  (goal-focus goal)

  ;; Productions
  
  ;; Rule to start searching for a target  
  (P find-red-target
    =goal>
      ISA         targeting
      state       find-red-target
    ?visual>
        state       free
  ==>
  +visual-location>
      ISA         visual-location
      :attended   nil
      kind        OVAL
      color       red
    =goal>
      state       move-cursor
  )

  (P on-move
    =goal>
      ISA         targeting
      state       find-red-target
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
      state         click-mouse
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
      state         find-red-target
  )
)
