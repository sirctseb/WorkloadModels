;;; Implementation of the targeting task for the experiment in the easy - slow condition
(define-model targeting-easy-slow

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
    :viewing-distance 96)
  ;; we'll count this as sgp
  ;; set the default visloc chunk to something that will never match
  ;; the effect is to disable buffer stuffing
  (set-visloc-default isa visual-location color does-not-exist)
  (start-hand-at-mouse)
  ; (sgp :mouse-fitts-coeff 0.15)
  (set-cursor-position 960 600)

  ;; chunk types
  (chunk-type targeting state target-x target-y cursor-diff-x cursor-diff-y target-location check-miss)
  (chunk-type friend-target x y)

  ;; dms
  (add-dm (track isa chunk) (attend-letter isa chunk)
    (goal isa targeting state find-red-target))

  ;; goal focus
  (goal-focus goal)

  ;; Productions
  
  ;; Rule to start searching for a target  
  (P find-red-target
    =goal>
      ISA         targeting
      state       find-red-target
    ;; check for empty vis-loc
    ?visual-location>
      buffer      empty
  ==>
    +visual-location>
      ISA         visual-location
      :attended   nil
      kind        OVAL
      color       red
    =goal>
      state       move-cursor
  )

  ;; if there's no unattended red target, search for any red target
  (P no-unattended-red
    =goal>
      ISA           targeting
      state         move-cursor
      ;; make sure we haven't already checked for a miss
      check-miss    nil
    ?visual-location>
      state         error
    ;; TODO this is a non-semantic manual free check to make sure that,
    ;; TODO in the case that we miss one target, go click the other,
    ;; TODO do a find-red-target that fails because the first is attended,
    ;; TODO and the second isn't gone yet because the click hasn't gone through,
    ;; TODO we wait until it is gone to do the search that allows attended targets
    ?manual>
      state         free
  ==>
    ;; search for any red that we might have missed
    +visual-location>
      ISA           visual-location
      kind          OVAL
      color         red
      
    =goal>
      check-miss    t
    !eval!          (dolog "failed to attend to target location in move cursor~%")
    !eval!          (incf *vis-fails*)
  )

  ;; if there is no red target at all, go to end state
  (P no-red
    =goal>
      ISA           targeting
      state         move-cursor
      ;; check that we've already searched attended reds
      check-miss    t

    ?visual-location>
      state         error
  ==>
    ;; go to end state
    =goal>
      state         end
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

    ;; check that visual is free and empty
    ?visual>
      state         free
      buffer        empty

    ;; make sure motor system is free
    ?manual>
      preparation   free
  ==>

    ;; request to move the cursor
    +manual>
      ISA           move-cursor
      loc           =visual-location

    ;; attend to target
    +visual>
      ISA           move-attention
      screen-pos    =visual-location
    =goal>
      state         click-mouse
      check-miss    nil
  )

  ;; TODO this holds visual for the whole mouse move
  ;; TODO we probably shouldn't do this but we need to check what
  ;; TODO effect it has on dual-task

  ; request a mouse click
  (P click-mouse
    =goal>
      ISA           targeting
      state         click-mouse

    ;; make sure motor module is free
    ?manual>
      state         free

    ;; harvest visual
    ;; TODO we don't actually want to wait for this here though,
    ;; TODO we should make a parallel production that just harvests visual
    =visual>
      ISA           OVAL
  ==>
    
    ;; submit click request
    +manual>
      ISA           click-mouse

    +visual>
      ISA           clear

    =goal>
      state         find-red-target
  )
) ; end model
