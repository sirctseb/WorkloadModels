;;; Implementation of the targeting task for the experiment in the easy - fast condition
(define-model targeting-easy-fast

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
  ;; we'll count this as sgp
  ;; set the default visloc chunk to something that will never match
  ;; the effect is to disable buffer stuffing
  (set-visloc-default isa visual-location screen-x 0 screen-x 1)

  ;; chunk-types
  (chunk-type targeting state target-x target-y projected-x projected-y)
  (chunk-type friend-target x y x-diff y-diff)

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

    ;; require manual to be free before searching for a target
    ;; TODO this is not really semantic. it resolves a problem where
    ;; TODO this +vis-loc can find a target that will be clicked on in about 200ms
    ;; TODO and then will vanish. then the cap-first-location +vis-loc will
    ;; TODO find the other red target because the first one is gone by then
    ;; TODO the projection is obviously way off and we a) miss and b) waste time
    ;; TODO this costs us about 200ms in mean completion time for 0 misses
    ?manual>
      state       free

    ;; check for empty vis-loc
    ?visual-location>
      buffer      empty

    ;; check for empty temporal
    ?temporal>
      buffer      empty
  ==>
    ;; do search for enemy target
    +visual-location>
      ISA         visual-location
      :attended   nil
      kind        OVAL
      color       red

    ;; update goal
    =goal>
      state       cap-first-location

    ;; start timer
    +temporal>
      ISA           time
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
    ;; also check that visual buffer is empty
    ?visual>
      state         free
      buffer        empty

  ==>
    ;; store location in goal
    =goal>
      target-x      =tx
      target-y      =ty
      state         lead-target

    !eval!          (format t "storing first target location: ~a, ~a~%" =tx =ty)

    ;; search for same location
    ;; TODO is this a violation of greedy-polite?
    ;; TODO i.e., should vis-loc become empty for a production cycle before we can do the search again?
    ;; TODO then we would have to store the location
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
    ;; TODO is this a violation of greedy-polite?
    =visual-location>
      screen-x      =projected-x
      screen-y      =projected-y

    ;; and move to next state
    ;; TODO could move move request here to speed up
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

    ;; check for oval location
    =visual-location>
      ISA           visual-location
      kind          OVAL

    ;; make sure motor system is free
    ;; TODO can greedy-polite say anything about the manual buffer? it doesn't contain anything
    ?manual>
      preparation   free

  ==>

    ;; request to move the cursor
    +manual>
      ISA           move-cursor
      loc           =visual-location

    ;; update goal
    =goal>
      state         click-mouse
  )

  ;; prepare a click while checking the target
  ;; TODO I don't think this abides by greedy-polite, but for combining with addition it doesn't matter
  (P prepare-click
    =goal>
      ISA           targeting
      state         click-mouse

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

  ;; wait until mouse move is done to click
  (P click-mouse
    =goal>
      ISA           targeting
      state         click-mouse

    ;; let prepare-click go first
    ;; TODO this is not a semantic test. it only exists to allow prepare-click to go first
    ;; TODO there should be a better way to let prepare-click to have priority
    ;; TODO we could just put a flag in goal
    ?manual>
      last-command  prepare
      preparation   free

  ==>
    =goal>
      state         find-red-target

    ;; submit click request
    +manual>
      ISA           execute

    !eval!          (format t "detected enemy, clicking~%")

    ;; increment the number of targets checked
    !eval!          (incf *check-order*)
  )
)
