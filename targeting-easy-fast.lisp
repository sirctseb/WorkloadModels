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
  ==>
    +visual-location>
      ISA         visual-location
      :attended   nil
      kind        OVAL
      color       red
    =goal>
      state       cap-first-location
     ;; reset timer
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
    ?visual>
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

    ;; let prepare-click go first
    ;; TODO this is not a semantic test. it only exists to allow prepare-click to go first
    ;; TODO there should be a better way to let prepare-click to have priority
    ;; TODO we could just put a flag in goal
    ?manual>
      last-command  prepare
      preparation   free

    ;; wait for visual to be free so we can clear it
    ?visual>
      state         free
  ==>
    =goal>
      state         find-red-target

    ;; submit click request
    +manual>
      ISA           execute

    ;; clear visual buffer so that it doesn't keep re-encoding and slowing down future searches
    +visual>
      ISA           clear

    !eval!          (format t "detected enemy, clicking~%")

    ;; increment the number of targets checked
    !eval!          (incf *check-order*)
  )
)
