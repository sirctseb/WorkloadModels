(define-model targeting-hard-fast

  (sgp
    :cursor-noise t
    :incremental-mouse-moves 0.01
    :esc t)

  (set-visloc-default isa visual-location color does-not-exist)
  (start-hand-at-mouse)
  (set-cursor-position 960 600)

  (chunk-type targeting state target-x target-y target-location
              friend-x friend-y friend-x-diff friend-y-diff cur-x-diff cur-y-diff ticks heuristic)
  (chunk-type response color action)

  (add-dm
    (enemy-response isa response color red action shoot)
    (friend-response isa response color green action dont-shoot)
    (goal isa targeting state find-black-target heuristic lowest))
  (set-base-levels (enemy-response 3) (friend-response 3))

  (goal-focus goal)

  (P find-black-target
    =goal>
      ISA         targeting
      state       find-black-target
      heuristic   =heuristic

    ?visual-location>
      buffer      empty
  ==>
    +visual-location>
      ISA         visual-location
      :attended   nil
      kind        OVAL
      color       black
      screen-x    =heuristic

    =goal>
      state       cap-first-location
  )

  (P avoid-friend-lowest
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
      screen-x      =tx
      screen-y      =ty

    !bind!          =on-line (is-on-line =tx =ty =fx =fy =x-diff =y-diff)

  ==>
    =goal>
      state         find-black-target
      heuristic     highest
  )
  (P avoid-friend-highest
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
      screen-x      =tx
      screen-y      =ty

    !bind!          =on-line (is-on-line =tx =ty =fx =fy =x-diff =y-diff)

  ==>
    =goal>
      state         find-black-target
      heuristic     lowest
  )

  (P cap-first-location
    =goal>
      ISA           targeting
      state         cap-first-location
      friend-x      nil

    =visual-location>
      ISA           visual-location
      screen-x      =tx
      screen-y      =ty
  ==>
    =goal>
      target-x      =tx
      target-y      =ty
      state         cap-first-location-search
      target-location =visual-location
  )
  (P cap-first-location-not-friend
    =goal>
      ISA           targeting
      state         cap-first-location
      friend-x      =fx
      friend-y      =fy
      friend-x-diff =x-diff
      friend-y-diff =y-diff

    =visual-location>
      ISA           visual-location
      screen-x      =tx
      screen-y      =ty

    !bind!          =on-line (not (is-on-line =tx =ty =fx =fy =x-diff =y-diff))
  ==>
    =goal>
      target-x      =tx
      target-y      =ty
      state         cap-first-location-search
      target-location =visual-location
  )

  (P cap-first-location-search
    =goal>
      isa targeting
      state cap-first-location-search
      target-location =target-location

    ?visual-location>
      buffer empty
  ==>
    +visual-location>
      ISA visual-location
      kind      OVAL
      :nearest  =target-location

    =goal>
      state lead-target
  )

  (P lead-target
    =goal>
      ISA           targeting
      state         lead-target
      target-x      =tx
      target-y      =ty

    =visual-location>
      ISA           visual-location
      screen-x      =sx
      screen-y      =sy
  ==>
    !bind!          =x-diff (- =sx =tx)
    !bind!          =y-diff (- =sy =ty)
    !bind!          =mag (sqrt (+ (* =x-diff =x-diff) (* =y-diff =y-diff)))
    !bind!          =x-diff-normal (/ =x-diff =mag)
    !bind!          =y-diff-normal (/ =y-diff =mag)
    !bind!          =projected-x (+ =sx (* *target-projection* =x-diff-normal))
    !bind!          =projected-y (+ =sy (* *target-projection* =y-diff-normal))

    =visual-location>
      screen-x      =projected-x
      screen-y      =projected-y

    =goal>
      state         move-cursor
      cur-x-diff    =x-diff-normal
      cur-y-diff    =y-diff-normal
  )

  (P move-cursor
    =goal>
      ISA           targeting
      state         move-cursor

    =visual-location>
      ISA           visual-location
      kind          OVAL

    ?manual>
      preparation   free
  ==>

    +manual>
      ISA           move-cursor
      loc           =visual-location

    =goal>
      state         check-target
      target-location =visual-location
  )

  (P check-target
    =goal>
      ISA           targeting
      state         check-target
      target-location =vis-loc

    ?visual-location>
        buffer        empty
  ==>

    +visual-location>
      ISA           visual-location
      kind          OVAL
      :nearest      =vis-loc

    =goal>
      state         distinguish-target
  )

  (P distinguish-target-black
    =goal>
      ISA           targeting
      state         distinguish-target
      target-location =target-location

    =visual-location>
      ISA           visual-location
      kind          OVAL
      color         black

    ?manual>
      state         busy
      last-command  prepare
  ==>
    +temporal>
      ISA           clear

    =goal>
      state distinguish-target-black-search
  )

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

    +visual-location>
      ISA           visual-location
      kind          OVAL
      :nearest      =target-location
  )

  (P prepare-click
    =goal>
      ISA           targeting
      state         distinguish-target

    ?manual>
      last-command  move-cursor
      preparation   free
  ==>
    +manual>
      ISA           prepare
      style         punch
      hand          right
      finger        index
  )

  (P detect-target-color
    =goal>
      ISA           targeting
      state         distinguish-target
      target-x      =cx
      target-y      =cy
      cur-x-diff    =x-diff
      cur-y-diff    =y-diff
      
    =visual-location>
      ISA         visual-location
      kind        OVAL
      - color     black
      color       =color
      screen-x    =sx
      screen-y    =sy

    !bind!          =on-line (is-on-line =sx =sy =cx =cy =x-diff =y-diff)

    ?retrieval>
      state       free
      buffer      empty
  ==>
    +retrieval>
      ISA         response
      color       =color

    =goal>
      state       decide-whether-to-shoot
  )

  (P decide-to-shoot
    =goal>
      ISA           targeting
      state         decide-whether-to-shoot

    =retrieval>
      ISA           response
      action        shoot

    ?manual>
      last-command  prepare
      preparation   free
  ==>
    =goal>
      state         find-black-target

    +manual>
      ISA           execute

    +temporal>
      ISA           clear
  )

  (P decide-not-to-shoot
    =goal>
      ISA           targeting
      state         decide-whether-to-shoot
      target-x      =sx
      target-y      =sy
      target-location =target-location
      cur-x-diff    =x-diff
      cur-y-diff    =y-diff

    =retrieval>
      ISA           response
      action        dont-shoot
  ==>

    +temporal>
      ISA           clear

    =goal>
      state         find-black-target
      friend-x      =sx
      friend-y      =sy
      friend-x-diff =x-diff
      friend-y-diff =y-diff
      heuristic     highest
  )

  (P distinguish-whiff
    =goal>
      ISA           targeting
      state         distinguish-target

    ?temporal>
      buffer        empty

    ?manual>
      state         free

    =visual-location>
      ISA           visual-location
      kind          OVAL
      color         black
  ==>
    +temporal>
      ISA           time

    =goal>
      state distinguish-whiff-search
  )

  (P distinguish-whiff-search
    =goal>
      isa targeting
      state distinguish-whiff-search
      target-location =target-location

    ?visual-location>
      buffer empty
  ==>
    +visual-location>
      ISA           visual-location
      kind          OVAL
      :nearest      =target-location

    =goal>
      state distinguish-target
  )

  (P whiff-spin
    =goal>
      ISA           targeting
      state         distinguish-target

    !bind!          =whiff-wait-time *whiff-wait-time*
    =temporal>
      ISA           time
      <= ticks      =whiff-wait-time

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

    !bind!          =whiff-wait-time *whiff-wait-time*

    =temporal>
      ISA           time
      > ticks       =whiff-wait-time

    =visual-location>
      ISA           visual-location
  ==>
    =goal>
      state         find-black-target

    +temporal>
      isa           clear
  )
)
