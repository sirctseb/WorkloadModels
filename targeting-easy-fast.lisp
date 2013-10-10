(define-model targeting-easy-fast

  (sgp
    :cursor-noise t
    :incremental-mouse-moves 0.01)

  (set-visloc-default isa visual-location color does-not-exist)
  (start-hand-at-mouse)
  (set-cursor-position 960 600)

  (chunk-type targeting state target-location target-x target-y projected-x projected-y)

  (add-dm (goal isa targeting state find-red-target))

  (goal-focus goal)

  (P find-red-target
    =goal>
      ISA         targeting
      state       find-red-target

    ?manual>
      state       free

    ?visual-location>
      buffer      empty
  ==>

    +visual-location>
      ISA         visual-location
      :attended   nil
      kind        OVAL
      color       red

    =goal>
      state       cap-first-location

  )

  (P fail-find
    =goal>
      ISA         targeting
      state       cap-first-location

    ?visual-location>
      state       error
  ==>
    =goal>
      state       fail
  )

  (P cap-first-location
    =goal>
      ISA           targeting
      state         cap-first-location

    =visual-location>
      ISA           visual-location
      screen-x      =tx
      screen-y      =ty

  ==>
    =goal>
      target-x      =tx
      target-y      =ty
      target-location =visual-location
      state         cap-first-location-search
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
      ISA           visual-location
      :nearest      =target-location
      color         red

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
      state move-attention
      target-location =visual-location
  )

  (P move-attention
    =goal>
      isa targeting
      state move-attention
      target-location =target-location
    
    ?visual>
      state         free
      buffer        empty
  ==>

    +visual>
      ISA           move-attention
      screen-pos    =target-location

    =goal>
      state prepare-click
  )

  (P prepare-click
    =goal>
      ISA           targeting
      state         prepare-click

    ?manual>
      last-command  move-cursor
      preparation   free

  ==>

    +manual>
      ISA           prepare
      style         punch
      hand          right
      finger        index

    =goal>
      state click-mouse
  )

  (P click-mouse
    =goal>
      ISA           targeting
      state         click-mouse

    ?manual>
      last-command  prepare
      preparation   free

    ?visual>
      state         free
    =visual>
      ISA           OVAL
  ==>
    +manual>
      ISA           execute

    +visual>
      ISA           clear

    =goal>
      state         find-red-target
  )
)
