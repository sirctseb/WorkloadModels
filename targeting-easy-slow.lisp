(define-model targeting-easy-slow

  (sgp
    :cursor-noise t
    :incremental-mouse-moves 0.01
    :esc t)

  (set-visloc-default isa visual-location color does-not-exist)
  (start-hand-at-mouse)
  (set-cursor-position 960 600)

  (chunk-type targeting state target-x target-y target-location check-miss)
  (chunk-type response color action)

  (add-dm
    (enemy-response isa response color red action shoot)
    (friend-response isa response color green action dont-shoot)
    (goal isa targeting state find-red-target))

  (goal-focus goal)

  (P find-red-target
    =goal>
      ISA         targeting
      state       find-red-target

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

  (P no-unattended-red
    =goal>
      ISA           targeting
      state         move-cursor
      check-miss    nil

    ?visual-location>
      state         error

    ?manual>
      state         free
  ==>
    =goal>
      state no-unattended-red-search
      check-miss    t
  )

  (P no-unattended-red-search
    =goal>
      ISA targeting
      state no-unattended-red-search

    ?visual-location>
      buffer empty
  ==>
    +visual-location>
      ISA visual-location
      kind OVAL
      color red

    =goal>
      state move-cursor
  )

  (P no-red
    =goal>
      ISA           targeting
      state         move-cursor
      check-miss    t

    ?visual-location>
      state         error
  ==>
    =goal>
      state         end
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
      state         move-attention
      target-location =visual-location
      check-miss    nil
  )

  (P move-attention
    =goal>
      ISA targeting
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
      state click-mouse
  )

  (P click-mouse
    =goal>
      ISA           targeting
      state         click-mouse

    ?manual>
      state         free

    =visual>
      ISA           OVAL
  ==>
    
    +manual>
      ISA           click-mouse

    +visual>
      ISA           clear

    =goal>
      state         find-red-target
  )
)
