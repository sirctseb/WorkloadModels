(define-model targeting-hard-slow

  (sgp
    :cursor-noise t
    :incremental-mouse-moves 0.01
    :esc t)

  (set-visloc-default isa visual-location color does-not-exist)
  (start-hand-at-mouse)
  (set-cursor-position 960 600)

  (chunk-type targeting state target-location target-x target-y friend-x friend-y)
  (chunk-type response color action)

  (add-dm
    (enemy-response isa response color red action shoot)
    (friend-response isa response color green action dont-shoot)
    (goal isa targeting state find-black-target))

  (set-base-levels (enemy-response 3) (friend-response 3))

  (goal-focus goal)

  (P find-black-target
    =goal>
      ISA         targeting
      state       find-black-target

    ?visual-location>
      buffer      empty
  ==>
    +visual-location>
      ISA         visual-location
      kind        OVAL
      color       black

    =goal>
      state       move-cursor
  )

  (P avoid-friend
    =goal>
      ISA           targeting
      state         move-cursor
      friend-x             =fx
      friend-y             =fy

    =visual-location>
      ISA           visual-location
      screen-x      =fx
      screen-y      =fy

  ==>
    =goal>
      state         find-black-target
  )

  (P move-cursor-no-friend-info
    =goal>
      ISA           targeting
      state         move-cursor
      friend-x      nil

    =visual-location>
      ISA           visual-location
      kind          OVAL
      screen-x      =x
      screen-y      =y

    ?manual>
      preparation   free
  ==>

    +manual>
      ISA           move-cursor
      loc           =visual-location

    =goal>
      state         move-attention
      target-x      =x
      target-y      =y
      target-location =visual-location
  )

  (P move-cursor-not-friend
    =goal>
      ISA           targeting
      state         move-cursor
      friend-x      =fx
      friend-y      =fy

    =visual-location>
      ISA           visual-location
      kind          OVAL
      - screen-x    =fx
      - screen-y    =fy
      screen-x      =x
      screen-y      =y

    ?manual>
      preparation   free
  ==>

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

  (P check-target
    =goal>
      ISA           targeting
      state         check-target
      target-x      =x
      target-y      =y

    ?visual-location>
      buffer        empty
  ==>
    +visual-location>
      ISA           visual-location
      kind          OVAL
      screen-x      =x
      screen-y      =y

    =goal>
      state         distinguish-target
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

  (P detect-target-color
    =goal>
      ISA           targeting
      state         distinguish-target
      target-x      =target-x
      target-y      =target-y
    
    =visual-location>
      ISA           visual-location
      kind          OVAL
      - color       black
      color         =color

    ?retrieval>
      state         free
      buffer        empty
  ==>
    +retrieval>
      ISA           response
      color         =color

    =goal>
      state         decide-whether-to-shoot
  )

  (P decide-to-shoot
    =goal>
      ISA           targeting
      state         decide-whether-to-shoot

    =retrieval>
      ISA           response
      action        shoot

  ==>
    =goal>
      state         click-mouse
  )

  (P decide-not-to-shoot
    =goal>
      ISA           targeting
      state         decide-whether-to-shoot
      target-x      =sx
      target-y      =sy

    =retrieval>
      ISA           response
      action        dont-shoot
  ==>
      
    =goal>
      state         find-black-target
      friend-x      =sx
      friend-y      =sy
  )

  (P distinguish-whiff
    =goal>
      ISA           targeting
      state         distinguish-target

    =visual-location>
      ISA           visual-location
      kind          OVAL
      color         black

    ?manual>
      state         free
  ==>
    =goal>
      state distinguish-whiff-search

    +manual>
      isa           move-cursor
      loc           =visual-location
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
    +visual-location>
      ISA           visual-location
      kind          OVAL
      screen-x      =target-x
      screen-y      =target-y

    =goal>
      state distinguish-target
  )

  (P distinguish-target-black
    =goal>
      ISA           targeting
      state         distinguish-target

    =visual-location>
      ISA           visual-location
      kind          OVAL
      color         black

    ?manual>
      state         busy
  ==>
    =goal>
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
    +visual-location>
      ISA           visual-location
      kind          OVAL
      screen-x      =target-x
      screen-y      =target-y

    =goal>
      state distinguish-target
  )

  (P click-mouse
    =goal>
      ISA           targeting
      state         click-mouse

    ?manual>
      state         free
  ==>
    
    +manual>
      ISA           click-mouse

    =goal>
      state         find-black-target
  )
)
