
(clear-all)

(defun do-arithmetic-trial ()

  (reset)
  
  (let* ((lis (permute-list '("1" "2" "3" "4" "5" "6" "7")))
    (answers nil)   
    (window (open-exp-window "Addition Problem"
      :visible t
      :width 300
      :height 300)))
  (add-text-to-exp-window :text "1 4 + 2 7"
    :width 100
    :x 100
    :y 140)

  (install-device window)
  (schedule-event-relative 5 'clear-screen)

  (proc-display)
  (run 30)))


(define-model addition

  (sgp :esc t :lf .05)
  (sgp :v t :show-focus t :trace-detail high)

  (chunk-type arithmetic first operator second result ones carry)
  (chunk-type arithmetic-problem first operator second result state ones carry tens plus-x second-ones-x first-ones-x)
  (chunk-type arithmetic-info first-tens first-ones second-tens second-ones)
  (chunk-type successor value successor)
  (chunk-type number)

  (add-dm
    (+ ISA CHUNK)
    (* ISA CHUNK)
    (n0 ISA NUMBER)
    (n1 ISA NUMBER)
    (n2 ISA NUMBER)
    (n3 ISA NUMBER)
    (n4 ISA NUMBER)
    (n5 ISA NUMBER)
    (n6 ISA NUMBER)
    (n7 ISA NUMBER)
    (n8 ISA NUMBER)
    (n9 ISA NUMBER)
    (n10 ISA NUMBER)
    (n11 ISA NUMBER)
    (n12 ISA NUMBER)
    (n13 ISA NUMBER)
    (n14 ISA NUMBER)
    (n15 ISA NUMBER)
    (n16 ISA NUMBER)
    (n17 ISA NUMBER)
    (n18 ISA NUMBER)
    (a0+0 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "0" RESULT (0) ONES (0) CARRY (0))
    (a0+1 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "1" RESULT (1) ONES (1) CARRY (0))
    (a0+2 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "2" RESULT (2) ONES (2) CARRY (0))
    (a0+3 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "3" RESULT (3) ONES (3) CARRY (0))
    (a0+4 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "4" RESULT (4) ONES (4) CARRY (0))
    (a0+5 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "5" RESULT (5) ONES (5) CARRY (0))
    (a0+6 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "6" RESULT (6) ONES (6) CARRY (0))
    (a0+7 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "7" RESULT (7) ONES (7) CARRY (0))
    (a0+8 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "8" RESULT (8) ONES (8) CARRY (0))
    (a0+9 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "9" RESULT (9) ONES (9) CARRY (0))
    (a1+0 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "0" RESULT (1) ONES (1) CARRY (0))
    (a1+1 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "1" RESULT (2) ONES (2) CARRY (0))
    (a1+2 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "2" RESULT (3) ONES (3) CARRY (0))
    (a1+3 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "3" RESULT (4) ONES (4) CARRY (0))
    (a1+4 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "4" RESULT (5) ONES (5) CARRY (0))
    (a1+5 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "5" RESULT (6) ONES (6) CARRY (0))
    (a1+6 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "6" RESULT (7) ONES (7) CARRY (0))
    (a1+7 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "7" RESULT (8) ONES (8) CARRY (0))
    (a1+8 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "8" RESULT (9) ONES (9) CARRY (0))
    (a1+9 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "9" RESULT (10) ONES (0) CARRY (1))
    (a2+0 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "0" RESULT (2) ONES (2) CARRY (0))
    (a2+1 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "1" RESULT (3) ONES (3) CARRY (0))
    (a2+2 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "2" RESULT (4) ONES (4) CARRY (0))
    (a2+3 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "3" RESULT (5) ONES (5) CARRY (0))
    (a2+4 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "4" RESULT (6) ONES (6) CARRY (0))
    (a2+5 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "5" RESULT (7) ONES (7) CARRY (0))
    (a2+6 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "6" RESULT (8) ONES (8) CARRY (0))
    (a2+7 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "7" RESULT (9) ONES (9) CARRY (0))
    (a2+8 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "8" RESULT (10) ONES (0) CARRY (1))
    (a2+9 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "9" RESULT (11) ONES (1) CARRY (1))
    (a3+0 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "0" RESULT (3) ONES (3) CARRY (0))
    (a3+1 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "1" RESULT (4) ONES (4) CARRY (0))
    (a3+2 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "2" RESULT (5) ONES (5) CARRY (0))
    (a3+3 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "3" RESULT (6) ONES (6) CARRY (0))
    (a3+4 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "4" RESULT (7) ONES (7) CARRY (0))
    (a3+5 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "5" RESULT (8) ONES (8) CARRY (0))
    (a3+6 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "6" RESULT (9) ONES (9) CARRY (0))
    (a3+7 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "7" RESULT (10) ONES (0) CARRY (1))
    (a3+8 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "8" RESULT (11) ONES (1) CARRY (1))
    (a3+9 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "9" RESULT (12) ONES (2) CARRY (1))
    (a4+0 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "0" RESULT (4) ONES (4) CARRY (0))
    (a4+1 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "1" RESULT (5) ONES (5) CARRY (0))
    (a4+2 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "2" RESULT (6) ONES (6) CARRY (0))
    (a4+3 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "3" RESULT (7) ONES (7) CARRY (0))
    (a4+4 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "4" RESULT (8) ONES (8) CARRY (0))
    (a4+5 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "5" RESULT (9) ONES (9) CARRY (0))
    (a4+6 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "6" RESULT (10) ONES (0) CARRY (1))
    (a4+7 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "7" RESULT (11) ONES (1) CARRY (1))
    (a4+8 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "8" RESULT (12) ONES (2) CARRY (1))
    (a4+9 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "9" RESULT (13) ONES (3) CARRY (1))
    (a5+0 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "0" RESULT (5) ONES (5) CARRY (0))
    (a5+1 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "1" RESULT (6) ONES (6) CARRY (0))
    (a5+2 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "2" RESULT (7) ONES (7) CARRY (0))
    (a5+3 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "3" RESULT (8) ONES (8) CARRY (0))
    (a5+4 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "4" RESULT (9) ONES (9) CARRY (0))
    (a5+5 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "5" RESULT (10) ONES (0) CARRY (1))
    (a5+6 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "6" RESULT (11) ONES (1) CARRY (1))
    (a5+7 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "7" RESULT (12) ONES (2) CARRY (1))
    (a5+8 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "8" RESULT (13) ONES (3) CARRY (1))
    (a5+9 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "9" RESULT (14) ONES (4) CARRY (1))
    (a6+0 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "0" RESULT (6) ONES (6) CARRY (0))
    (a6+1 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "1" RESULT (7) ONES (7) CARRY (0))
    (a6+2 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "2" RESULT (8) ONES (8) CARRY (0))
    (a6+3 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "3" RESULT (9) ONES (9) CARRY (0))
    (a6+4 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "4" RESULT (10) ONES (0) CARRY (1))
    (a6+5 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "5" RESULT (11) ONES (1) CARRY (1))
    (a6+6 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "6" RESULT (12) ONES (2) CARRY (1))
    (a6+7 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "7" RESULT (13) ONES (3) CARRY (1))
    (a6+8 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "8" RESULT (14) ONES (4) CARRY (1))
    (a6+9 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "9" RESULT (15) ONES (5) CARRY (1))
    (a7+0 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "0" RESULT (7) ONES (7) CARRY (0))
    (a7+1 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "1" RESULT (8) ONES (8) CARRY (0))
    (a7+2 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "2" RESULT (9) ONES (9) CARRY (0))
    (a7+3 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "3" RESULT (10) ONES (0) CARRY (1))
    (a7+4 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "4" RESULT (11) ONES (1) CARRY (1))
    (a7+5 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "5" RESULT (12) ONES (2) CARRY (1))
    (a7+6 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "6" RESULT (13) ONES (3) CARRY (1))
    (a7+7 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "7" RESULT (14) ONES (4) CARRY (1))
    (a7+8 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "8" RESULT (15) ONES (5) CARRY (1))
    (a7+9 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "9" RESULT (16) ONES (6) CARRY (1))
    (a8+0 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "0" RESULT (8) ONES (8) CARRY (0))
    (a8+1 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "1" RESULT (9) ONES (9) CARRY (0))
    (a8+2 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "2" RESULT (10) ONES (0) CARRY (1))
    (a8+3 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "3" RESULT (11) ONES (1) CARRY (1))
    (a8+4 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "4" RESULT (12) ONES (2) CARRY (1))
    (a8+5 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "5" RESULT (13) ONES (3) CARRY (1))
    (a8+6 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "6" RESULT (14) ONES (4) CARRY (1))
    (a8+7 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "7" RESULT (15) ONES (5) CARRY (1))
    (a8+8 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "8" RESULT (16) ONES (6) CARRY (1))
    (a8+9 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "9" RESULT (17) ONES (7) CARRY (1))
    (a9+0 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "0" RESULT (9) ONES (9) CARRY (0))
    (a9+1 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "1" RESULT (10) ONES (0) CARRY (1))
    (a9+2 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "2" RESULT (11) ONES (1) CARRY (1))
    (a9+3 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "3" RESULT (12) ONES (2) CARRY (1))
    (a9+4 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "4" RESULT (13) ONES (3) CARRY (1))
    (a9+5 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "5" RESULT (14) ONES (4) CARRY (1))
    (a9+6 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "6" RESULT (15) ONES (5) CARRY (1))
    (a9+7 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "7" RESULT (16) ONES (6) CARRY (1))
    (a9+8 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "8" RESULT (17) ONES (7) CARRY (1))
    (a9+9 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "9" RESULT (18) ONES (8) CARRY (1))
    (s01 ISA SUCCESSOR VALUE (0) SUCCESSOR (1))
    (s12 ISA SUCCESSOR VALUE (1) SUCCESSOR (2))
    (s23 ISA SUCCESSOR VALUE (2) SUCCESSOR (3))
    (s34 ISA SUCCESSOR VALUE (3) SUCCESSOR (4))
    (s45 ISA SUCCESSOR VALUE (4) SUCCESSOR (5))
    (s56 ISA SUCCESSOR VALUE (5) SUCCESSOR (6))
    (s67 ISA SUCCESSOR VALUE (6) SUCCESSOR (7))
    (s78 ISA SUCCESSOR VALUE (7) SUCCESSOR (8))
    (s89 ISA SUCCESSOR VALUE (8) SUCCESSOR (9))
    (addition-goal ISA arithmetic-problem operator + state find-second-ones)
    )

;;; TODO rules:
;; monitor for appearance of problems
;; - can limit to middle of screen because people will know that's where they are
;; attend to first operand
;; store first operand
;; attend to operator ? // they already know it's addition
;; store operator ? // same
;; attend to second operand
;; store second operand
;; retrieve sum
;; count up if retrieval fails // how?

  ;; Production to search for the ones place of the second addend
  (P find-second-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       find-second-ones
  ==>
    ;; perform search for right-most text
    +visual-location>
      ISA         visual-location
      :attended   nil
      kind        text
      screen-x    highest

    ;; update goal
    =goal>
      state       attend-second-ones
    )

  ;; Production to move visual attention to ones place of second addend
  (P attend-second-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       attend-second-ones

    ;; get vis-loc reference
    =visual-location>
      ISA         visual-location
      ;; grab screen-x to record it
      screen-x    =sx

    ;; check for free visual
    ?visual>
      state       free
  ==>
    ;; request to move attention to ones place of second addend
    +visual>
      ISA         move-attention
      screen-pos  =visual-location

    ;; update goal
    =goal>
      state       encode-second-ones
      second-ones-x =sx
    )

  ;; Production to encode and store the value of the ones place of the second addend
  (P encode-second-ones
    ;; chck goal state
    =goal>
      ISA         arithmetic-problem
      state       encode-second-ones

    ;; wait for visual object
    =visual>
      ISA         text
      value       =value
  ==>
    ;; update goal
    =goal>
      state       find-plus

    ;; request to store info in imaginal
    +imaginal>
      ISA         arithmetic-info
      second-ones =value
    )

  ;; Production to find the plus sign
  ;; TODO the RHS of this could just be in encode-second-ones
  (P find-plus-search
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       find-plus

  ==>
    ;; update goal
    =goal>
      state       find-plus-attend

    ;; search for plus sign
    +visual-location>
      ISA         visual-location
      kind        text
      < screen-x  current
      screen-x    highest
  )

  ;; Production to attend to location we think may be the plus
  (P find-plus-attend
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       find-plus-attend

    ;; get vis-loc
    =visual-location>
      ISA         visual-location
      kind        text

    ;; wait until visual is free
    ?visual>
      state       free
  ==>
    ;; attend to location that may be plus
    +visual>
      ISA         move-attention
      screen-pos  =visual-location

    ;; keep vis-loc
    =visual-location>

    ;; update goal
    =goal>
      state       find-plus-check
    )

  ;; Production to loop when object isn't the plus sign
  (P find-plus-check
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       find-plus-check
    
    ;; match not plus
    =visual>
      ISA         text
      - value     "+"
  ==>
    ;; do search again
    ;; search for plus sign
    +visual-location>
      ISA         visual-location
      kind        text
      < screen-x  current
      screen-x    highest

    ;; update goal
    =goal>
      state       find-plus-attend
    )

  ;; production to find ones place of the first addend
  (P find-first-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       find-plus-check

    ;; check vis-loc for location
    =visual-location>
      ISA         visual-location
      kind        text
      ;; grab screen-x
      screen-x    =sx

    ;; check visual for plus
    =visual>
      ISA         text
      value       "+"
  ==>
    ;; search for right-most text left of current vis-loc
    +visual-location>
      ISA         visual-location
      < screen-x  =sx
      kind        text
      screen-x    highest

    ;; update goal
    =goal>
      state       attend-first-ones
      ;; store x-pos of plus
      plus-x      =sx
    )

  ;; attend ones place of the first addend
  (P attend-first-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       attend-first-ones

    ;; get vis-loc
    =visual-location>
      ISA         visual-location
      ;; grab screen-x to store
      screen-x    =sx

    ;; wait for visual system
    ;; TODO clear visual after last attend?
    ?visual>
      state       free
  ==>
    ;; request move-attention to ones place of first addend
    +visual>
      ISA         move-attention
      screen-pos  =visual-location

    ;; update goal
    =goal>
      state       encode-first-ones
      ;; store ones location
      first-ones-x =sx
    )

  ;; Production to encode value of ones place of first addend
  (P encode-first-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       encode-first-ones

    ;; wait for visual attention to move
    =visual>
      ISA         text
      value       =value2

    ;; match imaginal to keep info about last number there
    =imaginal>
      ISA         arithmetic-info
      second-ones =second-ones
  ==>
    ;; update goal
    =goal>
      state       add-ones

    ;; put info in imaginal buffer
    +imaginal>
      ISA         arithmetic-info
      second-ones =second-ones
      first-ones  =value2
    )

  ;; Production to start adding the ones place
  ;; TODO this should be combined into retrieve-addition-ones.
  ;; TODO no need for intermediately copying state to goal
  (P add-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       add-ones

    ;; grab imaginal buffer
    =imaginal>
      ISA         arithmetic-info
      first-ones  =first-ones
      second-ones =second-ones
  ==>
    ;; update state with values
    =goal>
      first       =first-ones
      operator    +
      second      =second-ones
      state      retrieve-addition-ones
    )

  ;; Production request addition fact retrieval
  (P retrieve-addition-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       retrieve-addition-ones
      first       =first
      operator    +
      second      =second
      result      nil
  ==>
    ;; update goal
    =goal>
      state       finish-retrieve-ones

    ;; request dm retrieval
    +retrieval>
      ISA         arithmetic
      first       =first
      second      =second
      operator    +
    )

  ;; Production to get results of addition retrieval
  (P finish-retrieve-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       finish-retrieve-ones
      first       =first
      second      =second
    ;; get retrieval results
    =retrieval>
      ISA         arithmetic
      first       =first
      operator    +
      second      =second
      ones        =ones
      carry       =carry
  ==>
    ;; store ones and carry result in goal
    =goal>
      ones        =ones
      carry       =carry
      state       find-second-tens
    )
  
  ;; Production to search for tens place of second addend
  (P find-second-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       find-second-tens
      ;; get x pos of plus and second addend ones place
      plus-x      =plus-x
      second-ones-x =second-ones-x
  ==>
    +visual-location>
      ISA         visual-location
      kind        text
      ;; search right of plus
      > screen-x  =plus-x
      ;; search left of ones place
      < screen-x  =second-ones-x

    ;; update goal
    =goal>
      state       attend-second-tens
    )
  
  ;; Production to attend to tens place of second addend
  (P attend-second-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       attend-second-tens

    ;; check for vis-loc
    =visual-location>
      ISA         visual-location
      kind        text

    ;; wait for visual to be free
    ?visual>
      state       free
  ==>
    ;; request to move attention
    +visual>
      ISA         move-attention
      screen-pos  =visual-location

    ;; update goal
    =goal>
      state       encode-second-tens
    )

  ;; Production to skip tens place of second operand if it doesn't exist
  (P find-second-tens-fail
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       attend-second-tens

    ;; check if vis-loc request failed
    ?visual-location>
      state       error

  ==>
    ;; skip to searching for first addend tens place
    ;; update goal
    =goal>
      state       find-first-tens
    )

  ;; Production to encode value of tens place of second addend
  (P encode-second-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       encode-second-tens
    
    ;; wait for attention to shift
    =visual>
      ISA         text
      value       =value
  ==>
    ;; update state
    =goal>
      state       find-first-tens

    ;; request to store value in imaginal
    +imaginal>
      ISA         arithmetic-info
      second-tens =value
    )

  ;; Production to find tens place of first addend
  (P find-first-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       find-first-tens
      ;; match first ones place to compare
      first-ones-x  =first-ones-x
  ==>
    ;; search for location left of first ones place
    +visual-location>
      ISA         visual-location
      kind        text
      < screen-x   =first-ones-x

    ;; update goal
    =goal>
      state       attend-first-tens
    )

  ;; Production to attend to tens place of first addend
  (P attend-first-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       attend-first-tens
    
    ;; get vis-loc
    =visual-location>
      ISA         visual-location
      kind        text

    ;; wait for visual to be free
    ?visual>
      state       free
  ==>
    ;; request move-attention
    +visual>
      ISA         move-attention
      screen-pos  =visual-location
      
    ;; update goal
    =goal>
      state       encode-first-tens
    )

  ;; Production to detect when there is no tens place for first addend
  (P find-first-tens-fail
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       attend-first-tens
    
    ;; check if vis-loc search failed
    ?visual-location>
      state       error
  ==>
    ;; skip to adding tens places
    ;; update goal
    =goal>
      state       add-tens
    )

  ;; Production to encode value in tens place of first addend
  (P encode-first-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       encode-first-tens

    ;; wait for visual attention
    =visual>
      ISA         text
      value       =value

    ;; match imaginal to keep state
    ;; TODO case when second-tens was not found
    =imaginal>
      ISA         arithmetic-info
      second-tens =second-tens
  ==>
    ;; update goal
    =goal>
      state       add-tens
    
    ;; request to store value in imaginal
    +imaginal>
      ISA         arithmetic-info
      second-tens =second-tens
      first-tens  =value
    )

  ;; Production to start adding tens values
  (P add-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       add-tens
    
    ;; get info from imaginal
    =imaginal>
      ISA         arithmetic-info
      first-tens  =first-tens
      second-tens =second-tens
  ==>
    ;; update state with values
    =goal>
      first       =first-tens
      second      =second-tens
      state       retrieve-addition-tens
    )

  ;; Production to request retrieval of tens addition fact
  (P retrieve-addition-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       retrieve-addition-tens
      first       =first
      second      =second
  ==>
    ;; update goal
    =goal>
      state    finish-retrieve-tens

    ;; request dm retrieval
    +retrieval>
      ISA         arithmetic
      first       =first
      second      =second
      operator    +
    )

  ;; Production to get results of addition retrieval
  (P finish-retrieve-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       finish-retrieve-tens
      first       =first
      second      =second

    ;; get retrieval results
    =retrieval>
      ISA         arithmetic
      first       =first
      operator    +
      second      =second
      ones        =ones
  ==>
    ;; store ones and carry result in goal
    =goal>
      tens        =ones
      state       check-carry
    )

  ;; Production to increment tens if there is a carry
  (P check-carry
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       check-carry
      ;; check that there is a carry
      carry       (1)
      ;; match tens sum
      tens        =tens
  ==>
    ;; request retrieval of successor
    +retrieval>
      ISA         SUCCESSOR
      value       =tens

    ;; update goal
    =goal>
      state       increment-tens
    )

  ;; Production to move on to response if there is no carry
  (P check-no-carry
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       check-carry
      ;; no carry
      carry       (0)
  ==>
    =goal>
      state       respond
    )

  ;; Production to set new tens place after incrementing for carry
  (P increment-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       check-carry

    ;; match retrieval
    =retrieval>
      ISA         SUCCESSOR
      SUCCESSOR   =value
  ==>
    ;; set new tens value
    ;; update goal
    =goal>
      tens        =value
      state       response
    )

  (goal-focus addition-goal)
)
