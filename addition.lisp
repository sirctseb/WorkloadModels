
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
  (chunk-type arithmetic-problem first operator second result state ones carry tens)
  (chunk-type arithmetic-info first-tens first-ones second-tens second-ones)
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
    (addition-goal ISA arithmetic-problem operator + state read-first-operand)
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
      type        text
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
  (P find-plus
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       find-plus
  ==>
    ;; update goal
    =goal>
      state       find-first-ones

    ;; search for plus sign
    +visual-location>
      ISA         visual-location
      type        text
      value       +
  )

  ;; production to find ones place of the first addend
  (P find-first-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       find-first-ones

    ;; check vis-loc
    =visual-location>
      ISA         visual-location
      type        text
      value       +
      ;; grab screen-x
      screen-x    =sx
  ==>
    ;; search for right-most text left of current vis-loc
    +visual-location>
      ISA         visual-location
      < screen-x  =sx
      type        text
      screen-x    highest

    ;; update goal
    =goal>
      state       attend-first-ones
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
      state       encode-second-ones
    )

  ;; Production to encode value of ones place of first addend
  (P encode-second-operand
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       attend-second-operand

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
  (P add-operands
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
      operator    finish-retrieve-ones

    ;; request dm retrieval
    +retrieval>
      ISA         arithmetic
      first       =first
      second      =second
      operator    +
    )

  (P finish-retrieve
    =goal>
      ISA         arithmetic-problem
      first       =first
      operator    prevent-from-matching
      second      =second
      result      nil
    =retrieval>
      ISA         arithmetic
      first       =first
      operator    +
      second      =second
      result      =answer
  ==>
    =goal>
      result      =answer
      !output!       (=answer)
    )

  (goal-focus addition-goal)
)
