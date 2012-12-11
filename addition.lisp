
(clear-all)

(define-model addition

(sgp :esc t :lf .05)

(chunk-type arithmetic first operator second result)
(chunk-type arithmetic-problem first operator second result state)
(chunk-type number)

(add-dm
 ;;; Operators + and * are simple wmes.
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
 (a0+0 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 0 RESULT (0))
 (a0+1 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 1 RESULT (1))
 (a0+2 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 2 RESULT (2))
 (a0+3 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 3 RESULT (3))
 (a0+4 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 4 RESULT (4))
 (a0+5 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 5 RESULT (5))
 (a0+6 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 6 RESULT (6))
 (a0+7 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 7 RESULT (7))
 (a0+8 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 8 RESULT (8))
 (a0+9 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 9 RESULT (9))
 (a1+0 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 0 RESULT (1))
 (a1+1 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 1 RESULT (2))
 (a1+2 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 2 RESULT (3))
 (a1+3 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 3 RESULT (4))
 (a1+4 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 4 RESULT (5))
 (a1+5 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 5 RESULT (6))
 (a1+6 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 6 RESULT (7))
 (a1+7 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 7 RESULT (8))
 (a1+8 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 8 RESULT (9))
 (a1+9 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 9 RESULT (10))
 (a2+0 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 0 RESULT (2))
 (a2+1 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 1 RESULT (3))
 (a2+2 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 2 RESULT (4))
 (a2+3 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 3 RESULT (5))
 (a2+4 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 4 RESULT (6))
 (a2+5 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 5 RESULT (7))
 (a2+6 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 6 RESULT (8))
 (a2+7 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 7 RESULT (9))
 (a2+8 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 8 RESULT (10))
 (a2+9 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 9 RESULT (11))
 (a3+0 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 0 RESULT (3))
 (a3+1 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 1 RESULT (4))
 (a3+2 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 2 RESULT (5))
 (a3+3 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 3 RESULT (6))
 (a3+4 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 4 RESULT (7))
 (a3+5 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 5 RESULT (8))
 (a3+6 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 6 RESULT (9))
 (a3+7 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 7 RESULT (10))
 (a3+8 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 8 RESULT (11))
 (a3+9 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 9 RESULT (12))
 (a4+0 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 0 RESULT (4))
 (a4+1 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 1 RESULT (5))
 (a4+2 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 2 RESULT (6))
 (a4+3 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 3 RESULT (7))
 (a4+4 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 4 RESULT (8))
 (a4+5 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 5 RESULT (9))
 (a4+6 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 6 RESULT (10))
 (a4+7 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 7 RESULT (11))
 (a4+8 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 8 RESULT (12))
 (a4+9 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 9 RESULT (13))
 (a5+0 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 0 RESULT (5))
 (a5+1 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 1 RESULT (6))
 (a5+2 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 2 RESULT (7))
 (a5+3 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 3 RESULT (8))
 (a5+4 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 4 RESULT (9))
 (a5+5 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 5 RESULT (10))
 (a5+6 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 6 RESULT (11))
 (a5+7 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 7 RESULT (12))
 (a5+8 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 8 RESULT (13))
 (a5+9 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 9 RESULT (14))
 (a6+0 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 0 RESULT (6))
 (a6+1 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 1 RESULT (7))
 (a6+2 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 2 RESULT (8))
 (a6+3 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 3 RESULT (9))
 (a6+4 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 4 RESULT (10))
 (a6+5 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 5 RESULT (11))
 (a6+6 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 6 RESULT (12))
 (a6+7 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 7 RESULT (13))
 (a6+8 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 8 RESULT (14))
 (a6+9 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 9 RESULT (15))
 (a7+0 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 0 RESULT (7))
 (a7+1 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 1 RESULT (8))
 (a7+2 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 2 RESULT (9))
 (a7+3 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 3 RESULT (10))
 (a7+4 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 4 RESULT (11))
 (a7+5 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 5 RESULT (12))
 (a7+6 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 6 RESULT (13))
 (a7+7 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 7 RESULT (14))
 (a7+8 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 8 RESULT (15))
 (a7+9 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 9 RESULT (16))
 (a8+0 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 0 RESULT (8))
 (a8+1 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 1 RESULT (9))
 (a8+2 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 2 RESULT (10))
 (a8+3 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 3 RESULT (11))
 (a8+4 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 4 RESULT (12))
 (a8+5 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 5 RESULT (13))
 (a8+6 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 6 RESULT (14))
 (a8+7 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 7 RESULT (15))
 (a8+8 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 8 RESULT (16))
 (a8+9 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 9 RESULT (17))
 (a9+0 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 0 RESULT (9))
 (a9+1 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 1 RESULT (10))
 (a9+2 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 2 RESULT (11))
 (a9+3 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 3 RESULT (12))
 (a9+4 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 4 RESULT (13))
 (a9+5 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 5 RESULT (14))
 (a9+6 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 6 RESULT (15))
 (a9+7 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 7 RESULT (16))
 (a9+8 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 8 RESULT (17))
 (a9+9 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 9 RESULT (18))
 (addition-goal ISA arithmetic-problem first 2 second 5 operator +)
 )

;(P initialize-addition
;   =goal>
;      ISA         arithmetic
;      arg1        =num1
;      arg2        =num2
;      operator    +
;      result      nil
;==>
;   =goal>
;      sum         =num1
;      count       0
;   +retrieval>
;      isa        count-order
;      first      =num1
;)

;(P terminate-addition
;   =goal>
;      ISA         add
;      count       =num
;      arg2        =num
;      sum         =answer
;==>
;   =goal>
;      count       nil
;)

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

(P find-first-operand
   =goal>
      ISA         arithmetic-problem
      state       read-first-operand
==>
   +visual-location>
      ISA         visual-location
      :attended   nil
   =>goal>
      state       find-location
)

(P attend-first-operand
   =goal>
      ISA         arithmetic-problem
      state       find-location
   =>visual-location>
      ISA         visual-location
   ?visual>
      state       free
==>
   +visual>
      ISA         move-attention
      screen-pos  =visual-location
   =goal>
      state       attend-first-operand
)

(P retrieve-addition
   =goal>
      ISA         arithmetic-problem
      first       =first
      operator    +
      second      =second
      result      nil
==>
   =goal>
      operator    prevent-from-matching
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
)

;(P increment-count
;   =goal>
;      ISA         add
;      sum         =sum
;      count       =count
;   =retrieval>
;      ISA         count-order
;      first       =count
;      second      =newcount
;==>
;   =goal>
;      count       =newcount
;   +retrieval>
;      isa        count-order
;      first      =sum
;)
;
;(P increment-sum
;   =goal>
;      ISA         add
;      sum         =sum
;      count       =count
;    - arg2        =count
;   =retrieval>
;      ISA         count-order
;      first       =sum
;      second      =newsum
;==>
;   =goal>
;      sum         =newsum
;   +retrieval>
;      isa        count-order
;      first      =count
;)

(goal-focus addition-goal)
)
