
(clear-all)

(defun do-arithmetic-trial (&optional (addend1 14) (addend2 7))

  (reset)
  
  (let* ((lis (permute-list '("1" "2" "3" "4" "5" "6" "7")))
    (answers nil)   
    (text1 (format nil "~a" addend1))
    (text2 (format nil "~a" addend2))
    (window (open-exp-window "Addition Problem"
      :visible t
      :width 300
      :height 300)))
  (add-text-to-exp-window :text text1
    :width 25
    :x 75
    :y 140)
  (add-text-to-exp-window :text "+"
    :width 5
    :x 100
    :y 140)
  (add-text-to-exp-window :text text2
    :width 25
    :x 110
    :y 140)

  (install-device window)
  (schedule-event-relative 5 'clear-screen)

  (proc-display)
  (run 30)))


(define-model addition

  (sgp :esc t :lf .05)
  (sgp :v t :show-focus t :trace-detail high)

  (chunk-type arithmetic first operator second result ones carry)
  (chunk-type arithmetic-problem first-ones operator second-ones first-tens second-tens result state ones carry tens second-ones-x first-ones-x)
  (chunk-type arithmetic-info first-tens first-ones second-tens second-ones)
  (chunk-type successor value successor)
  (chunk-type number ones tens value)

  (add-dm
    (+ ISA CHUNK)
    (* ISA CHUNK)
    (a0+0 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "0" RESULT "0" ONES "0" CARRY "0")
    (a0+1 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "1" RESULT "1" ONES "1" CARRY "0")
    (a0+2 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "2" RESULT "2" ONES "2" CARRY "0")
    (a0+3 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "3" RESULT "3" ONES "3" CARRY "0")
    (a0+4 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "4" RESULT "4" ONES "4" CARRY "0")
    (a0+5 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "5" RESULT "5" ONES "5" CARRY "0")
    (a0+6 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "6" RESULT "6" ONES "6" CARRY "0")
    (a0+7 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "7" RESULT "7" ONES "7" CARRY "0")
    (a0+8 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "8" RESULT "8" ONES "8" CARRY "0")
    (a0+9 ISA ARITHMETIC FIRST "0" OPERATOR + SECOND "9" RESULT "9" ONES "9" CARRY "0")
    (a1+0 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "0" RESULT "1" ONES "1" CARRY "0")
    (a1+1 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "1" RESULT "2" ONES "2" CARRY "0")
    (a1+2 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "2" RESULT "3" ONES "3" CARRY "0")
    (a1+3 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "3" RESULT "4" ONES "4" CARRY "0")
    (a1+4 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "4" RESULT "5" ONES "5" CARRY "0")
    (a1+5 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "5" RESULT "6" ONES "6" CARRY "0")
    (a1+6 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "6" RESULT "7" ONES "7" CARRY "0")
    (a1+7 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "7" RESULT "8" ONES "8" CARRY "0")
    (a1+8 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "8" RESULT "9" ONES "9" CARRY "0")
    (a1+9 ISA ARITHMETIC FIRST "1" OPERATOR + SECOND "9" RESULT "10" ONES "0" CARRY "1")
    (a2+0 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "0" RESULT "2" ONES "2" CARRY "0")
    (a2+1 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "1" RESULT "3" ONES "3" CARRY "0")
    (a2+2 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "2" RESULT "4" ONES "4" CARRY "0")
    (a2+3 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "3" RESULT "5" ONES "5" CARRY "0")
    (a2+4 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "4" RESULT "6" ONES "6" CARRY "0")
    (a2+5 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "5" RESULT "7" ONES "7" CARRY "0")
    (a2+6 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "6" RESULT "8" ONES "8" CARRY "0")
    (a2+7 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "7" RESULT "9" ONES "9" CARRY "0")
    (a2+8 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "8" RESULT "10" ONES "0" CARRY "1")
    (a2+9 ISA ARITHMETIC FIRST "2" OPERATOR + SECOND "9" RESULT "11" ONES "1" CARRY "1")
    (a3+0 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "0" RESULT "3" ONES "3" CARRY "0")
    (a3+1 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "1" RESULT "4" ONES "4" CARRY "0")
    (a3+2 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "2" RESULT "5" ONES "5" CARRY "0")
    (a3+3 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "3" RESULT "6" ONES "6" CARRY "0")
    (a3+4 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "4" RESULT "7" ONES "7" CARRY "0")
    (a3+5 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "5" RESULT "8" ONES "8" CARRY "0")
    (a3+6 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "6" RESULT "9" ONES "9" CARRY "0")
    (a3+7 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "7" RESULT "10" ONES "0" CARRY "1")
    (a3+8 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "8" RESULT "11" ONES "1" CARRY "1")
    (a3+9 ISA ARITHMETIC FIRST "3" OPERATOR + SECOND "9" RESULT "12" ONES "2" CARRY "1")
    (a4+0 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "0" RESULT "4" ONES "4" CARRY "0")
    (a4+1 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "1" RESULT "5" ONES "5" CARRY "0")
    (a4+2 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "2" RESULT "6" ONES "6" CARRY "0")
    (a4+3 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "3" RESULT "7" ONES "7" CARRY "0")
    (a4+4 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "4" RESULT "8" ONES "8" CARRY "0")
    (a4+5 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "5" RESULT "9" ONES "9" CARRY "0")
    (a4+6 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "6" RESULT "10" ONES "0" CARRY "1")
    (a4+7 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "7" RESULT "11" ONES "1" CARRY "1")
    (a4+8 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "8" RESULT "12" ONES "2" CARRY "1")
    (a4+9 ISA ARITHMETIC FIRST "4" OPERATOR + SECOND "9" RESULT "13" ONES "3" CARRY "1")
    (a5+0 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "0" RESULT "5" ONES "5" CARRY "0")
    (a5+1 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "1" RESULT "6" ONES "6" CARRY "0")
    (a5+2 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "2" RESULT "7" ONES "7" CARRY "0")
    (a5+3 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "3" RESULT "8" ONES "8" CARRY "0")
    (a5+4 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "4" RESULT "9" ONES "9" CARRY "0")
    (a5+5 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "5" RESULT "10" ONES "0" CARRY "1")
    (a5+6 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "6" RESULT "11" ONES "1" CARRY "1")
    (a5+7 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "7" RESULT "12" ONES "2" CARRY "1")
    (a5+8 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "8" RESULT "13" ONES "3" CARRY "1")
    (a5+9 ISA ARITHMETIC FIRST "5" OPERATOR + SECOND "9" RESULT "14" ONES "4" CARRY "1")
    (a6+0 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "0" RESULT "6" ONES "6" CARRY "0")
    (a6+1 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "1" RESULT "7" ONES "7" CARRY "0")
    (a6+2 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "2" RESULT "8" ONES "8" CARRY "0")
    (a6+3 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "3" RESULT "9" ONES "9" CARRY "0")
    (a6+4 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "4" RESULT "10" ONES "0" CARRY "1")
    (a6+5 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "5" RESULT "11" ONES "1" CARRY "1")
    (a6+6 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "6" RESULT "12" ONES "2" CARRY "1")
    (a6+7 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "7" RESULT "13" ONES "3" CARRY "1")
    (a6+8 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "8" RESULT "14" ONES "4" CARRY "1")
    (a6+9 ISA ARITHMETIC FIRST "6" OPERATOR + SECOND "9" RESULT "15" ONES "5" CARRY "1")
    (a7+0 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "0" RESULT "7" ONES "7" CARRY "0")
    (a7+1 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "1" RESULT "8" ONES "8" CARRY "0")
    (a7+2 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "2" RESULT "9" ONES "9" CARRY "0")
    (a7+3 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "3" RESULT "10" ONES "0" CARRY "1")
    (a7+4 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "4" RESULT "11" ONES "1" CARRY "1")
    (a7+5 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "5" RESULT "12" ONES "2" CARRY "1")
    (a7+6 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "6" RESULT "13" ONES "3" CARRY "1")
    (a7+7 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "7" RESULT "14" ONES "4" CARRY "1")
    (a7+8 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "8" RESULT "15" ONES "5" CARRY "1")
    (a7+9 ISA ARITHMETIC FIRST "7" OPERATOR + SECOND "9" RESULT "16" ONES "6" CARRY "1")
    (a8+0 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "0" RESULT "8" ONES "8" CARRY "0")
    (a8+1 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "1" RESULT "9" ONES "9" CARRY "0")
    (a8+2 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "2" RESULT "10" ONES "0" CARRY "1")
    (a8+3 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "3" RESULT "11" ONES "1" CARRY "1")
    (a8+4 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "4" RESULT "12" ONES "2" CARRY "1")
    (a8+5 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "5" RESULT "13" ONES "3" CARRY "1")
    (a8+6 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "6" RESULT "14" ONES "4" CARRY "1")
    (a8+7 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "7" RESULT "15" ONES "5" CARRY "1")
    (a8+8 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "8" RESULT "16" ONES "6" CARRY "1")
    (a8+9 ISA ARITHMETIC FIRST "8" OPERATOR + SECOND "9" RESULT "17" ONES "7" CARRY "1")
    (a9+0 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "0" RESULT "9" ONES "9" CARRY "0")
    (a9+1 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "1" RESULT "10" ONES "0" CARRY "1")
    (a9+2 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "2" RESULT "11" ONES "1" CARRY "1")
    (a9+3 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "3" RESULT "12" ONES "2" CARRY "1")
    (a9+4 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "4" RESULT "13" ONES "3" CARRY "1")
    (a9+5 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "5" RESULT "14" ONES "4" CARRY "1")
    (a9+6 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "6" RESULT "15" ONES "5" CARRY "1")
    (a9+7 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "7" RESULT "16" ONES "6" CARRY "1")
    (a9+8 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "8" RESULT "17" ONES "7" CARRY "1")
    (a9+9 ISA ARITHMETIC FIRST "9" OPERATOR + SECOND "9" RESULT "18" ONES "8" CARRY "1")
    (n00 ISA NUMBER VALUE "0" ONES "0" TENS nil)
    (n01 ISA NUMBER VALUE "1" ONES "1" TENS nil)
    (n02 ISA NUMBER VALUE "2" ONES "2" TENS nil)
    (n03 ISA NUMBER VALUE "3" ONES "3" TENS nil)
    (n04 ISA NUMBER VALUE "4" ONES "4" TENS nil)
    (n05 ISA NUMBER VALUE "5" ONES "5" TENS nil)
    (n06 ISA NUMBER VALUE "6" ONES "6" TENS nil)
    (n07 ISA NUMBER VALUE "7" ONES "7" TENS nil)
    (n08 ISA NUMBER VALUE "8" ONES "8" TENS nil)
    (n09 ISA NUMBER VALUE "9" ONES "9" TENS nil)
    (n10 ISA NUMBER VALUE "10" ONES "0" TENS "1")
    (n11 ISA NUMBER VALUE "11" ONES "1" TENS "1")
    (n12 ISA NUMBER VALUE "12" ONES "2" TENS "1")
    (n13 ISA NUMBER VALUE "13" ONES "3" TENS "1")
    (n14 ISA NUMBER VALUE "14" ONES "4" TENS "1")
    (n15 ISA NUMBER VALUE "15" ONES "5" TENS "1")
    (n16 ISA NUMBER VALUE "16" ONES "6" TENS "1")
    (n17 ISA NUMBER VALUE "17" ONES "7" TENS "1")
    (n18 ISA NUMBER VALUE "18" ONES "8" TENS "1")
    (n19 ISA NUMBER VALUE "19" ONES "9" TENS "1")
    (n20 ISA NUMBER VALUE "20" ONES "0" TENS "2")
    (n21 ISA NUMBER VALUE "21" ONES "1" TENS "2")
    (n22 ISA NUMBER VALUE "22" ONES "2" TENS "2")
    (n23 ISA NUMBER VALUE "23" ONES "3" TENS "2")
    (n24 ISA NUMBER VALUE "24" ONES "4" TENS "2")
    (n25 ISA NUMBER VALUE "25" ONES "5" TENS "2")
    (n26 ISA NUMBER VALUE "26" ONES "6" TENS "2")
    (n27 ISA NUMBER VALUE "27" ONES "7" TENS "2")
    (n28 ISA NUMBER VALUE "28" ONES "8" TENS "2")
    (n29 ISA NUMBER VALUE "29" ONES "9" TENS "2")
    (n30 ISA NUMBER VALUE "30" ONES "0" TENS "3")
    (n31 ISA NUMBER VALUE "31" ONES "1" TENS "3")
    (n32 ISA NUMBER VALUE "32" ONES "2" TENS "3")
    (n33 ISA NUMBER VALUE "33" ONES "3" TENS "3")
    (n34 ISA NUMBER VALUE "34" ONES "4" TENS "3")
    (n35 ISA NUMBER VALUE "35" ONES "5" TENS "3")
    (n36 ISA NUMBER VALUE "36" ONES "6" TENS "3")
    (n37 ISA NUMBER VALUE "37" ONES "7" TENS "3")
    (n38 ISA NUMBER VALUE "38" ONES "8" TENS "3")
    (n39 ISA NUMBER VALUE "39" ONES "9" TENS "3")
    (n40 ISA NUMBER VALUE "40" ONES "0" TENS "4")
    (n41 ISA NUMBER VALUE "41" ONES "1" TENS "4")
    (n42 ISA NUMBER VALUE "42" ONES "2" TENS "4")
    (n43 ISA NUMBER VALUE "43" ONES "3" TENS "4")
    (n44 ISA NUMBER VALUE "44" ONES "4" TENS "4")
    (n45 ISA NUMBER VALUE "45" ONES "5" TENS "4")
    (n46 ISA NUMBER VALUE "46" ONES "6" TENS "4")
    (n47 ISA NUMBER VALUE "47" ONES "7" TENS "4")
    (n48 ISA NUMBER VALUE "48" ONES "8" TENS "4")
    (n49 ISA NUMBER VALUE "49" ONES "9" TENS "4")
    (n50 ISA NUMBER VALUE "50" ONES "0" TENS "5")
    (n51 ISA NUMBER VALUE "51" ONES "1" TENS "5")
    (n52 ISA NUMBER VALUE "52" ONES "2" TENS "5")
    (n53 ISA NUMBER VALUE "53" ONES "3" TENS "5")
    (n54 ISA NUMBER VALUE "54" ONES "4" TENS "5")
    (n55 ISA NUMBER VALUE "55" ONES "5" TENS "5")
    (n56 ISA NUMBER VALUE "56" ONES "6" TENS "5")
    (n57 ISA NUMBER VALUE "57" ONES "7" TENS "5")
    (n58 ISA NUMBER VALUE "58" ONES "8" TENS "5")
    (n59 ISA NUMBER VALUE "59" ONES "9" TENS "5")
    (n60 ISA NUMBER VALUE "60" ONES "0" TENS "6")
    (n61 ISA NUMBER VALUE "61" ONES "1" TENS "6")
    (n62 ISA NUMBER VALUE "62" ONES "2" TENS "6")
    (n63 ISA NUMBER VALUE "63" ONES "3" TENS "6")
    (n64 ISA NUMBER VALUE "64" ONES "4" TENS "6")
    (n65 ISA NUMBER VALUE "65" ONES "5" TENS "6")
    (n66 ISA NUMBER VALUE "66" ONES "6" TENS "6")
    (n67 ISA NUMBER VALUE "67" ONES "7" TENS "6")
    (n68 ISA NUMBER VALUE "68" ONES "8" TENS "6")
    (n69 ISA NUMBER VALUE "69" ONES "9" TENS "6")
    (n70 ISA NUMBER VALUE "70" ONES "0" TENS "7")
    (n71 ISA NUMBER VALUE "71" ONES "1" TENS "7")
    (n72 ISA NUMBER VALUE "72" ONES "2" TENS "7")
    (n73 ISA NUMBER VALUE "73" ONES "3" TENS "7")
    (n74 ISA NUMBER VALUE "74" ONES "4" TENS "7")
    (n75 ISA NUMBER VALUE "75" ONES "5" TENS "7")
    (n76 ISA NUMBER VALUE "76" ONES "6" TENS "7")
    (n77 ISA NUMBER VALUE "77" ONES "7" TENS "7")
    (n78 ISA NUMBER VALUE "78" ONES "8" TENS "7")
    (n79 ISA NUMBER VALUE "79" ONES "9" TENS "7")
    (n80 ISA NUMBER VALUE "80" ONES "0" TENS "8")
    (n81 ISA NUMBER VALUE "81" ONES "1" TENS "8")
    (n82 ISA NUMBER VALUE "82" ONES "2" TENS "8")
    (n83 ISA NUMBER VALUE "83" ONES "3" TENS "8")
    (n84 ISA NUMBER VALUE "84" ONES "4" TENS "8")
    (n85 ISA NUMBER VALUE "85" ONES "5" TENS "8")
    (n86 ISA NUMBER VALUE "86" ONES "6" TENS "8")
    (n87 ISA NUMBER VALUE "87" ONES "7" TENS "8")
    (n88 ISA NUMBER VALUE "88" ONES "8" TENS "8")
    (n89 ISA NUMBER VALUE "89" ONES "9" TENS "8")
    (n90 ISA NUMBER VALUE "90" ONES "0" TENS "9")
    (n91 ISA NUMBER VALUE "91" ONES "1" TENS "9")
    (n92 ISA NUMBER VALUE "92" ONES "2" TENS "9")
    (n93 ISA NUMBER VALUE "93" ONES "3" TENS "9")
    (n94 ISA NUMBER VALUE "94" ONES "4" TENS "9")
    (n95 ISA NUMBER VALUE "95" ONES "5" TENS "9")
    (n96 ISA NUMBER VALUE "96" ONES "6" TENS "9")
    (n97 ISA NUMBER VALUE "97" ONES "7" TENS "9")
    (n98 ISA NUMBER VALUE "98" ONES "8" TENS "9")
    (n99 ISA NUMBER VALUE "99" ONES "9" TENS "9")
    (s01 ISA SUCCESSOR VALUE "0" SUCCESSOR "1")
    (s12 ISA SUCCESSOR VALUE "1" SUCCESSOR "2")
    (s23 ISA SUCCESSOR VALUE "2" SUCCESSOR "3")
    (s34 ISA SUCCESSOR VALUE "3" SUCCESSOR "4")
    (s45 ISA SUCCESSOR VALUE "4" SUCCESSOR "5")
    (s56 ISA SUCCESSOR VALUE "5" SUCCESSOR "6")
    (s67 ISA SUCCESSOR VALUE "6" SUCCESSOR "7")
    (s78 ISA SUCCESSOR VALUE "7" SUCCESSOR "8")
    (s89 ISA SUCCESSOR VALUE "8" SUCCESSOR "9")
    (addition-goal ISA arithmetic-problem operator + state find-second)
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

  ;; Production to search for the second addend
  (P find-second
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       find-second
  ==>
    ;; perform search for right-most text
    +visual-location>
      ISA         visual-location
      :attended   nil
      kind        text
      screen-x    highest

    ;; update goal
    =goal>
      state       attend-second
    )

  ;; Production to move visual attention to second addend
  (P attend-second
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       attend-second

    ;; get vis-loc reference
    =visual-location>
      ISA         visual-location

    ;; check for free visual
    ?visual>
      state       free
  ==>
    ;; request to move attention to second addend
    +visual>
      ISA         move-attention
      screen-pos  =visual-location

    ;; update goal
    =goal>
      state       encode-second
    )

  ;; Production to encode and store the value of the second addend
  (P encode-second
    ;; chck goal state
    =goal>
      ISA         arithmetic-problem
      state       encode-second

    ;; wait for visual object
    =visual>
      ISA         text
      value       =value

    ?retrieval>
      state       free
  ==>
    ;; update goal
    =goal>
      state       find-first

    ;; request number info from retrieval
    +retrieval>
      ISA         number
      value       =value
    )

  ;; Production to get number chunk and store tens and ones
  (P store-second
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      second-ones nil
    
    ;; wait for retrieval
    =retrieval>
      ISA         number
      ones        =ones
      tens        =tens
  ==>
    ;; update goal
    =goal>
      second-ones =ones
      second-tens =tens
    )

  ;; Production to get number chunk and store tens and ones when tens is nil
  (P store-second-nil-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      second-ones nil
      
    ;; wait for retrieval
    =retrieval>
      ISA         number
      ones        =ones
      tens        nil
  ==>
    ;; update goal
    =goal>
      second-ones =ones
    )

  ;; production to find the first addend
  (P find-first
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       find-first

  ==>
    ;; search for right-most text left of current vis-loc
    ;; TODO this can just be leftmost now.
    +visual-location>
      ISA         visual-location
      ;; plus sign is at 105
      < screen-x  105
      kind        text
      screen-x    highest

    ;; update goal
    =goal>
      state       attend-first
    )

  ;; attend the first addend
  (P attend-first
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       attend-first

    ;; get vis-loc
    =visual-location>
      ISA         visual-location

    ;; wait for visual system
    ;; TODO clear visual after last attend?
    ?visual>
      state       free
  ==>
    ;; request move-attention to first addend
    +visual>
      ISA         move-attention
      screen-pos  =visual-location

    ;; update goal
    =goal>
      state       encode-first
    )

  ;; Production to encode value of first addend
  (P encode-first-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       encode-first

    ;; wait for visual attention to move
    =visual>
      ISA         text
      value       =value

    ;; make sure retrieval is free
    ?retrieval>
      state       free
  ==>
    ;; request the dm of the number info
    +retrieval>
      ISA         number
      value       =value

    ;; update goal
    =goal>
      state       store-first
    )

  ;; Production to get the number info from dm and store in goal
  (P store-first
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       store-first
    
    ;; wait for retrieval
    =retrieval>
      ISA         number
      ones        =ones
      tens        =tens
  ==>
    ;; update goal
    =goal>
      state       retrieve-addition-ones
      first-ones  =ones
      first-tens  =tens
    )

  ;; Production to get the number info from dm and store in goal when tens is nil
  (P store-first-tens-nil
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       store-first
    
    ;; wait for retrieval
    =retrieval>
      ISA         number
      ones        =ones
      tens        nil
  ==>
    ;; update goal
    =goal>
      state       retrieve-addition-ones
      first-ones  =ones
  )

  ;; Production request addition fact retrieval
  (P retrieve-addition-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       retrieve-addition-ones
      first-ones  =first
      operator    +
      second-ones =second
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
      first-ones  =first
      second-ones      =second
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
      state       add-tens
    )
  
  ;; Production to add tens when there are none
  (P add-tens-nil-nil
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       add-tens
      ;; check that neither has tens place
      first-tens  nil
      second-tens nil
  ==>
    ;; update goal
    =goal>
      tens        "0"
      state       check-carry
    )
  
  ;; Production to add tens when there is only tens place in first addend
  (P add-tens-first-nil
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       add-tens
      ;; check that only first has tens
      - first-tens nil
      first-tens  =first-tens
      second-tens nil
  ==>
    ;; update goal
    =goal>
      tens        =first-tens
      state       check-carry
    )

  ;; Production to add tens when there is only tens place in second addend
  (P add-tens-nil-second
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       add-tens
      ;; check that only second has tens
      first-tens  nil
      second-tens =second-tens
      - second-tens nil
  ==>
    ;; update goal
    =goal>
      tens        =second-tens
      state       check-carry
    )

  ;; Production to start adding tens values
  (P add-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       add-tens
      first-tens  =first-tens
      second-tens =second-tens
    
  ==>
    ;; update goal
    =goal>
      state       retrieve-addition-tens
    )

  ;; Production to request retrieval of tens addition fact
  (P retrieve-addition-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       retrieve-addition-tens
      first-tens  =first
      second-tens =second
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
      first-tens  =first
      second-tens =second

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
      carry       "1"
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
      carry       "0"
  ==>
    =goal>
      state       response
    )

  ;; Production to set new tens place after incrementing for carry
  (P increment-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       increment-tens

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

  ;; Production to speak the tens value
  (P respond-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       response
      ;; match tens value
      tens        =tens
      ;; make sure tens not "0"
      - tens      "0"

    ;; check for vocal preparation free
    ?vocal>
      preparation free
  ==>
    ;; vocalize the tens value
    +vocal>
      ISA         speak
      string      =tens

    ;; update goal
    =goal>
      state       response-ones
    )

  ;; Production to only speak ones value if tens is 0
  (P respons-ones-no-tens
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       response
      ;; make sure tens is 0
      tens        "0"
      ;; match ones
      ones        =ones

    ;; check for vocal prep free
    ?vocal>
      preparation free
  ==>
    ;; vocalize ones value
    +vocal>
      ISA         speak
      string      =ones

    ;; update goal
    =goal>
      state       done
    )

  ;; Production to speak the ones value
  (P respond-ones
    ;; check goal state
    =goal>
      ISA         arithmetic-problem
      state       response-ones
      ;; match ones value
      ones        =ones

    ;; wait for vocal prep free
    ?vocal>
      preparation free
  ==>
    ;; vocalize ones value
    +vocal>
      ISA         speak
      string      =ones
    ;; update goal
    =goal>
      state       done
    )

  (goal-focus addition-goal)
)
