(defparameter *seq-base-level* 1)
(defparameter *n-low-base-level* 3.5)
(defparameter *n-high-base-level* 2)
(defparameter *a-base-level* 2.7)

(define-model addition

  (sgp
    :esc t
    :ans 0.5)

  (set-visloc-default isa visual-location color does-not-exist)

  (chunk-type arithmetic first second result ones carry)
  (chunk-type arithmetic-problem first-ones second-ones first-tens second-tens state ones carry tens)
  (chunk-type successor value successor)
  (chunk-type number ones tens value)

  (add-dm
    (a0+0 ISA ARITHMETIC FIRST "0" SECOND "0" RESULT "0" ONES "0" CARRY "0")
    (a0+1 ISA ARITHMETIC FIRST "0" SECOND "1" RESULT "1" ONES "1" CARRY "0")
    (a0+2 ISA ARITHMETIC FIRST "0" SECOND "2" RESULT "2" ONES "2" CARRY "0")
    (a0+3 ISA ARITHMETIC FIRST "0" SECOND "3" RESULT "3" ONES "3" CARRY "0")
    (a0+4 ISA ARITHMETIC FIRST "0" SECOND "4" RESULT "4" ONES "4" CARRY "0")
    (a0+5 ISA ARITHMETIC FIRST "0" SECOND "5" RESULT "5" ONES "5" CARRY "0")
    (a0+6 ISA ARITHMETIC FIRST "0" SECOND "6" RESULT "6" ONES "6" CARRY "0")
    (a0+7 ISA ARITHMETIC FIRST "0" SECOND "7" RESULT "7" ONES "7" CARRY "0")
    (a0+8 ISA ARITHMETIC FIRST "0" SECOND "8" RESULT "8" ONES "8" CARRY "0")
    (a0+9 ISA ARITHMETIC FIRST "0" SECOND "9" RESULT "9" ONES "9" CARRY "0")
    (a1+0 ISA ARITHMETIC FIRST "1" SECOND "0" RESULT "1" ONES "1" CARRY "0")
    (a1+1 ISA ARITHMETIC FIRST "1" SECOND "1" RESULT "2" ONES "2" CARRY "0")
    (a1+2 ISA ARITHMETIC FIRST "1" SECOND "2" RESULT "3" ONES "3" CARRY "0")
    (a1+3 ISA ARITHMETIC FIRST "1" SECOND "3" RESULT "4" ONES "4" CARRY "0")
    (a1+4 ISA ARITHMETIC FIRST "1" SECOND "4" RESULT "5" ONES "5" CARRY "0")
    (a1+5 ISA ARITHMETIC FIRST "1" SECOND "5" RESULT "6" ONES "6" CARRY "0")
    (a1+6 ISA ARITHMETIC FIRST "1" SECOND "6" RESULT "7" ONES "7" CARRY "0")
    (a1+7 ISA ARITHMETIC FIRST "1" SECOND "7" RESULT "8" ONES "8" CARRY "0")
    (a1+8 ISA ARITHMETIC FIRST "1" SECOND "8" RESULT "9" ONES "9" CARRY "0")
    (a1+9 ISA ARITHMETIC FIRST "1" SECOND "9" RESULT "10" ONES "0" CARRY "1")
    (a2+0 ISA ARITHMETIC FIRST "2" SECOND "0" RESULT "2" ONES "2" CARRY "0")
    (a2+1 ISA ARITHMETIC FIRST "2" SECOND "1" RESULT "3" ONES "3" CARRY "0")
    (a2+2 ISA ARITHMETIC FIRST "2" SECOND "2" RESULT "4" ONES "4" CARRY "0")
    (a2+3 ISA ARITHMETIC FIRST "2" SECOND "3" RESULT "5" ONES "5" CARRY "0")
    (a2+4 ISA ARITHMETIC FIRST "2" SECOND "4" RESULT "6" ONES "6" CARRY "0")
    (a2+5 ISA ARITHMETIC FIRST "2" SECOND "5" RESULT "7" ONES "7" CARRY "0")
    (a2+6 ISA ARITHMETIC FIRST "2" SECOND "6" RESULT "8" ONES "8" CARRY "0")
    (a2+7 ISA ARITHMETIC FIRST "2" SECOND "7" RESULT "9" ONES "9" CARRY "0")
    (a2+8 ISA ARITHMETIC FIRST "2" SECOND "8" RESULT "10" ONES "0" CARRY "1")
    (a2+9 ISA ARITHMETIC FIRST "2" SECOND "9" RESULT "11" ONES "1" CARRY "1")
    (a3+0 ISA ARITHMETIC FIRST "3" SECOND "0" RESULT "3" ONES "3" CARRY "0")
    (a3+1 ISA ARITHMETIC FIRST "3" SECOND "1" RESULT "4" ONES "4" CARRY "0")
    (a3+2 ISA ARITHMETIC FIRST "3" SECOND "2" RESULT "5" ONES "5" CARRY "0")
    (a3+3 ISA ARITHMETIC FIRST "3" SECOND "3" RESULT "6" ONES "6" CARRY "0")
    (a3+4 ISA ARITHMETIC FIRST "3" SECOND "4" RESULT "7" ONES "7" CARRY "0")
    (a3+5 ISA ARITHMETIC FIRST "3" SECOND "5" RESULT "8" ONES "8" CARRY "0")
    (a3+6 ISA ARITHMETIC FIRST "3" SECOND "6" RESULT "9" ONES "9" CARRY "0")
    (a3+7 ISA ARITHMETIC FIRST "3" SECOND "7" RESULT "10" ONES "0" CARRY "1")
    (a3+8 ISA ARITHMETIC FIRST "3" SECOND "8" RESULT "11" ONES "1" CARRY "1")
    (a3+9 ISA ARITHMETIC FIRST "3" SECOND "9" RESULT "12" ONES "2" CARRY "1")
    (a4+0 ISA ARITHMETIC FIRST "4" SECOND "0" RESULT "4" ONES "4" CARRY "0")
    (a4+1 ISA ARITHMETIC FIRST "4" SECOND "1" RESULT "5" ONES "5" CARRY "0")
    (a4+2 ISA ARITHMETIC FIRST "4" SECOND "2" RESULT "6" ONES "6" CARRY "0")
    (a4+3 ISA ARITHMETIC FIRST "4" SECOND "3" RESULT "7" ONES "7" CARRY "0")
    (a4+4 ISA ARITHMETIC FIRST "4" SECOND "4" RESULT "8" ONES "8" CARRY "0")
    (a4+5 ISA ARITHMETIC FIRST "4" SECOND "5" RESULT "9" ONES "9" CARRY "0")
    (a4+6 ISA ARITHMETIC FIRST "4" SECOND "6" RESULT "10" ONES "0" CARRY "1")
    (a4+7 ISA ARITHMETIC FIRST "4" SECOND "7" RESULT "11" ONES "1" CARRY "1")
    (a4+8 ISA ARITHMETIC FIRST "4" SECOND "8" RESULT "12" ONES "2" CARRY "1")
    (a4+9 ISA ARITHMETIC FIRST "4" SECOND "9" RESULT "13" ONES "3" CARRY "1")
    (a5+0 ISA ARITHMETIC FIRST "5" SECOND "0" RESULT "5" ONES "5" CARRY "0")
    (a5+1 ISA ARITHMETIC FIRST "5" SECOND "1" RESULT "6" ONES "6" CARRY "0")
    (a5+2 ISA ARITHMETIC FIRST "5" SECOND "2" RESULT "7" ONES "7" CARRY "0")
    (a5+3 ISA ARITHMETIC FIRST "5" SECOND "3" RESULT "8" ONES "8" CARRY "0")
    (a5+4 ISA ARITHMETIC FIRST "5" SECOND "4" RESULT "9" ONES "9" CARRY "0")
    (a5+5 ISA ARITHMETIC FIRST "5" SECOND "5" RESULT "10" ONES "0" CARRY "1")
    (a5+6 ISA ARITHMETIC FIRST "5" SECOND "6" RESULT "11" ONES "1" CARRY "1")
    (a5+7 ISA ARITHMETIC FIRST "5" SECOND "7" RESULT "12" ONES "2" CARRY "1")
    (a5+8 ISA ARITHMETIC FIRST "5" SECOND "8" RESULT "13" ONES "3" CARRY "1")
    (a5+9 ISA ARITHMETIC FIRST "5" SECOND "9" RESULT "14" ONES "4" CARRY "1")
    (a6+0 ISA ARITHMETIC FIRST "6" SECOND "0" RESULT "6" ONES "6" CARRY "0")
    (a6+1 ISA ARITHMETIC FIRST "6" SECOND "1" RESULT "7" ONES "7" CARRY "0")
    (a6+2 ISA ARITHMETIC FIRST "6" SECOND "2" RESULT "8" ONES "8" CARRY "0")
    (a6+3 ISA ARITHMETIC FIRST "6" SECOND "3" RESULT "9" ONES "9" CARRY "0")
    (a6+4 ISA ARITHMETIC FIRST "6" SECOND "4" RESULT "10" ONES "0" CARRY "1")
    (a6+5 ISA ARITHMETIC FIRST "6" SECOND "5" RESULT "11" ONES "1" CARRY "1")
    (a6+6 ISA ARITHMETIC FIRST "6" SECOND "6" RESULT "12" ONES "2" CARRY "1")
    (a6+7 ISA ARITHMETIC FIRST "6" SECOND "7" RESULT "13" ONES "3" CARRY "1")
    (a6+8 ISA ARITHMETIC FIRST "6" SECOND "8" RESULT "14" ONES "4" CARRY "1")
    (a6+9 ISA ARITHMETIC FIRST "6" SECOND "9" RESULT "15" ONES "5" CARRY "1")
    (a7+0 ISA ARITHMETIC FIRST "7" SECOND "0" RESULT "7" ONES "7" CARRY "0")
    (a7+1 ISA ARITHMETIC FIRST "7" SECOND "1" RESULT "8" ONES "8" CARRY "0")
    (a7+2 ISA ARITHMETIC FIRST "7" SECOND "2" RESULT "9" ONES "9" CARRY "0")
    (a7+3 ISA ARITHMETIC FIRST "7" SECOND "3" RESULT "10" ONES "0" CARRY "1")
    (a7+4 ISA ARITHMETIC FIRST "7" SECOND "4" RESULT "11" ONES "1" CARRY "1")
    (a7+5 ISA ARITHMETIC FIRST "7" SECOND "5" RESULT "12" ONES "2" CARRY "1")
    (a7+6 ISA ARITHMETIC FIRST "7" SECOND "6" RESULT "13" ONES "3" CARRY "1")
    (a7+7 ISA ARITHMETIC FIRST "7" SECOND "7" RESULT "14" ONES "4" CARRY "1")
    (a7+8 ISA ARITHMETIC FIRST "7" SECOND "8" RESULT "15" ONES "5" CARRY "1")
    (a7+9 ISA ARITHMETIC FIRST "7" SECOND "9" RESULT "16" ONES "6" CARRY "1")
    (a8+0 ISA ARITHMETIC FIRST "8" SECOND "0" RESULT "8" ONES "8" CARRY "0")
    (a8+1 ISA ARITHMETIC FIRST "8" SECOND "1" RESULT "9" ONES "9" CARRY "0")
    (a8+2 ISA ARITHMETIC FIRST "8" SECOND "2" RESULT "10" ONES "0" CARRY "1")
    (a8+3 ISA ARITHMETIC FIRST "8" SECOND "3" RESULT "11" ONES "1" CARRY "1")
    (a8+4 ISA ARITHMETIC FIRST "8" SECOND "4" RESULT "12" ONES "2" CARRY "1")
    (a8+5 ISA ARITHMETIC FIRST "8" SECOND "5" RESULT "13" ONES "3" CARRY "1")
    (a8+6 ISA ARITHMETIC FIRST "8" SECOND "6" RESULT "14" ONES "4" CARRY "1")
    (a8+7 ISA ARITHMETIC FIRST "8" SECOND "7" RESULT "15" ONES "5" CARRY "1")
    (a8+8 ISA ARITHMETIC FIRST "8" SECOND "8" RESULT "16" ONES "6" CARRY "1")
    (a8+9 ISA ARITHMETIC FIRST "8" SECOND "9" RESULT "17" ONES "7" CARRY "1")
    (a9+0 ISA ARITHMETIC FIRST "9" SECOND "0" RESULT "9" ONES "9" CARRY "0")
    (a9+1 ISA ARITHMETIC FIRST "9" SECOND "1" RESULT "10" ONES "0" CARRY "1")
    (a9+2 ISA ARITHMETIC FIRST "9" SECOND "2" RESULT "11" ONES "1" CARRY "1")
    (a9+3 ISA ARITHMETIC FIRST "9" SECOND "3" RESULT "12" ONES "2" CARRY "1")
    (a9+4 ISA ARITHMETIC FIRST "9" SECOND "4" RESULT "13" ONES "3" CARRY "1")
    (a9+5 ISA ARITHMETIC FIRST "9" SECOND "5" RESULT "14" ONES "4" CARRY "1")
    (a9+6 ISA ARITHMETIC FIRST "9" SECOND "6" RESULT "15" ONES "5" CARRY "1")
    (a9+7 ISA ARITHMETIC FIRST "9" SECOND "7" RESULT "16" ONES "6" CARRY "1")
    (a9+8 ISA ARITHMETIC FIRST "9" SECOND "8" RESULT "17" ONES "7" CARRY "1")
    (a9+9 ISA ARITHMETIC FIRST "9" SECOND "9" RESULT "18" ONES "8" CARRY "1")

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

    (s01 ISA SUCCESSOR VALUE "0" SUCCESSOR "1")
    (s12 ISA SUCCESSOR VALUE "1" SUCCESSOR "2")
    (s23 ISA SUCCESSOR VALUE "2" SUCCESSOR "3")
    (s34 ISA SUCCESSOR VALUE "3" SUCCESSOR "4")

    (addition-goal ISA arithmetic-problem state find-first))

  (goal-focus addition-goal)

  (set-base-levels
    (s01 *seq-base-level*) (s12 *seq-base-level*) (s23 *seq-base-level*)
    (s34 *seq-base-level*) (s45 *seq-base-level*) (s56 *seq-base-level*)
    (s67 *seq-base-level*) (s78 *seq-base-level*) (s89 *seq-base-level*)

    (n00 *n-low-base-level*) (n01 *n-low-base-level*) (n02 *n-low-base-level*)
    (n03 *n-low-base-level*) (n04 *n-low-base-level*) (n05 *n-low-base-level*) (n06 *n-low-base-level*)
    (n07 *n-low-base-level*) (n08 *n-low-base-level*) (n09 *n-low-base-level*)
    (n10 *n-low-base-level*) (n11 *n-low-base-level*) (n12 *n-low-base-level*)

    (n13 *n-high-base-level*) (n14 *n-high-base-level*) (n15 *n-high-base-level*)
    (n16 *n-high-base-level*) (n17 *n-high-base-level*) (n18 *n-high-base-level*)
    (n19 *n-high-base-level*) (n20 *n-high-base-level*) (n21 *n-high-base-level*)
    (n22 *n-high-base-level*) (n23 *n-high-base-level*) (n24 *n-high-base-level*) (n25 *n-high-base-level*)
  )

  (set-base-levels
    (a0+0 *a-base-level*) (a0+1 *a-base-level*) (a0+2 *a-base-level*) (a0+3 *a-base-level*) (a0+4 *a-base-level*)
    (a0+5 *a-base-level*) (a0+6 *a-base-level*) (a0+7 *a-base-level*) (a0+8 *a-base-level*) (a0+9 *a-base-level*)
    (a1+0 *a-base-level*) (a1+1 *a-base-level*) (a1+2 *a-base-level*) (a1+3 *a-base-level*) (a1+4 *a-base-level*)
    (a1+5 *a-base-level*) (a1+6 *a-base-level*) (a1+7 *a-base-level*) (a1+8 *a-base-level*) (a1+9 *a-base-level*)
    (a2+0 *a-base-level*) (a2+1 *a-base-level*) (a2+2 *a-base-level*) (a2+3 *a-base-level*) (a2+4 *a-base-level*)
    (a2+5 *a-base-level*) (a2+6 *a-base-level*) (a2+7 *a-base-level*) (a2+8 *a-base-level*) (a2+9 *a-base-level*)
    (a3+0 *a-base-level*) (a3+1 *a-base-level*) (a3+2 *a-base-level*) (a3+3 *a-base-level*) (a3+4 *a-base-level*)
    (a3+5 *a-base-level*) (a3+6 *a-base-level*) (a3+7 *a-base-level*) (a3+8 *a-base-level*) (a3+9 *a-base-level*)
    (a4+0 *a-base-level*) (a4+1 *a-base-level*) (a4+2 *a-base-level*) (a4+3 *a-base-level*) (a4+4 *a-base-level*)
    (a4+5 *a-base-level*) (a4+6 *a-base-level*) (a4+7 *a-base-level*) (a4+8 *a-base-level*) (a4+9 *a-base-level*)
    (a5+0 *a-base-level*) (a5+1 *a-base-level*) (a5+2 *a-base-level*) (a5+3 *a-base-level*) (a5+4 *a-base-level*)
    (a5+5 *a-base-level*) (a5+6 *a-base-level*) (a5+7 *a-base-level*) (a5+8 *a-base-level*) (a5+9 *a-base-level*)
    (a6+0 *a-base-level*) (a6+1 *a-base-level*) (a6+2 *a-base-level*) (a6+3 *a-base-level*) (a6+4 *a-base-level*)
    (a6+5 *a-base-level*) (a6+6 *a-base-level*) (a6+7 *a-base-level*) (a6+8 *a-base-level*) (a6+9 *a-base-level*)
    (a7+0 *a-base-level*) (a7+1 *a-base-level*) (a7+2 *a-base-level*) (a7+3 *a-base-level*) (a7+4 *a-base-level*)
    (a7+5 *a-base-level*) (a7+6 *a-base-level*) (a7+7 *a-base-level*) (a7+8 *a-base-level*) (a7+9 *a-base-level*)
    (a8+0 *a-base-level*) (a8+1 *a-base-level*) (a8+2 *a-base-level*) (a8+3 *a-base-level*) (a8+4 *a-base-level*)
    (a8+5 *a-base-level*) (a8+6 *a-base-level*) (a8+7 *a-base-level*) (a8+8 *a-base-level*) (a8+9 *a-base-level*)
    (a9+0 *a-base-level*) (a9+1 *a-base-level*) (a9+2 *a-base-level*) (a9+3 *a-base-level*) (a9+4 *a-base-level*)
    (a9+5 *a-base-level*) (a9+6 *a-base-level*) (a9+7 *a-base-level*) (a9+8 *a-base-level*) (a9+9 *a-base-level*)
  )

  (P find-first
    =goal>
      ISA         arithmetic-problem
      state       find-first

    ?visual-location>
      buffer      empty
  ==>
    +visual-location>
      ISA         visual-location
      :attended   nil
      kind        text
      screen-x    lowest

    =goal>
      state       attend-first
    )

  (P attend-first
    =goal>
      ISA         arithmetic-problem
      state       attend-first

    =visual-location>
      ISA         visual-location
      kind text

    ?visual>
      state       free
      buffer      empty
  ==>
    +visual>
      ISA         move-attention
      screen-pos  =visual-location

    =goal>
      state       encode-first
  )

  (P encode-first
    =goal>
      ISA         arithmetic-problem
      state       encode-first

    ?visual>
      state       free
    =visual>
      ISA         text
      value       =value

    ?retrieval>
      state       free
      buffer      empty

  ==>
    +retrieval>
      ISA         number
      value       =value

    +visual>
      ISA         clear

    =goal>
      state       find-second
  )

  (P store-first
    =goal>
      ISA         arithmetic-problem
      first-ones nil
    
    =retrieval>
      ISA         number
      ones        =ones
      tens        =tens
  ==>
    =goal>
      first-ones =ones
      first-tens =tens
    )

  (P store-first-nil-tens
    =goal>
      ISA         arithmetic-problem
      first-ones  nil

    =retrieval>
      ISA         number
      ones        =ones
      tens        nil
  ==>
    =goal>
      first-ones =ones
  )

  (P find-second
    =goal>
      ISA arithmetic-problem
      state find-second

    ?visual-location>
      buffer  empty
    ?visual>
      state free
      buffer empty
  ==>
    +visual-location>
      ISA         visual-location
      kind        text
      screen-x    highest

    =goal>
      state attend-second
  )

  (P attend-second
    =goal>
      ISA arithmetic-problem
      state attend-second

    =visual-location>
      ISA visual-location
      kind text

    ?visual>
      buffer empty
      state free
  ==>
    +visual>
      ISA         move-attention
      screen-pos  =visual-location

    =goal>
      state encode-second
  )

  (P encode-second-ones
    =goal>
      ISA         arithmetic-problem
      state       encode-second
      - first-ones nil

    =visual>
      ISA         text
      value       =value

    ?visual>
      state free

    ?retrieval>
      state       free
      buffer      empty
  ==>
    +retrieval>
      ISA         number
      value       =value

    +visual>
      ISA         clear

    =goal>
      state       store-second
  )

  (P store-second
    =goal>
      ISA         arithmetic-problem
      state       store-second
      first-ones =first-ones
    
    =retrieval>
      ISA         number
      ones        =second-ones
      tens        =second-tens

    ?retrieval>
      state       free
  ==>
    =goal>
      state        finish-retrieve-ones
      second-ones  =second-ones
      second-tens  =second-tens

    +retrieval>
      ISA         arithmetic
      first       =first-ones
      second      =second-ones
    )

  (P store-second-tens-nil
    =goal>
      ISA         arithmetic-problem
      state       store-second
      first-ones =first-ones

    =retrieval>
      ISA         number
      ones        =second-ones
      tens        nil

    ?retrieval>
      state       free
  ==>
    +retrieval>
      ISA         arithmetic
      first       =first-ones
      second      =second-ones
    
    =goal>
      state       finish-retrieve-ones
      second-ones =second-ones
  )

  (P finish-retrieve-ones
    =goal>
      ISA         arithmetic-problem
      state       finish-retrieve-ones
      first-ones  =first
      second-ones =second

    =retrieval>
      ISA         arithmetic
      first       =first
      second      =second
      ones        =ones
      carry       =carry
  ==>
    =goal>
      ones        =ones
      carry       =carry
      state       add-tens
  )
  
  (P add-tens-nil-nil-no-carry
    =goal>
      ISA         arithmetic-problem
      state       add-tens
      first-tens  nil
      second-tens nil
      carry       "0"
  ==>
    =goal>
      tens        "0"
      state       response
  )

  (P add-tens-nil-nil-carry
    =goal>
      ISA         arithmetic-problem
      state       add-tens
      first-tens  nil
      second-tens nil
      - carry     "0"
  ==>
    =goal>
      tens        "1"
      state       response
  )
  
  (P add-tens-first-nil-no-carry
    =goal>
      ISA         arithmetic-problem
      state       add-tens
      first-tens  =first-tens
      second-tens nil
      carry       "0"
  ==>
    =goal>
      tens        =first-tens
      state       response
  )

  (P add-tens-first-nil-carry
    =goal>
      ISA         arithmetic-problem
      state       add-tens
      first-tens  =first-tens
      second-tens nil
      - carry       "0"
  ==>
    =goal>
      tens        =first-tens
      state       check-carry
  )

  (P add-tens-nil-second-no-carry
    =goal>
      ISA         arithmetic-problem
      state       add-tens
      first-tens  nil
      second-tens =second-tens
      carry       "0"
  ==>
    =goal>
      tens        =second-tens
      state       response
  )

  (P add-tens-nil-second-carry
    =goal>
      ISA         arithmetic-problem
      state       add-tens
      first-tens  nil
      second-tens =second-tens
      - carry       "0"
  ==>
    =goal>
      tens        =second-tens
      state       check-carry
  )

  (P add-tens
    =goal>
      ISA         arithmetic-problem
      state       add-tens
      first-tens  =first-tens
      second-tens =second-tens
  ==>
    =goal>
      state       retrieve-addition-tens
  )

  (P retrieve-addition-tens
    =goal>
      ISA         arithmetic-problem
      state       retrieve-addition-tens
      first-tens  =first
      second-tens =second

    ?retrieval>
      state       free
      buffer      empty
  ==>
    =goal>
      state    finish-retrieve-tens

    +retrieval>
      ISA         arithmetic
      first       =first
      second      =second
  )

  (P finish-retrieve-tens-no-carry
    =goal>
      ISA         arithmetic-problem
      state       finish-retrieve-tens
      first-tens  =first
      second-tens =second
      carry       "0"

    =retrieval>
      ISA         arithmetic
      first       =first
      second      =second
      ones        =ones
  ==>
    =goal>
      tens        =ones
      state       response
  )

  (P finish-retrieve-tens-carry
    =goal>
      ISA         arithmetic-problem
      state       finish-retrieve-tens
      first-tens  =first
      second-tens =second
      - carry       "0"

    =retrieval>
      ISA         arithmetic
      first       =first
      second      =second
      ones        =ones
  ==>
    =goal>
      tens        =ones
      state       check-carry
  )

  (P check-carry
    =goal>
      ISA         arithmetic-problem
      state       check-carry
      carry       "1"
      tens        =tens

    ?retrieval>
      state       free
      buffer      empty
  ==>
    +retrieval>
      ISA         SUCCESSOR
      value       =tens

    =goal>
      state       increment-tens
    )

  (P increment-tens
    =goal>
      ISA         arithmetic-problem
      state       increment-tens

    =retrieval>
      ISA         SUCCESSOR
      SUCCESSOR   =value
  ==>
    =goal>
      tens        =value
      state       response
    )

  (P respond-tens
    =goal>
      ISA         arithmetic-problem
      state       response
      tens        =tens
      - tens      "0"

    ?vocal>
      preparation free
  ==>
    +vocal>
      ISA         speak
      string      =tens

    =goal>
      state       response-ones
    )

  (P respond-ones-no-tens
    =goal>
      ISA         arithmetic-problem
      state       response
      tens        "0"
      ones        =ones

    ?vocal>
      preparation free
  ==>
    +vocal>
      ISA         speak
      string      =ones

    =goal>
      state       done
    )

  (P respond-ones
    =goal>
      ISA         arithmetic-problem
      state       response-ones
      ones        =ones

    ?vocal>
      preparation free
  ==>
    +vocal>
      ISA         speak
      string      =ones

    =goal>
      state       done
    )
)
