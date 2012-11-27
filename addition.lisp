
(clear-all)

(define-model addition

(sgp :esc t :lf .05)

(chunk-type arithmetic first operator second result)
(chunk-type arithmetic-problem first operator second result)
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

(defun deterministic-addition-sampling-ch9 (facts s mp rt)
  ;;; results = 5x5x12 array
  (let ((results (make-array '(5 5 12)))
        (errors (list nil nil))
        ;;; answers = array of length 25
        (answers (make-array 25 :initial-element 0))
        (probabilities (make-array 25 :initial-element 0.0))
        
        (full-percentages (make-array '(5 5 13) :initial-element 0.0)))

    (setf mp (* 10 mp))

    ;;; answers = facts.map((fact) => fact.result)
    (let ((index 0))
      (dolist (fact facts)
        (setf (svref answers index) (first (get-slot-value (get-safe-wme fact) 'result)))
        (incf index)))
    ;;; add a goal fact
    (addwm-fct '((goal isa arithmetic operator +)) :reset-ia nil)
    ;;; set focus on the goal
    (wmfocus goal)
    ;;; limit facts to first 25
    (setf facts (subseq facts 0 25))

    ; for i = 0:4
    (dotimes (i 5)
      (dotimes (j 5)
        ;;; modify the focus chunk to (first 1+i) (second 1+j)
        (modfocus-fct `(first ,(1+ i) second ,(1+ j)))
        ;;; define some params i guess
        ; omission = e^(rt/s)
        (let* ((omission (exp (/ rt s)))
               (sum omission)
               (index 0))
          ;;; for each fact
          (dolist (fact facts)
            ; set probabilities[index] = some big function
            (setf (svref probabilities index)
                  (exp (/ (- (activation (get-safe-wme fact))
                             (* (/ mp 10.0)
                                (+ (abs (- (floor index 5) i))
                                   (abs (- (mod index 5) j)))))
                          s)))
            ; sum += probabilities[index]
            (incf sum (svref probabilities index))
            ; index ++
            (incf index))
          ; k = 0:25
          (dotimes (k 25)
            ; results[i,j,answers[k]] += probabilities[k] / sum
            (incf (aref results i j (svref answers k))
                  (/ (svref probabilities k) sum)))
          ; results[i,j,11] += omission/sum
          ; cjb: wha?
          (incf (aref results i j 11) (/ omission sum)))))
      (dotimes (i 5)
        (let ((sum 0.0))
          (dotimes (j 5)
            ; percentage = 100 & results[i,j, i+j+2]
            (let ((percentage (* 100.0 (aref results i j (+ i j 2)))))
              ; sum += percentage
              (incf sum percentage)
              ))
          ; errors[0].push(sum/5)
          (push-last (/ sum 5) (first errors))))
      (dotimes (j 5)
        (let ((sum 0.0))
          (dotimes (i 5)
            ; sum += 100 * results[i,j, i+j+2]
            (incf sum (* 100.0 (aref results i j (+ i j 2)))))
          ; errors[1].push(sum/5)
          (push-last (/ sum 5) (second errors))))
      
      (dotimes (i 5)
        (dotimes (j 5)
          (dotimes (k 12)
            (if (= k 11)
                ; full-percentages[i,j,12] = 100 * results[i,j,k]
                (setf (aref full-percentages i j 12) (* 100.0 (aref results i j k)))
                ; full-percentages[i,j,k] = 100 * results[i,j,k]
                (setf (aref full-percentages i j k) (* 100.0 (aref results i j k)))))))
      ; errors.push(full-percentages)
      (push-last full-percentages errors)))

(defun rehearse-chunk-fct-ch9 (chunks &key (repeat 1) (force nil)
                                  (cycle nil) (time nil))
  "Rehearses and returns chunks.  If a chunk is a list, then the first element
   of the list is the chunk to rehearse and the rest is the list of sources.
   When force is on, update the statistics whether or not learning is on.
   When cycle and/or time is set, increment the equivalent ACT-R counters
   by those amounts (1 by default)."
  (let ((structures nil)
        (level (* 1.0 repeat)))
    (unless (integerp repeat)
      (setf repeat (round repeat)))
    (dolist (spec chunks structures)
      (let ((chunk (if (listp spec) (first spec) spec)))
        (push-last chunk structures)
        (when chunk
          (when (or force *base-level-learning*)
            ;; include add-reference directly to do repeat at once
            (let ((references (wme-references chunk)))
              (incf (first references) level)
              (unless *optimized-learning*
                (setf (rest references)
                      (nconc (make-list repeat :initial-element *time*)
                             (rest references))))))
          (when (or force *associative-learning*)
            (let ((sources nil))
              (if (listp spec)
                (dolist (source (rest spec))
                  (setf source (get-safe-wme source))
                  (when source
                    (push source sources)))
                ; add the chunk itself to the set of sources
                (setf sources (cons chunk (rest (wme-slot-wmes chunk)))))
;              (setf level (/ 1.0 (length sources)))
              ;; Do not parcel out the sources.
              (incf (wme-needed chunk) level)
              (dolist (source sources)
                (incf (wme-contexts source) level)
                (incf (ia-fnicj (get-make-ia source chunk)) level))))
          (when cycle
            (incf *cycle* (* repeat (if (numberp cycle) cycle 1))))
          (when time
            (incf *time* (* level (if (numberp time) time 1.0)))))))))

(defun square-ch9 (x)
  "Squares x."
  (* x x))

(defun frequency-table-ch9 (&key (n 10) (ratio 2.0) (boost 0.0) (flip 0.0))
  "Generates the frequency table for a n x n table.
   Ratio specifies the frequency ratio between most (largest)
   and least common (smallest) problem in a row or column.
   The frequency decreases linearly within a row or column.
   Boost specifies an extra probability for the first row and column.
   Flip specifies an extra probability for the lower half of the diagonal,
   i.e. problems with a row index larger than the column index."
  (let* ((frequencies (make-array (list n n) :initial-element 0.0))
         (start (square-ch9 (/ (* 2.0 ratio) (* n (+ 1.0 ratio)))))
         (fraction (/ (- ratio 1.0) (* ratio (- n 1))))
         (delta (* start fraction)))
    ; basic frequency (ratio) distribution
    (dotimes (i n)
      (let ((frequency start)
            (difference (* start fraction)))
        (dotimes (j n)
          (setf (aref frequencies i j) frequency)
          (decf frequency difference))
        (decf start delta)))
    ; account for boosts
    (when (/= boost 0.0)
      (dotimes (i n)
        (unless (= i 0)
          (incf (aref frequencies 0 i) (/ boost (* 2 (- n 1))))
          (incf (aref frequencies i 0) (/ boost (* 2 (- n 1))))))
      (dotimes (i n)
        (dotimes (j n)
          (setf (aref frequencies i j)
                (/ (aref frequencies i j) (+ 1.0 boost))))))
    ; account for flips
    (when (/= flip 0.0)
      (dotimes (i n)
        (dotimes (j n)
          (if (> i j)
            (setf (aref frequencies i j)
                  (* (+ 1.0 flip) (aref frequencies i j)))
            (when (< i j)
              (setf (aref frequencies i j)
                  (* (- 1.0 flip) (aref frequencies i j))))))))            
    frequencies))


(defun addition-retrieval-ch9 (&key (s 0.15) (mp .15) (rt -2.25)  (ratio 2.5)
                                (problems 1000.0) (problems-per-day 100.0)
                                (flip 0.06) )
  (reset)
  (let ((schedule (frequency-table-ch9 :n 5 :ratio ratio :boost flip :flip flip))
        (facts nil)
        (delay (/ 86400.0 problems-per-day)))
    
    (dotimes (i 5)
      (dotimes (j 5)
        ;;; fact = first wme matching ((isa arithmetic) (first 1+i) (second 1+j))
        (let ((fact (no-output
                     (first (sdm-fct (list 'isa 'arithmetic
                                           'first (1+ i) 'second (1+ j)))))))
          ;;; add fact to the end of facts
          (push-last fact facts)
          ;;; rehearse chunks
          (dotimes (k (round (* problems (aref schedule i j))))
            (rehearse-chunk-fct-ch9 (list fact))))))
    ;;; FIX: introduce the delay
    (incf *time* (* problems delay))
    (incf *cycle* problems)
    (incf *spread-stamp* problems)
    (output-addition-ch9 (deterministic-addition-sampling-ch9 facts s mp rt ) t)))