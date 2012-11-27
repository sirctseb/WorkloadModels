;;;<pre><xmp>
;;; 
;;; This file contains the cognitive arithmetic
;;; models for chapter 9 (other than the lifetime simulation).
;;; The simulations require ACT-R 4.0b5,
;;; however are not traditional ACT-R models.
;;; In order to obtain results more quickly, the 
;;; results are derived directly from the activation
;;; of the chunks, without productions that must
;;; match them.

;;; The following functions run the corresponding simulation:
;;;
;;; (addition-retrieval-ch9)
;;; (multiplication-computation-ch9)
;;; (problem-size-effect-ch9)
;;; (problem-size-over-time-ch9)


(defconstant *problem-size-data-ch9* '(((1 .78) (1 .915) (2 .815) (2 1.085) (3 .8) (3 .87) (4 .82) (4 .84)
                                        (5 .9) (5 1.07) (6 .74) (6 .94) (7 .86) (7 .94) (8 .81) (8 .83) (9 .86) (9 1))
                                       ((0 .965) (2 .925) (4 .79) (6 .99) (8 .79) (10 .89) (12 .92) (14 .97) (16 .94) (18 1.03))  
                                       ((3  .86) (3  .94) (4  .925) (4 .945) (5 .81) (5 .85) (5 1.02) (5 1.065)
                                        (6 .85) (6 .87) (6 .89) (6 .91) (7 .83) (7 .87) (7 .89) (7 .985)
                                        (7 1.015) (8 .845) (8 .905) (8 .915) (8 .935) (8 1.045) (8 1.055) (9 .82) (9 .88) (9 .89) (9 .91) (9 .93)
                                        (9 .97) (9 .99) (9 1.08) (10 .76) (10 .86) (10 .945) (10 .975) (10 .985) (10 1.06) (11 .98) (11 1.03) (11 1.04)
                                        (11 1.05) (11 1.1) (11 1.18) (11 1.285) (11 1.315) (12 1.1) (12 1.13) (12 1.2) (12 1.28) (12 1.295)
                                        (12 1.46) (13 1.13) (13 1.23) (13 1.27) (13 1.33) (13 1.62) (13 1.72) (14 1.23) (14 1.25) (14 1.37)
                                        (14 1.52) (15 1.25) (15 1.28) (15 1.29) (15 1.37) (16 1.35) (16 1.42) (17 1.15) (17 1.4))))
(defconstant *multiplication-frequency-ch9*
  '(4  3  2  4  4  1  5  1  3  4
    2  3  2  3  4  3  5  7  5  2
    1  3 19 18 17 20 18 17 17 20
    2  3 18 24 17 19 15 15 15 13
    3  1 19 20 19 20 14 11  2 17
    3  2  9 15 18 21 13 14 11 14
    2  4  6 10 12 12 13 14 11  6
    4  2  6  7  6  9  8  8  7  9
    1  3  6  6  6  7  8  7  6  6
    2  1  5 11  6  7  8  9  7  7)
  "Number of times each multiplication problem was presented in second-
   and third-grade textbooks from Siegler 88 Table 4.
   Rows are multiplicand from 0 to 9 and columns are multiplier from 0 to 9.")

(defconstant *addition-data-ch9* (list '(67.6 47 45.8 38.8 33.8) '(77.2 53.2 42.8 33.8 26.6)
                                       (make-array '(5 5 13) :initial-contents 
                                                   '(((0 5 86 0 2 0 2 0 0 0 0 2 4)
                                                      (0 0 9 70 2 0 4 0 0 7 2 2 5)
                                                      (0 2 0 11 71 5 2 2 0 0 0 0 7)
                                                      (0 0 0 0 11 61 9 7 0 0 0 2 11)
                                                      (0 0 0 0 13 16 50 11 0 2 2 0 5))
                                                     ((0 7 5 79 5 0 0 0 0 0 0 0 4)
                                                      (2 0 4 5 80 4 0 5 0 0 0 0 0)
                                                      (0 0 4 7 38 34 9 2 2 2 0 0 4)
                                                      (0 2 0 7 2 43 29 7 7 0 0 0 4)
                                                      (0 2 0 5 2 16 43 13 0 0 2 0 18))
                                                     ((0 2 0 9 79 4 0 4 0 0 0 0 4)
                                                      (0 0 9 11 11 55 7 0 0 0 0 0 7)
                                                      (4 0 0 5 21 9 48 0 2 2 2 0 7)
                                                      (0 0 0 5 11 23 14 29 2 0 0 0 16)
                                                      (0 0 0 7 0 13 23 14 18 0 5 0 20))
                                                     ((0 0 4 2 9 68 2 2 7 0 0 0 7)
                                                      (0 0 7 9 0 20 36 13 7 0 2 0 7)
                                                      (0 0 0 5 18 9 9 38 9 0 2 0 11)
                                                      (4 0 0 2 2 29 7 7 34 0 4 0 13)
                                                      (0 0 0 0 4 9 16 9 11 18 11 4 20))
                                                     ((0 0 4 0 4 7 71 4 4 0 4 0 4)
                                                      (0 0 5 20 2 18 27 25 2 0 2 0 0)
                                                      (0 0 2 11 9 18 5 16 23 0 5 0 11)
                                                      (0 0 0 0 11 21 16 5 11 16 4 0 16)
                                                      (4 0 0 0 0 7 25 11 2 4 34 4 11))))))


(defconstant *problem-size-over-time-data-ch9* '((3.65 8.7) (2.15 3.2) (1.3 1.8) (1.1 1.6) ( .95 1.15)))

(defconstant *multiplication-data-ch9* '(( 19 28 3 25 32 40 48) (8 12 30 25 40 36 50)))


(defvar *addition-frequency* nil
  "Number of times each addition problem is used, if multiplication
   by repeated addition were used on the multiplication problems above.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This section contains the interface for the WWW using the
;;; ACT-R on the Web application by Elmar Schwarz

(defvar *v*)
(defvar *text*)
(defvar *graphic*)
(defvar *overlay*)
(defvar *noise*)
(defvar *mismatch*)
(defvar *thresh*)
(defvar *intercept*)
(defvar *factor*)
(defvar *zero-prod*)
(setf *zero-prod* .5)
(setf *intercept* .4)
(setf *factor* 1)
(setf *noise* .15)
(setf *thresh* -2.25)
(setf *mismatch* .15)
(setf *text* t)
(setf *v* nil)
(setf *graphic* nil)
(setf *overlay* nil)



(defvar *local-symbols*)
(defvar *WWW-interface*)


#|
(setf *local-symbols*
      '(
       *addition-frequency* 
       *noise*
        *thresh*
        *mismatch*
        *v*
        *text*
        *graphic*
        *overlay*))

(setf  *WWW-interface* 
      '((:heading "Addition Retrieval" 2)
        (:table)
        
        (:table)
        "Activation Noise (ans) : "         (:string :sy *noise* .15)  (:new-row)
        "Mismatch Penalty (mp):"       (:string :sy *mismatch*  .15)    (:new-row)
        "Retrieval Threshold (rt):"       (:string :sy *thresh*  -2.25)    (:new-row)
        (:table-end)
        (:table)
        (:checkbox "Text output" :sy *text*  t) (:new-row)
        (:checkbox "Graphic output" :sy *graphic*  nil) (:new-row)
        (:checkbox "Show simulation and experiment data on one graph" :sy *overlay*  nil) 
        (:table-end)
       (:table-end)
        
        (:new-para)
        (:button "Show Experiment Results" "(output-addition-ch9 *addition-data-ch9* nil)")
       
        (:new-para)
        
        
        (:button "Run model" "(if (and (numberp *noise*) (numberp *mismatch*) (numberp *thresh*))
                                  (progn
                                     (translate-frequencies-ch9)
                                      (addition-retrieval-ch9 :s *noise* :mp *mismatch* :rt *thresh*))

                                  (format *standard-output* \"All parameters must be numbers~%\"))")
        (:reset "Default values")
        
        (:button "Chunk types" "(chunk-type)")
        (:button "Chunks" "(dm)")
         (:new-para)
         "TIME and SIZE:"
        (:new-para)
        "- It usually takes less than 1 minute to run the model"
        (:new-para)))

(setf *local-symbols*
      '(
       *addition-frequency* 
       *noise*
        *mismatch*
        *v*
        *text*
        *graphic*
        *overlay*))

(setf  *WWW-interface* 
      '((:heading "Multiplication Computation" 2)
        (:table)
        
        (:table)
        "Activation Noise (ans) : "         (:string :sy *noise* .12)  (:new-row)
        "Mismatch Penalty (mp):"       (:string :sy *mismatch*  .15)    (:new-row)
        (:table-end)
        (:table)
        (:checkbox "Detailed output" :sy *v*  nil) (:new-row)
        (:checkbox "Text output" :sy *text*  t) (:new-row)
        (:checkbox "Graphic output" :sy *graphic*  nil) (:new-row)
        (:checkbox "Show simulation and experiment data on one graph" :sy *overlay*  nil) 
        (:table-end)
       (:table-end)
        
        (:new-para)
        (:button "Show Experiment Results" "(output-multiplication-ch9 *multiplication-data-ch9* nil)")
       
        (:new-para)
        
        
        (:button "Run model" "(if (and (numberp *noise*) (numberp *mismatch*) )
                                  (progn
                                     (translate-frequencies-ch9)
                                      (multiplication-computation-ch9 :s *noise* :mp *mismatch* :output *v*))

                                  (format *standard-output* \"All parameters must be numbers~%\"))")
        (:reset "Default values")
        
        (:button "Chunk types" "(chunk-type)")
        (:button "Chunks" "(dm)")
         (:new-para)
         "TIME and SIZE:"
        (:new-para)
        "- It usually takes less than 1 minute to run the model"
        (:new-para)))


(setf *local-symbols*
      '(
       *addition-frequency* 
        *zero-prod*
       *intercept*
        *factor*
        *v*
        *text*
        *graphic*
        *overlay*))

(defvar *WWW-interface*)

(setf  *WWW-interface* 
      '((:heading "Problem Size Effect" 2)
        (:table)
        
        (:table)
        "Zero production action time: "         (:string :sy *zero-prod*  .5)  (:new-row)
        "Intercept (I) for encoding/answering: "         (:string :sy *intercept*  .4)  (:new-row)
        "Latency Factor (F):"             (:string :sy *factor*  1)    (:new-row)
        (:table-end)
        (:table)
        (:checkbox "Detailed output" :sy *v*  nil) (:new-row)
        (:checkbox "Text output" :sy *text*  t) (:new-row)
        (:checkbox "Graphic output" :sy *graphic*  nil) (:new-row)
        (:checkbox "Show simulation and experiment data on one graph" :sy *overlay*  nil) 
        (:table-end)
       (:table-end)
        
        (:new-para)
        (:button "Show Experiment Results" "(output-problem-size-ch9 *problem-size-data-ch9* nil)")
       
        (:new-para)
        
        
        (:button "Run model" "(if (and (numberp *thresh*) (numberp *intercept*) (numberp *zero-prod*))
                                  (progn
                                     (translate-frequencies-ch9)
                                      (problem-size-effect-ch9 :intercept *intercept* :factor *factor* :output *v*))

                                  (format *standard-output* \"All parameters must be numbers~%\"))")
        (:reset "Default values")
        (:button "Production Rules" "(let ((prods (no-output (pp))))
                                       (dolist (x prods)
                                         (pp-fct (list x))
                                         (spp-fct (list x))
                                         (format *standard-output* \"~%\")))")
        (:button "Chunk types" "(chunk-type)")
        (:button "Chunks" "(dm)")
         (:new-para)
         "TIME and SIZE:"
        (:new-para)
        "- It usually takes less than 1 minute to run the model"
        (:new-para)))


(setf *local-symbols*
      '(
       *addition-frequency* 
        *zero-prod*
       *intercept*
        *factor*
        *v*
        *text*
        *graphic*
        *overlay*))

(defvar *WWW-interface*)

(setf  *WWW-interface* 
      '((:heading "Problem Size Effect Over Time" 2)
        (:table)
        
        (:table)
        "Zero production action time: "         (:string :sy *zero-prod*  .5)  (:new-row)
        "Intercept (I) for encoding/answering: "         (:string :sy *intercept*  .4)  (:new-row)
        "Latency Factor (F):"             (:string :sy *factor*  1)    (:new-row)
        (:table-end)
        (:table)
        (:checkbox "Text output" :sy *text*  t) (:new-row)
        (:checkbox "Graphic output" :sy *graphic*  nil) (:new-row)
        (:checkbox "Show simulation and experiment data on one graph" :sy *overlay*  nil) 
        (:table-end)
       (:table-end)
        
        (:new-para)
        (:button "Show Experiment Results" "(output-problem-size-over-time-ch9 *problem-size-over-time-data-ch9* nil)")
       
        (:new-para)
        
        
        (:button "Run model" "(if (and (numberp *thresh*) (numberp *intercept*) (numberp *zero-prod*))
                                  (progn
                                     (translate-frequencies-ch9)
                                      (problem-size-over-time-ch9 :intercept *intercept* :factor *factor*))

                                  (format *standard-output* \"All parameters must be numbers~%\"))")
        (:reset "Default values")
        (:button "Production Rules" "(let ((prods (no-output (pp))))
                                       (dolist (x prods)
                                         (pp-fct (list x))
                                         (spp-fct (list x))
                                         (format *standard-output* \"~%\")))")
        (:button "Chunk types" "(chunk-type)")
        (:button "Chunks" "(dm)")
         (:new-para)
         "TIME and SIZE:"
        (:new-para)
        "- It usually takes less than 1 minute to run the model"
        (:new-para)))
|#





;;; AL PATCH: define rehearse-chunk-fct-ch9 

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
      (let ((chunk (get-safe-wme (if (listp spec) (first spec) spec))))
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
;;; CODE

(defun square-ch9 (x)
  "Squares x."
  (* x x))

(defun output-frequencies-ch9 (frequencies)
  "Prints a two-dimensional square array of frequencies."
  (let ((n (array-dimension frequencies 0)))
    (format t "FREQUENCIES:~%  +    ")
    (dotimes (i n)
      (format t "~6s" i))
    (format t "~%")
    
    (dotimes (i n)
      (format t "~4s" i)
      (dotimes (j n)
        (format t " ~5,3F" (aref frequencies i j)))
      (terpri))))

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

(defun problem-size-effect-ch9 (&key (problems 500000.0) (ratio 2.0)
                                 (n 10) (problems-per-day 100.0)
                                 (intercept 0.4) (exponent 1.0) (factor 1.0)
                                 (output t) (over-time nil))
  "Displays the problem-size effect latencies for n x n table."
  (reset)
  (let ((schedule (frequency-table-ch9 :n n :ratio ratio))
        (arithmetics (no-output (sdm isa arithmetic)))
        (numbers (wme-type-wmes (get-type 'number)))
        (zeroes (make-array (* 2 n) :initial-element 0.0))
        (ties (make-array (* 2 n) :initial-element 0.0))
        (others (make-array (* 2 n) :initial-element 0.0))
        (counts (make-array (* 2 n) :initial-element 0.0))
        (latencies (make-array (list n n) :initial-element 0.0))
        (plus (get-wme '+))
        (delay (/ 86400.0 problems-per-day)))
    ;;; Provides the history by rehearsing each arithmetic chunk
    ;;; according to distribution frequency for a total of problems
    (when output
      (output-frequencies-ch9 schedule))
    (let ((facts arithmetics))
      (dotimes (i n)
        (dotimes (j n)
          (let ((fact (pop facts)))
            (rehearse-chunk-fct-ch9 (list fact)
                                    :repeat (round (* problems
                                                      (aref schedule i j))))))))
    ;;; Increment the global counters
    (incf *time* (* problems delay))
    (incf *cycle* problems)
    (incf *spread-stamp* problems)
    ;;; Create the goal
    (addwm-fct '((goal isa arithmetic operator +)) :reset-ia nil)
    (wmfocus goal)
    ;;; Computes the activations and latencies.
    (when output 
      (format t "~%ACTIVATIONS:~%  +    ")
      (dotimes (i n)
        (format t "~7s" i))
      (format t "~%"))
    (let ((facts arithmetics))
      (dotimes (i n)
        (when output
          (format t "~4s" i))
        (dotimes (j n)
          
          (mod-focus-fct `(first ,i second ,j))
          
          (let* ((fact (pop facts))
                 (activation (activation (get-safe-wme fact)))
                 (latency (* factor (exp (- (* exponent activation))))))
            (cond ((or (zerop i) (zerop j))
                   (incf (aref zeroes (+ i j)) latency))
                  ((= i j)
                   (incf (aref ties (+ i j)) latency))
                  (t
                   (incf (aref others (+ i j)) latency)
                   (incf (aref counts (+ i j)) 1.0)))
            (setf (aref latencies i j) latency)
            (when output (format t " ~6,3F" activation))))
        (when output (format t "~%"))))
    ;;; Outputs the base levels and IAs.
    (when output
      (format t "~%BASE LEVELS:~%  +    ")
      (dotimes (i n)
        (format t "~7s" i))
      (format t "~%")
      (let ((facts arithmetics))
        (dotimes (i n)
          (format t "~4s" i)
          (dotimes (j n)
            (let ((fact (pop facts)))
              (format t " ~6,3F" (wme-base-level (get-safe-wme fact)))))
        (format t "~%")))
      (format t "~%IA(1st):~%  +    ")
      (dotimes (i n)
        (format t "~7s" i))
      (format t "~%")
      (let ((facts arithmetics))
        (dotimes (i n)
          (format t "~4s" i)
          (let ((first (nth i numbers)))
            (dotimes (j n)
              (let ((fact (pop facts)))
                (format t " ~6,3F" (get-ia (get-safe-wme first) (get-safe-wme fact))))))
          (terpri)))
      (format t "~%IA(+):~%  +    ")
      (dotimes (i n)
        (format t "~7s" i))
      (format t "~%")
      (let ((facts arithmetics))
        (dotimes (i n)
          (format t "~4s" i)
          (dotimes (j n)
            (let ((fact (pop facts)))
              (format t " ~6,3F" (get-ia (get-safe-wme plus) (get-safe-wme fact)))))
          (terpri)))
      (format t "~%IA(2nd):~%  +    ")
      (dotimes (i n)
        (format t "~7s" i))
      (format t "~%")
      (let ((facts arithmetics))
        (dotimes (i n)
          (format t "~4s" i)
          (dotimes (j n)
            (let ((second (nth j numbers))
                  (fact (pop facts)))
              (format t " ~6,3F" (get-ia (get-safe-wme second) (get-safe-wme fact)))))
          (terpri)))
      (format t "~%LATENCIES:~%"))
    ;;; compute the latency statistics 
    (if over-time
        (let ((sizecounts (make-array '(2 2) :initial-element 0.0)))
          (dotimes (i (1- (* 2 n)))
            (when (< i n)
              (incf (aref sizecounts 0 0) (aref zeroes i))
              (incf (aref sizecounts 1 0) (if (zerop i) 1.0 2.0)))
            (when (and (zerop (mod i 2)) (not (zerop i)))
              (incf (aref sizecounts 0 (floor i n)) (aref ties i))
              (incf (aref sizecounts 1 (floor i n)) 1.0))
            (when (and (> i 2) (< i (* 2 (1- n))))
              (incf (aref sizecounts 0 (floor i n)) (aref others i))
              (incf (aref sizecounts 1 (floor i n)) (aref counts i))))
          (list (+ intercept (/ (aref sizecounts 0 0)
                                  (aref sizecounts 1 0))) 
                (+ intercept (/ (aref sizecounts 0 1)
                                  (aref sizecounts 1 1)))))

        (let ((data (list nil nil nil)))
          (dotimes (i (1- (* 2 n)))
            (when (< i n)
              (push-last (list i (+ intercept (/ (aref zeroes i)
                                                 (if (zerop i) 1.0 2.0)))) (first data)))
            
            (when (and (zerop (mod i 2)) (not (zerop i)))
              (push-last (list i (+ intercept (aref ties i))) (second data)))
            (when (and (> i 2) (< i (* 2 (1- n))))
              (push-last (list i (+ intercept (/ (aref others i)
                                                 (aref counts i)))) (third data))
              ))
          ;; adjust for the non-existant add-zero production
          
          (let ((zero-time (+ *intercept* *zero-prod*))
                (zts nil))
            (setf (first data) 
                  (dotimes (i 10 zts)
                    (push-last (list i zero-time) zts))))
          (output-problem-size-ch9 data t)))))

(defun output-problem-size-ch9 (data sim)

    (when sim
      (format *standard-output* "~%~%Parameters for run: (~S ~S ~S)~%" 
              *zero-prod* *intercept* *factor*))

  (when *text*
    (format *standard-output* "~%~A data:~%" (if sim "Simulation" "Experimental"))
    (format *standard-output* "~%Latencies:~%")
    (format *standard-output* "Sum        RT(sec)~%")
    (format *standard-output* "        Zero     Tie      Other~%")
    (do* 
      ((d (copy-list data))
       (count 0)
       (i1 (if (and (first d) (= (caar (first d)) count)) (pop (first d)) nil)
           (if (and (first d) (= (caar (first d)) count)) (pop (first d)) nil))
       (i2 (if (and (second d) (= (caar (second d)) count)) (pop (second d)) nil)
           (if (and (second d) (= (caar (second d)) count)) (pop (second d)) nil))
       (i3 (if (and (third d) (= (caar (third d)) count)) (pop (third d)) nil)
           (if (and (third d) (= (caar (third d)) count)) (pop (third d)) nil)))
      ((and (null (first d)) (null (second d)) (null (third d)) (null i1) (null i2) (null i3)))
      (if (and (null i1) (null i2) (null i3))
          (incf count)
          (progn
            (format *standard-output* "~4s" count)
            (if i1
                (format *standard-output* "~9,3f" (second i1))
                (format *standard-output* "         "))
            (if i2
                (format *standard-output* "~9,3f" (second i2))
                (format *standard-output* "         "))
            (if i3
                (format *standard-output* "~9,3f" (second i3))
                (format *standard-output* "         "))
            (format *standard-output* "~%"))))


    (when (and sim *overlay*)
      (format *standard-output* "~%Experimental data:~%")
      (format *standard-output* "~%Latencies:~%")
      (format *standard-output* "Sum        RT(sec)~%")
      (format *standard-output* "        Zero     Tie      Other~%")
      
      (do* 
        ((d (copy-list *problem-size-data-ch9*))
         (count 0)
         (i1 (if (and (first d) (= (caar (first d)) count)) (pop (first d)) nil)
             (if (and (first d) (= (caar (first d)) count)) (pop (first d)) nil))
         (i2 (if (and (second d) (= (caar (second d)) count)) (pop (second d)) nil)
             (if (and (second d) (= (caar (second d)) count)) (pop (second d)) nil))
         (i3 (if (and (third d) (= (caar (third d)) count)) (pop (third d)) nil)
             (if (and (third d) (= (caar (third d)) count)) (pop (third d)) nil)))
        ((and (null (first d)) (null (second d)) (null (third d)) (null i1) (null i2) (null i3)))
        
        (if (and (null i1) (null i2) (null i3))
            (incf count)
            (progn
              (format *standard-output* "~4s" count)
              (if i1
                  (format *standard-output* "~9,3f" (second i1))
                  (format *standard-output* "         "))
              (if i2
                  (format *standard-output* "~9,3f" (second i2))
                  (format *standard-output* "         "))
              (if i3
                  (format *standard-output* "~9,3f" (second i3))
                  (format *standard-output* "         "))
              (format *standard-output* "~%")))))

    (unless *graphic* (format *standard-output* 
                              "~%</pre>If your browser supports JAVA, you 
                               can display the data in a graph by checking 
                               the Graphic output box on the interface page.<pre>~%~%")))

  (when *graphic* 
    (format *standard-output* " 
        <applet 
        code = \"DansGraphs.class\" 
        width = 400 
        height = 500> 

        <PARAM name=\"title\" value=\"Problem Size Effect\">
        <PARAM name=\"xmin\" value=\"0\">
        <PARAM name=\"xmax\" value=\"20\">
        <PARAM name=\"ymax\" value=\"1.8\">
        <PARAM name=\"ymin\" value=\"0.6\">
        <PARAM name=\"xdiv\" value=\"5\">
        <PARAM name=\"xspacing\" value=\"10\">
        <PARAM name=\"longestline\" value=\"~s\">
        <PARAM name=\"numlines\" value=\"~S\">
        <PARAM name=\"ydiv\" value=\".1\">
        <PARAM name=\"yspacing\" value=\".2\">
        <PARAM name=\"lcolor0\" value=\"0\">
        <PARAM name=\"lcolor1\" value=\"1\">
        <PARAM name=\"lcolor2\" value=\"2\">
        <PARAM name=\"lstyle0\" value=\"~s\">
        <PARAM name=\"lstyle1\" value=\"~s\">
        <PARAM name=\"lstyle2\" value=\"~s\">
        <PARAM name=\"xname\" value=\"Sum\">
        <PARAM name=\"yname\" value=\"RT (sec)\">
        <PARAM name=\"name0\" value=\"~a Zero\">
        <PARAM name=\"name1\" value=\"~a Tie\">
        <PARAM name=\"name2\" value=\"~a Other\">
" 
            (if (and sim *overlay*)
                (max (length (first data)) (length (second data)) (length (third data))
                      (length (first *problem-size-data-ch9*)) (length (second *problem-size-data-ch9*)) (length (third *problem-size-data-ch9*)))
                (max (length (first data)) (length (second data)) (length (third data))))
            (if (and sim *overlay*) 6 3)
            (if sim 2 1)
            (if sim 2 1)
            (if sim 2 1)
            (if sim "Simulation Data" "Experiment Data")
            (if sim "Simulation Data" "Experiment Data")
            (if sim "Simulation Data" "Experiment Data")
)
    
    (dotimes (i 3)
      (format *standard-output* "<PARAM name=\"xval~s\" value=\"" i)
      (dolist (x (nth i data))
        (format *standard-output* "~6,3f;" (car x)))
      (format *standard-output* "\">
                               <PARAM name=\"yval~s\" value=\"" i)
      (dolist (y (nth i data))
        (format *standard-output* "~6,3f;" (cadr y)))
      (format *standard-output* "\">"))
    
    (when (and *overlay* sim)
      (format *standard-output* "
        <PARAM name=\"lcolor3\" value=\"0\">
        <PARAM name=\"lcolor4\" value=\"1\">
        <PARAM name=\"lcolor5\" value=\"2\">
        <PARAM name=\"lstyle3\" value=\"1\">
        <PARAM name=\"lstyle4\" value=\"1\">
        <PARAM name=\"lstyle5\" value=\"1\">
        <PARAM name=\"name3\" value=\"Experiment Data Zero\">
        <PARAM name=\"name4\" value=\"Experiment Data Tie\">
        <PARAM name=\"name5\" value=\"Experiment Data Other\">
         ")
      
     (dotimes (i 3)
      (format *standard-output* "<PARAM name=\"xval~s\" value=\"" (+ 3 i))
      (dolist (x (nth i *problem-size-data-ch9*))
        (format *standard-output* "~6,3f;" (car x)))
      (format *standard-output* "\">
                               <PARAM name=\"yval~s\" value=\"" (+ 3 i))
      (dolist (y (nth i *problem-size-data-ch9*))
        (format *standard-output* "~6,3f;" (cadr y)))
      (format *standard-output* "\">")))
    
    (format *standard-output* "
             <HR> Either your browser does not support JAVA or this graph has scrolled off the top of the display.~%
             </HR></applet>")))


(defun problem-size-over-time-ch9 (&key (years (list 0.5 3.5 6.5 9.5 12.5))
                                    (ratio 2.0) (n 10) (problemsperday 100.0)
                                    (intercept 0.4) (exponent 1.0) (factor 1.0)
                                    (output nil))
  (let ((res nil))
    (dolist (year years res)
      (push-last  (problem-size-effect-ch9 :problems (* year 365.0 problemsperday)
                                           :ratio ratio :n n :problems-per-day problemsperday
                                           :intercept intercept :exponent exponent :factor factor
                                           :output output :over-time t) res))
    (output-problem-size-over-time-ch9 res t)))
  
(defun output-problem-size-over-time-ch9 (data sim)
  
  (let ((data-names '("1st grade" "4th grade" "7th grade" "10th grade" "College"))
        (sim-names '("N=18,250" "N=127,750" "N=237,250" "N=346,750" "N=456,250")))
  (when sim
    (format *standard-output* "~%~%Parameters for run: (~S ~S ~S)~%" 
            *zero-prod* *intercept* *factor*))
  
  (when *text*
    (format *standard-output* "~%~A data:~%" (if sim "Simulation" "Experimental"))
    (format *standard-output* "age                RT(sec)~%")
    (format *standard-output* "              Small      Large~%")
    (dotimes (i (length data))
      (format *standard-output* "~10a    ~5,2F      ~5,2F~%" (if sim (nth i sim-names) (nth i data-names))
              (first (nth i data)) (second (nth i data))))



    (when (and sim *overlay*)
      (format *standard-output* "~%Experimental data:~%")
    (format *standard-output* "age                RT(sec)~%")
    (format *standard-output* "              Small      Large~%")
    (dotimes (i (length data))
      (format *standard-output* "~10a    ~5,2F      ~5,2F~%" (nth i data-names)
              (first (nth i *problem-size-over-time-data-ch9*)) (second (nth i *problem-size-over-time-data-ch9*)))))

    (unless *graphic* (format *standard-output* 
                              "~%</pre>If your browser supports JAVA, you 
                               can display the data in a graph by checking 
                               the Graphic output box on the interface page.<pre>~%~%")))

  (when *graphic* 
    (format *standard-output* " 
        <applet 
        code = \"DansGraphs.class\" 
        width = 400 
        height = ~S> 

        <PARAM name=\"title\" value=\"Problem Size Effect\">
        <PARAM name=\"xmin\" value=\"0\">
        <PARAM name=\"xmax\" value=\"4\">
        <PARAM name=\"ymax\" value=\"10\">
        <PARAM name=\"ymin\" value=\"0\">
        <PARAM name=\"numxlabels\" value=\"5\">
        <PARAM name=\"longestxlabel\" value=\"WWWWW\">
        <PARAM name=\"xlabels\" value=\"  ;Small;  ;Large;  ;\">
        <PARAM name=\"longestline\" value=\"2\">
        <PARAM name=\"numlines\" value=\"~S\">
        <PARAM name=\"ydiv\" value=\"1\">
        <PARAM name=\"yspacing\" value=\"2\">

        <PARAM name=\"xval0\" value=\"1;3;\">
        <PARAM name=\"xval1\" value=\"1;3;\">
        <PARAM name=\"xval2\" value=\"1;3;\">
        <PARAM name=\"xval3\" value=\"1;3;\">
        <PARAM name=\"xval4\" value=\"1;3;\">

        <PARAM name=\"lcolor0\" value=\"0\">
        <PARAM name=\"lcolor1\" value=\"1\">
        <PARAM name=\"lcolor2\" value=\"2\">
        <PARAM name=\"lcolor3\" value=\"3\">
        <PARAM name=\"lcolor4\" value=\"4\">
        <PARAM name=\"lstyle0\" value=\"~s\">
        <PARAM name=\"lstyle1\" value=\"~s\">
        <PARAM name=\"lstyle2\" value=\"~s\">
        <PARAM name=\"lstyle3\" value=\"~s\">
        <PARAM name=\"lstyle4\" value=\"~s\">
        <PARAM name=\"xname\" value=\"Problem Size\">
        <PARAM name=\"yname\" value=\"RT (sec)\">
        <PARAM name=\"name0\" value=\"~a (~A)\">
        <PARAM name=\"name1\" value=\"~a (~A)\">
        <PARAM name=\"name2\" value=\"~a (~A)\">
        <PARAM name=\"name3\" value=\"~a (~A)\">
        <PARAM name=\"name4\" value=\"~a (~A)\">
" 
            (if (and sim *overlay*) 600 500)
            (if (and sim *overlay*) 10 5)
            (if sim 2 6553)
            (if sim 2 6553)
            (if sim 2 6553)
            (if sim 2 6553)
            (if sim 2 6553)
            (if sim "Simulation Data" "Experiment Data") (if sim (nth 0 sim-names) (nth 0 data-names)) 
            (if sim "Simulation Data" "Experiment Data") (if sim (nth 1 sim-names) (nth 1 data-names)) 
            (if sim "Simulation Data" "Experiment Data") (if sim (nth 2 sim-names) (nth 2 data-names)) 
            (if sim "Simulation Data" "Experiment Data") (if sim (nth 3 sim-names) (nth 3 data-names)) 
            (if sim "Simulation Data" "Experiment Data") (if sim (nth 4 sim-names) (nth 4 data-names)) 
)
    
    (dotimes (i 5)
      (format *standard-output* "<PARAM name=\"yval~s\" value=\"" i)
      (format *standard-output* "~6,3f;~6,3f;\">" (first (nth i data)) (second (nth i data))))

    
    (when (and *overlay* sim)
      (format *standard-output* "
        <PARAM name=\"xval5\" value=\"1;3;\">
        <PARAM name=\"xval6\" value=\"1;3;\">
        <PARAM name=\"xval7\" value=\"1;3;\">
        <PARAM name=\"xval8\" value=\"1;3;\">
        <PARAM name=\"xval9\" value=\"1;3;\">
        <PARAM name=\"lcolor5\" value=\"0\">
        <PARAM name=\"lcolor6\" value=\"1\">
        <PARAM name=\"lcolor7\" value=\"2\">
        <PARAM name=\"lcolor8\" value=\"3\">
        <PARAM name=\"lcolor9\" value=\"4\">
        <PARAM name=\"lstyle5\" value=\"6553\">
        <PARAM name=\"lstyle6\" value=\"6553\">
        <PARAM name=\"lstyle7\" value=\"6553\">
        <PARAM name=\"lstyle8\" value=\"6553\">
        <PARAM name=\"lstyle9\" value=\"6553\">
        <PARAM name=\"name5\" value=\"Experiment Data (~a)\">
        <PARAM name=\"name6\" value=\"Experiment Data (~a)\">
        <PARAM name=\"name7\" value=\"Experiment Data (~a)\">
        <PARAM name=\"name8\" value=\"Experiment Data (~a)\">
        <PARAM name=\"name9\" value=\"Experiment Data (~a)\">
         "    
              (nth 0 data-names)
              (nth 1 data-names) 
              (nth 2 data-names)
              (nth 3 data-names) 
              (nth 4 data-names))
      

      (dotimes (i 5)
        (format *standard-output* "<PARAM name=\"yval~s\" value=\"" (+ 5 i))
        (format *standard-output* "~6,3f;~6,3f;\">" (first (nth i  *problem-size-over-time-data-ch9*)) 
                (second (nth i  *problem-size-over-time-data-ch9*))))
    )
    
    (format *standard-output* "
             <HR> Either your browser does not support JAVA or this graph has scrolled off the top of the display.~%
             </HR></applet>"))))


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

(defun output-addition-ch9 (data sim)
    (when sim
      (format *standard-output* "~%~%Parameters for run: (~S ~S ~S)~%" 
              *noise* *mismatch* *thresh*))

  (when *text*
    (format *standard-output* "~%~A data:~%" (if sim "Simulation" "Experimental"))
    (format *standard-output* "~%                                  Retrieval Percentages~%")
    (format *standard-output* "Problem   0     1     2     3     4     5     6     7     8     9    10    11   Other~%")
    (dotimes (i 5)
      (dotimes (j 5)
        (format *standard-output* "~s + ~s " (+ i 1) (+ 1 j))
        (dotimes (k 13)
          (format *standard-output* "~6,1F" (aref (third data) i j k)))
        (format *standard-output* "~%")))


    (when (and sim *overlay*)
      (format *standard-output* "~%Experimental data:~%")
      (format *standard-output* "~%                                  Retrieval Percentages~%")
    (format *standard-output* "Problem   0     1     2     3     4     5     6     7     8     9    10    11   Other~%")
    (dotimes (i 5)
      (dotimes (j 5)
        (format *standard-output* "~s + ~s " (+ 1 i) (+ 1 j))
        (dotimes (k 13)
          (format *standard-output* "~6,1F" (aref (third *addition-data-ch9*) i j k)))
        (format *standard-output* "~%"))))


    (format *standard-output* "~%~A data:~%" (if sim "Simulation" "Experimental"))
    (format *standard-output* "            Percentage Correct~%")
    (format *standard-output* "Number    As Augend    As Addend~%")
    (dotimes (i 5)
      (format *standard-output* "~4d~12,1f~13,1f~%" (+ 1 i) (nth i (first data)) (nth i (second data))))

    (when (and sim *overlay*)
      (format *standard-output* "~%Experimental data:~%")
      (format *standard-output* "            Percentage Correct~%")
      (format *standard-output* "Number    As Augend    As Addend~%")
      (dotimes (i 5)
        (format *standard-output* "~4d~12,1f~13,1f~%" (+ 1 i) (nth i (first *addition-data-ch9*)) (nth i (second *addition-data-ch9*)))))
    
    (unless *graphic* (format *standard-output* 
                              "~%</pre>If your browser supports JAVA, you 
                               can display the data in a graph by checking 
                               the Graphic output box on the interface page.<pre>~%~%")))

  (when *graphic* 
    (output-graph-addition-ch9 (first data) (first *addition-data-ch9*) sim "Augend")
    (output-graph-addition-ch9 (second data) (second *addition-data-ch9*) sim "Addend")))

(defun output-graph-addition-ch9 (data exp sim title)
  
    (format *standard-output* " 
        <applet 
        code = \"DansGraphs.class\" 
        width = 400 
        height = 500> 

        <PARAM name=\"title\" value=\"Addition Retrieval (~A)\">
        <PARAM name=\"xmin\" value=\"0\">
        <PARAM name=\"xmax\" value=\"6\">
        <PARAM name=\"ymax\" value=\"80\">
        <PARAM name=\"ymin\" value=\"0\">
        <PARAM name=\"xval0\" value=\"1;2;3;4;5;\">
        <PARAM name=\"xspacing\" value=\"1\">
        <PARAM name=\"longestline\" value=\"5\">
        <PARAM name=\"numlines\" value=\"~S\">
        <PARAM name=\"ydiv\" value=\"10\">
        <PARAM name=\"yspacing\" value=\"20\">
        <PARAM name=\"lcolor0\" value=\"0\">
        <PARAM name=\"lstyle0\" value=\"~s\">
        <PARAM name=\"xname\" value=\"~A\">
        <PARAM name=\"yname\" value=\"% Correct\">
        <PARAM name=\"name0\" value=\"~a\">
" 
            title
            (if (and sim *overlay*) 2 1)
            (if sim 2 6553)
            title
            (if sim "Simulation Data" "Experiment Data"))
    
    (format *standard-output* "<PARAM name=\"yval0\" value=\"")
    (dolist (y data)
      (format *standard-output* "~6,3f;" y))
    (format *standard-output* "\">")
    
    (when (and *overlay* sim)
      (format *standard-output* "
        <PARAM name=\"xval1\" value=\"1;2;3;4;5;\">
        <PARAM name=\"lcolor1\" value=\"0\">
        <PARAM name=\"lstyle1\" value=\"6553\">
        <PARAM name=\"name1\" value=\"Experiment Data\">
         ")
      
     (format *standard-output* "<PARAM name=\"yval1\" value=\"")
    (dolist (y exp)
      (format *standard-output* "~6,3f;" y))
    (format *standard-output* "\">"))
    
    (format *standard-output* "
             <HR> Either your browser does not support JAVA or this graph has scrolled off the top of the display.~%
             </HR></applet>"))


(defun translate-frequencies-ch9 (&optional (multiplication *multiplication-frequency-ch9*))
  (let ((addition (make-array '(10 10) :initial-element 0))
        (total 0.0))
    (dotimes (multiplicand 10)
      (dotimes (multiplier 10)
        (let ((frequency (pop multiplication))
              (sum multiplicand))
          (dotimes (count (1- multiplier))
            (incf (aref addition sum multiplicand) frequency)
            (incf total frequency)
            (setf sum (mod (+ sum multiplicand) 10))))))
    (dotimes (i 10)
      (dotimes (j 10)
        (setf (aref addition i j) (/ (aref addition i j) total))))
    (setf *addition-frequency* addition)))



(translate-frequencies-ch9)



(defun deterministic-multiplication-sampling-ch9 (facts s mp &optional (output t))
  (let ((addition (make-array '(10 10) :initial-element 0.0))
        (multiplication (make-array '(10 10) :initial-element 1.0))
        (probabilities (make-array 100 :initial-element 0.0)))
    (addwm-fct '((goal isa arithmetic operator +)) :reset-ia nil)
    (wmfocus goal)

    (when output 
      (format *standard-output* "Probabilities Addition Correct:~%")
      (format *standard-output* "   +")
      (dotimes (i 10)
        (format *standard-output* "~5d " i)))

    (dotimes (i 10)
      (when output
        (format *standard-output* "~%~2d  " i))

      (dotimes (j 10)
        (modfocus-fct `(first ,i second ,j))
        (let* ((sum 0.0)
               (index 0))
          (dolist (fact facts)
            (setf (svref probabilities index)
                  (exp (/ (- (activation (get-safe-wme fact))
                             (* (/ mp 10.0)
                                (+ (abs (- (floor index 10) i))
                                   (abs (- (mod index 10) j)))))
                          s)))
            (incf sum (svref probabilities index))
            (incf index))
          (dotimes (k 100)
            (when (= (+ (floor k 10) (mod k 10)) (+ i j))
              (incf (aref addition i j) (/ (svref probabilities k) sum))))
          (when output  
            (format *standard-output* " ~5,1F" (* 100.0 (aref addition i j))))))
      )
    (when output 
      (format *standard-output* "~%~%Probabilities Multiplication Errors:~%")
      (format *standard-output* "   *")
      (dotimes (i 10)
        (format *standard-output* "~5d " i)))
    (dotimes (i 10)
      (when output
        (format *standard-output* "~%~2d  " i))
      (dotimes (j 10)
        (let ((sum i))
          (dotimes (k (- j 1))
            (setf (aref multiplication i j)
                  (* (aref multiplication i j) (aref addition sum i)))
            (setf sum (mod (+ sum i) 10))))
        (when output
          (format *standard-output* " ~5,1F" (* 100.0 (- 1.0 (aref multiplication i j))))))
      )
    (when output 
      (format *standard-output* "~%~%"))
    (let ((res (list nil nil)))

      (dotimes (i 7)
        (let ((sum 0.0))
          (dotimes (j 7)
            (incf sum (aref multiplication (+ i 3) (+ j 3))))
          (setf sum (* 100 (- 1.0 (/ sum 7))))
          (push-last sum (first res))))
      
      (dotimes (i 7)
        (let ((sum 0.0))
          (dotimes (j 7)
            (incf sum (aref multiplication (+ j 3) (+ i 3))))
          (setf sum (* 100 (- 1.0 (/ sum 7))))
          (push-last sum (second res))))
      
      (output-multiplication-ch9 res t))))

(defun output-multiplication-ch9 (data sim)
  (when sim
      (format *standard-output* "~%~%Parameters for run: (~S ~S)~%" 
              *noise* *mismatch*))

  (when *text*
    (format *standard-output* "~%~A data:~%" (if sim "Simulation" "Experimental"))
    (format *standard-output* "                    Error Percentage~%")
    (format *standard-output* "Number    As Multiplicand    As Multiplier~%")
    (dotimes (i 7)
      (format *standard-output* "~4d~17,1f~16,1f~%" (+ 3 i) (nth i (first data)) (nth i (second data))))

    (when (and sim *overlay*)
      (format *standard-output* "~%Experimental data:~%")
      (format *standard-output* "            Error Percentage~%")
      (format *standard-output* "Number    As Multiplicand    As Multiplier~%")
      (dotimes (i 7)
        (format *standard-output* "~4d~17,1f~16,1f~%" (+ 3 i) (nth i (first *multiplication-data-ch9*)) (nth i (second *multiplication-data-ch9*)))))
    
    (unless *graphic* (format *standard-output* 
                              "~%</pre>If your browser supports JAVA, you 
                               can display the data in a graph by checking 
                               the Graphic output box on the interface page.<pre>~%~%")))

  (when *graphic* 
    (output-graph-multiplication-ch9 (first data) (first *multiplication-data-ch9*) sim "Multiplicand")
    (output-graph-multiplication-ch9 (second data) (second *multiplication-data-ch9*) sim "Multiplier")))

(defun output-graph-multiplication-ch9 (data exp sim title)
  
    (format *standard-output* " 
        <applet 
        code = \"DansGraphs.class\" 
        width = 400 
        height = 500> 

        <PARAM name=\"title\" value=\"Multiplication Computation (~A)\">
        <PARAM name=\"xmin\" value=\"2\">
        <PARAM name=\"xmax\" value=\"10\">
        <PARAM name=\"ymax\" value=\"60\">
        <PARAM name=\"ymin\" value=\"0\">
        <PARAM name=\"xval0\" value=\"3;4;5;6;7;8;9;\">
        <PARAM name=\"xspacing\" value=\"1\">
        <PARAM name=\"longestline\" value=\"7\">
        <PARAM name=\"numlines\" value=\"~S\">
        <PARAM name=\"ydiv\" value=\"5\">
        <PARAM name=\"yspacing\" value=\"10\">
        <PARAM name=\"lcolor0\" value=\"0\">
        <PARAM name=\"lstyle0\" value=\"~s\">
        <PARAM name=\"xname\" value=\"~A\">
        <PARAM name=\"yname\" value=\"% Error\">
        <PARAM name=\"name0\" value=\"~a\">
" 
            title
            (if (and sim *overlay*) 2 1)
            (if sim 2 6553)
            title
            (if sim "Simulation Data" "Experiment Data"))
    
    (format *standard-output* "<PARAM name=\"yval0\" value=\"")
    (dolist (y data)
      (format *standard-output* "~6,3f;" y))
    (format *standard-output* "\">")
    
    (when (and *overlay* sim)
      (format *standard-output* "
        <PARAM name=\"xval1\" value=\"3;4;5;6;7;8;9;\">
        <PARAM name=\"lcolor1\" value=\"0\">
        <PARAM name=\"lstyle1\" value=\"6553\">
        <PARAM name=\"name1\" value=\"Experiment Data\">
         ")
      
     (format *standard-output* "<PARAM name=\"yval1\" value=\"")
    (dolist (y exp)
      (format *standard-output* "~6,3f;" y))
    (format *standard-output* "\">"))
    
    (format *standard-output* "
             <HR> Either your browser does not support JAVA or this graph has scrolled off the top of the display.~%
             </HR></applet>"))


(defun multiplication-computation-ch9 (&key (s 0.12) (mp .15) (ratio 4.0)
                                        (addition 150000.0) (multiplication 5000.0)
                                        (problems-per-day 100.0) (output t))
  (setf mp (* 10 mp))
  (reset)

  (let* ((addition-schedule (frequency-table-ch9 :n 10 :ratio ratio))
         (multiplication-schedule *addition-frequency*)
         (hundred-facts (subseq (no-output (sdm isa arithmetic)) 0 100))
         (facts hundred-facts)
         (problems (+ addition multiplication))
         (delay (/ 86400.0 problems-per-day)))
    (dotimes (i 10)
      (dotimes (j 10)
        (let ((fact (pop facts)))
          (dotimes (k (round (+ (* addition (aref addition-schedule i j))
                                (* multiplication (aref multiplication-schedule i j)))))
            (rehearse-chunk-fct-ch9 (list fact))))))
    (incf *time* (* problems delay))
    (incf *cycle* problems)
    (incf *spread-stamp* problems)
    (deterministic-multiplication-sampling-ch9 hundred-facts s mp output)))

;;; MODEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</xmp><pre><a name="model">
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</a>

(clear-all)

(sgp :era t
     :bll 0.5
     :al 1.0
     :v nil
     )

;;; RELATED MODELS

;;; WMETYPES

;;; The NUMBERS wmetype, used to represent numbers, holds their tens and units
;;; which are usually single-digit numbers but could be more complex.

(wmetype NUMBER)

;;; The ARITHMETIC wmetype, used for arithmetic goals and facts, holds
;;; the first and second operand, the operator and the result when available.

(wmetype arithmetic first operator second result)

;;; WMES

(addwm
 ;;; Operators + and * are simple wmes.
 (+ ISA CHUNK)
 (* ISA CHUNK)
 (0 ISA NUMBER)
 (1 ISA NUMBER)
 (2 ISA NUMBER)
 (3 ISA NUMBER)
 (4 ISA NUMBER)
 (5 ISA NUMBER)
 (6 ISA NUMBER)
 (7 ISA NUMBER)
 (8 ISA NUMBER)
 (9 ISA NUMBER)
 (10 ISA NUMBER)
 (11 ISA NUMBER)
 (12 ISA NUMBER)
 (13 ISA NUMBER)
 (14 ISA NUMBER)
 (15 ISA NUMBER)
 (16 ISA NUMBER)
 (17 ISA NUMBER)
 (18 ISA NUMBER)
 (0+0 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 0 RESULT (0))
 (0+1 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 1 RESULT (1))
 (0+2 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 2 RESULT (2))
 (0+3 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 3 RESULT (3))
 (0+4 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 4 RESULT (4))
 (0+5 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 5 RESULT (5))
 (0+6 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 6 RESULT (6))
 (0+7 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 7 RESULT (7))
 (0+8 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 8 RESULT (8))
 (0+9 ISA ARITHMETIC FIRST 0 OPERATOR + SECOND 9 RESULT (9))
 (1+0 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 0 RESULT (1))
 (1+1 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 1 RESULT (2))
 (1+2 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 2 RESULT (3))
 (1+3 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 3 RESULT (4))
 (1+4 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 4 RESULT (5))
 (1+5 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 5 RESULT (6))
 (1+6 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 6 RESULT (7))
 (1+7 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 7 RESULT (8))
 (1+8 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 8 RESULT (9))
 (1+9 ISA ARITHMETIC FIRST 1 OPERATOR + SECOND 9 RESULT (10))
 (2+0 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 0 RESULT (2))
 (2+1 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 1 RESULT (3))
 (2+2 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 2 RESULT (4))
 (2+3 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 3 RESULT (5))
 (2+4 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 4 RESULT (6))
 (2+5 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 5 RESULT (7))
 (2+6 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 6 RESULT (8))
 (2+7 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 7 RESULT (9))
 (2+8 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 8 RESULT (10))
 (2+9 ISA ARITHMETIC FIRST 2 OPERATOR + SECOND 9 RESULT (11))
 (3+0 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 0 RESULT (3))
 (3+1 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 1 RESULT (4))
 (3+2 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 2 RESULT (5))
 (3+3 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 3 RESULT (6))
 (3+4 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 4 RESULT (7))
 (3+5 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 5 RESULT (8))
 (3+6 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 6 RESULT (9))
 (3+7 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 7 RESULT (10))
 (3+8 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 8 RESULT (11))
 (3+9 ISA ARITHMETIC FIRST 3 OPERATOR + SECOND 9 RESULT (12))
 (4+0 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 0 RESULT (4))
 (4+1 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 1 RESULT (5))
 (4+2 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 2 RESULT (6))
 (4+3 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 3 RESULT (7))
 (4+4 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 4 RESULT (8))
 (4+5 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 5 RESULT (9))
 (4+6 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 6 RESULT (10))
 (4+7 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 7 RESULT (11))
 (4+8 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 8 RESULT (12))
 (4+9 ISA ARITHMETIC FIRST 4 OPERATOR + SECOND 9 RESULT (13))
 (5+0 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 0 RESULT (5))
 (5+1 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 1 RESULT (6))
 (5+2 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 2 RESULT (7))
 (5+3 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 3 RESULT (8))
 (5+4 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 4 RESULT (9))
 (5+5 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 5 RESULT (10))
 (5+6 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 6 RESULT (11))
 (5+7 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 7 RESULT (12))
 (5+8 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 8 RESULT (13))
 (5+9 ISA ARITHMETIC FIRST 5 OPERATOR + SECOND 9 RESULT (14))
 (6+0 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 0 RESULT (6))
 (6+1 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 1 RESULT (7))
 (6+2 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 2 RESULT (8))
 (6+3 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 3 RESULT (9))
 (6+4 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 4 RESULT (10))
 (6+5 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 5 RESULT (11))
 (6+6 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 6 RESULT (12))
 (6+7 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 7 RESULT (13))
 (6+8 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 8 RESULT (14))
 (6+9 ISA ARITHMETIC FIRST 6 OPERATOR + SECOND 9 RESULT (15))
 (7+0 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 0 RESULT (7))
 (7+1 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 1 RESULT (8))
 (7+2 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 2 RESULT (9))
 (7+3 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 3 RESULT (10))
 (7+4 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 4 RESULT (11))
 (7+5 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 5 RESULT (12))
 (7+6 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 6 RESULT (13))
 (7+7 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 7 RESULT (14))
 (7+8 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 8 RESULT (15))
 (7+9 ISA ARITHMETIC FIRST 7 OPERATOR + SECOND 9 RESULT (16))
 (8+0 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 0 RESULT (8))
 (8+1 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 1 RESULT (9))
 (8+2 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 2 RESULT (10))
 (8+3 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 3 RESULT (11))
 (8+4 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 4 RESULT (12))
 (8+5 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 5 RESULT (13))
 (8+6 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 6 RESULT (14))
 (8+7 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 7 RESULT (15))
 (8+8 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 8 RESULT (16))
 (8+9 ISA ARITHMETIC FIRST 8 OPERATOR + SECOND 9 RESULT (17))
 (9+0 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 0 RESULT (9))
 (9+1 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 1 RESULT (10))
 (9+2 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 2 RESULT (11))
 (9+3 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 3 RESULT (12))
 (9+4 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 4 RESULT (13))
 (9+5 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 5 RESULT (14))
 (9+6 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 6 RESULT (15))
 (9+7 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 7 RESULT (16))
 (9+8 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 8 RESULT (17))
 (9+9 ISA ARITHMETIC FIRST 9 OPERATOR + SECOND 9 RESULT (18)))

(SWP (0 :SIMILARITIES ((1 . 0.9) (2 . 0.8) (3 . 0.7) (4 . 0.6)
      (5 . 0.5) (6 . 0.4) (7 . 0.3) (8 . 0.2) (9 . 0.1)))
     (1 :SIMILARITIES ((0 . 0.9) (2 . 0.9) (3 . 0.8) (4 . 0.7)
      (5 . 0.6) (6 . 0.5) (7 . 0.4) (8 . 0.3) (9 . 0.2)))
     (2 :SIMILARITIES ((0 . 0.8) (1 . 0.9) (3 . 0.9) (4 . 0.8)
      (5 . 0.7) (6 . 0.6) (7 . 0.5) (8 . 0.4) (9 . 0.3)))
     (3 :SIMILARITIES ((0 . 0.7) (1 . 0.8) (2 . 0.9) (4 . 0.9)
      (5 . 0.8) (6 . 0.7) (7 . 0.6) (8 . 0.5) (9 . 0.4)))
     (4 :SIMILARITIES ((0 . 0.6) (1 . 0.7) (2 . 0.8) (3 . 0.9)
      (5 . 0.9) (6 . 0.8) (7 . 0.7) (8 . 0.6) (9 . 0.5)))
     (5 :SIMILARITIES ((0 . 0.5) (1 . 0.6) (2 . 0.7) (3 . 0.8)
      (4 . 0.9) (6 . 0.9) (7 . 0.8) (8 . 0.7) (9 . 0.6)))
     (6 :SIMILARITIES ((0 . 0.4) (1 . 0.5) (2 . 0.6) (3 . 0.7)
      (4 . 0.8) (5 . 0.9) (7 . 0.9) (8 . 0.8) (9 . 0.7)))
     (7 :SIMILARITIES ((0 . 0.3) (1 . 0.4) (2 . 0.5) (3 . 0.6)
      (4 . 0.7) (5 . 0.8) (6 . 0.9) (8 . 0.9) (9 . 0.8)))
     (8 :SIMILARITIES ((0 . 0.2) (1 . 0.3) (2 . 0.4) (3 . 0.5)
      (4 . 0.6) (5 . 0.7) (6 . 0.8) (7 . 0.9) (9 . 0.9)))
     (9 :SIMILARITIES ((0 . 0.1) (1 . 0.2) (2 . 0.3) (3 . 0.4)
      (4 . 0.5) (5 . 0.6) (6 . 0.7) (7 . 0.8) (8 . 0.9))))



