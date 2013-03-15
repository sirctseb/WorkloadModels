;; TODO use relative path or environment variable for these
;; load actr
(load "~/Documents/actr6/load-act-r-6.lisp")
;; load model
(load "~/Desktop/addition/targeting.lisp")
;; set projection parameters if passed on command line
(when (> (length ccl:*unprocessed-command-line-arguments*) 0) (setf *target-projection* (parse-integer (car ccl:*unprocessed-command-line-arguments*))))
(when (> (length ccl:*unprocessed-command-line-arguments*) 1) (setf *whiff-wait-time* (parse-integer (cadr ccl:*unprocessed-command-line-arguments*))))
;; run trials
(run-trials :trials 1000 :moving t :visible nil :trace-file "trace.txt" :trace nil)
;; quit
(quit)