;; TODO use relative path or environment variable for these
;; load actr
(load "~/Documents/actr6/load-act-r-6.lisp")
;; load model
(load "~/Desktop/addition/targeting.lisp")
;; set parameters if passed on command line
(defvar trials 1000)
(when (> (length ccl:*unprocessed-command-line-arguments*) 0) (setf trials (parse-integer (car ccl:*unprocessed-command-line-arguments*))))
;; run trials
(run-trials :trials trials :moving t :visible nil :trace-file "trace.txt" :trace nil)
;; quit
(quit)