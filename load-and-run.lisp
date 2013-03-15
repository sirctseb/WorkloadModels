;; TODO use relative path or environment variable for these
;; load actr
(load "~/Documents/actr6/load-act-r-6.lisp")
;; load model
(load "~/Desktop/addition/targeting.lisp")
;; run trials
(run-trials :trials 1000 :moving t :visible nil :trace-file "trace.txt" :trace nil)
;; quit
(quit)