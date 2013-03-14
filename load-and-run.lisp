;; TODO use relative path or environment variable for these
;; load actr
(load "~/Documents/actr6/load-act-r-6.lisp")
;; load model
(load "~/Desktop/addition/targeting.lisp")
;; run trials
(run-trials :trials 100 :moving t :visible nil)
;; quit
(quit)