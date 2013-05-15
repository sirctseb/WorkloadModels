;; TODO use relative path or environment variable for these
;; load actr while suppressing standard output so we don't see all the version info
(let ((*standard-output* (make-broadcast-stream)))
	(load "~/Documents/actr6/load-act-r-6.lisp"))
;; load model
(load "targeting.lisp")
;; load number parser
(load "parse-number.lisp")
;; set parameters if passed on command line
(defvar trials 1000)
(let* ((args ccl:*unprocessed-command-line-arguments*) (difficult (if (equal (nth 0 args) "t") t nil)) (moving (if (equal (nth 1 args) "t") t nil)))
	(when (> (length ccl:*unprocessed-command-line-arguments*) 2) (setf trials (parse-integer (nth 2 ccl:*unprocessed-command-line-arguments*))))
	(when (> (length ccl:*unprocessed-command-line-arguments*) 3) (setf *target-projection-alpha* (parse-integer (nth 3 ccl:*unprocessed-command-line-arguments*))))
	(when (> (length ccl:*unprocessed-command-line-arguments*) 4) (setf *target-projection-beta* (parse-number (nth 4 ccl:*unprocessed-command-line-arguments*))))
	(when (> (length ccl:*unprocessed-command-line-arguments*) 5) (setf *whiff-wait-time* (parse-integer (nth 5 ccl:*unprocessed-command-line-arguments*))))
	;; run trials
	(run-trials :trials trials :moving moving :difficult difficult :visible nil :trace-file "trace.txt" :trace nil))
;; quit
(quit)