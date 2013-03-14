exp_root = ~/dart/WorkloadExperiment
model_root = ~/Desktop/addition
output_dir = $(model_root)/output
SHELL=/bin/bash

all: showplots

log.txt: targeting.lisp load-and-run.lisp
	. $$HOME/.profile; rm log.txt; ccl64 -l load-and-run.lisp

run-trials: $(output_dir)/log.txt

stats: $(output_dir)/aggregate.txt test.R
	RScript test.R --args stats

$(output_dir)/aggregate.txt: log.txt aggregate.go
	. $$HOME/.profile; go run aggregate.go > $(output_dir)/aggregate.txt
	
$(output_dir)/bymiss.pdf $(output_dir)/bywhiff.pdf $(output_dir)/byhover.pdf $(output_dir)/byavoid.pdf $(output_dir)/bytotalwhiff.pdf: $(output_dir)/aggregate.txt test.R
	RScript test.R --args stats plots; rm Rplots.pdf

bymiss: $(output_dir)/bymiss.pdf
	open $(output_dir)/bymiss.pdf
bywhiff: $(output_dir)/bywhiff.pdf
	open $(output_dir)/bywhiff.pdf
byhover: $(output_dir)/byhover.pdf
	open $(output_dir)/byhover.pdf
byavoid: $(output_dir)/byavoid.pdf
	open $(output_dir)/byavoid.pdf
byorder: $(output_dir)/byorder.pdf
	open $(output_dir)/byorder.pdf
bytotalwhiff: $(output_dir)/bytotalwhiff.pdf
	open $(output_dir)/bytotalwhiff.pdf

showplots: bymiss bywhiff byhover byavoid byorder bytotalwhiff
