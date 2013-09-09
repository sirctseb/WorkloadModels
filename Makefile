model_root = .
output_dir = $(model_root)/output
exp_dir = ~/Documents/WorkloadExperiment
SHELL=/bin/bash
TRIALS=8
MODEL_TRIALS=480
SAVE=saved
DIFFICULT=t
MOVING=t
# whether addition should be put in the qn-actr model file
ADDITION=t
# whether targeting should be put in the qn-actr model file
TARGETING=t

all: showplots

log.txt: targeting.lisp load-and-run.lisp
	. $$HOME/.profile; rm log.txt; ccl64 -l load-and-run.lisp -- $(DIFFICULT) $(MOVING) $(TRIALS)

run-trials: log.txt

stats: $(output_dir)/aggregate.txt test.R
	RScript test.R --stats

$(output_dir)/aggregate.txt: log.txt aggregate.go
	. $$HOME/.profile; go run aggregate.go > $(output_dir)/aggregate.txt
	
$(output_dir)/bymiss.pdf $(output_dir)/bywhiff.pdf $(output_dir)/byhover.pdf $(output_dir)/byavoid.pdf $(output_dir)/bytotalwhiff.pdf: $(output_dir)/aggregate.txt test.R
	RScript test.R --stats --plots; rm Rplots.pdf

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

$(output_dir)/table.txt: targeting.lisp load-and-run.lisp
	echo "misses, whiffs, totalwhiffs, avoids, accuracy, proj, whifftime" > $(output_dir)/table.txt; \
	for proj in 55 56 57 58 59 60 ; do \
		for whiff in 16 18 20 22 24 ; do \
			rm log.txt; \
			. $$HOME/.profile; ccl64 -l load-and-run.lisp -- $(DIFFICULT) $(MOVING) $(TRIALS) $$proj $$whiff; \
			. $$HOME/.profile; go run aggregate.go > $(output_dir)/aggregate.txt; \
			RScript test.R --table --projection $$proj --whifftime $$whiff; \
		done \
	done
projection-table: $(output_dir)/table.txt

save: log.txt $(output_dir)/*.pdf $(output_dir)/aggregate.txt
	mkdir -p output/$(SAVE)
	cp log.txt $(output_dir)/*.pdf $(output_dir)/aggregate.txt output/$(SAVE)

$(output_dir)/compare.pdf: $(output_dir)/$(COMPARE)/aggregate.txt $(output_dir)/$(TO)/aggregate.txt
	RScript test.R --compare $(COMPARE) --to $(TO)
compare: $(output_dir)/compare.pdf
	open $(output_dir)/compare.pdf

qnactralways:

qnactr: targeting-hard-slow.lisp addition.lisp generateQNfile.pl qnactr/header.txt qnactr/additionheader.txt qnactr/targetheader.txt qnactralways
	for trial in {1..$(TRIALS)}; do \
\
		perl generateQNfile.pl -x $$trial -m $(MODELNUM) -n $(MODEL_TRIALS) -a -o easy -i no && mkdir -p qnactr/block1/trial$$trial && cp qnactr/qnactr_model.txt qnactr/block1/trial$$trial/QN_ACTR_Model_Initialization.txt && cp qnactr/block.txt qnactr/block1/trial$$trial/block.txt; \
		perl generateQNfile.pl -x $$trial -m $(MODELNUM) -n $(MODEL_TRIALS) -a -o hard -i no && mkdir -p qnactr/block2/trial$$trial && cp qnactr/qnactr_model.txt qnactr/block2/trial$$trial/QN_ACTR_Model_Initialization.txt && cp qnactr/block.txt qnactr/block2/trial$$trial/block.txt; \
\
		perl generateQNfile.pl -x $$trial -m $(MODELNUM) -n $(MODEL_TRIALS) -t -d hard -s slow -i no && mkdir -p qnactr/block3/trial$$trial && cp qnactr/qnactr_model.txt qnactr/block3/trial$$trial/QN_ACTR_Model_Initialization.txt && cp qnactr/block.txt qnactr/block3/trial$$trial/block.txt; \
\
		perl generateQNfile.pl -x $$trial -m $(MODELNUM) -n $(MODEL_TRIALS) -a -o easy -t -d hard -s slow -i no && mkdir -p qnactr/block4/trial$$trial && cp qnactr/qnactr_model.txt qnactr/block4/trial$$trial/QN_ACTR_Model_Initialization.txt && cp qnactr/block.txt qnactr/block4/trial$$trial/block.txt; \
		perl generateQNfile.pl -x $$trial -m $(MODELNUM) -n $(MODEL_TRIALS) -a -o hard -t -d hard -s slow -i no && mkdir -p qnactr/block5/trial$$trial && cp qnactr/qnactr_model.txt qnactr/block5/trial$$trial/QN_ACTR_Model_Initialization.txt && cp qnactr/block.txt qnactr/block5/trial$$trial/block.txt; \
\
\
		perl generateQNfile.pl -x $$trial -m $(MODELNUM) -n $(MODEL_TRIALS) -a -o easy -i yes && mkdir -p qnactr/block6/trial$$trial && cp qnactr/qnactr_model.txt qnactr/block6/trial$$trial/QN_ACTR_Model_Initialization.txt && cp qnactr/block.txt qnactr/block6/trial$$trial/block.txt; \
		perl generateQNfile.pl -x $$trial -m $(MODELNUM) -n $(MODEL_TRIALS) -a -o hard -i yes && mkdir -p qnactr/block7/trial$$trial && cp qnactr/qnactr_model.txt qnactr/block7/trial$$trial/QN_ACTR_Model_Initialization.txt && cp qnactr/block.txt qnactr/block7/trial$$trial/block.txt; \
\
		perl generateQNfile.pl -x $$trial -m $(MODELNUM) -n $(MODEL_TRIALS) -t -d hard -s slow -i yes && mkdir -p qnactr/block8/trial$$trial && cp qnactr/qnactr_model.txt qnactr/block8/trial$$trial/QN_ACTR_Model_Initialization.txt && cp qnactr/block.txt qnactr/block8/trial$$trial/block.txt; \
\
		perl generateQNfile.pl -x $$trial -m $(MODELNUM) -n $(MODEL_TRIALS) -a -o easy -t -d hard -s slow -i yes && mkdir -p qnactr/block9/trial$$trial && cp qnactr/qnactr_model.txt qnactr/block9/trial$$trial/QN_ACTR_Model_Initialization.txt && cp qnactr/block.txt qnactr/block9/trial$$trial/block.txt; \
		perl generateQNfile.pl -x $$trial -m $(MODELNUM) -n $(MODEL_TRIALS) -a -o hard -t -d hard -s slow -i yes && mkdir -p qnactr/block10/trial$$trial && cp qnactr/qnactr_model.txt qnactr/block10/trial$$trial/QN_ACTR_Model_Initialization.txt && cp qnactr/block.txt qnactr/block10/trial$$trial/block.txt; \
	done

publish-models: qnactr
	mkdir -p $(exp_dir)/output/subject$(MODELNUM)/block{1,2,3,4,5,6,7,8,9,10}/trial{1..$(TRIALS)}
	for block in 1 2 3 4 5 6 7 8 9 10; do \
		for trial in {1..$(TRIALS)}; do \
			cp qnactr/block$$block/trial$$trial/block.txt $(exp_dir)/output/subject$(MODELNUM)/block$$block; \
			cp qnactr/block$$block/trial$$trial/QN_ACTR_Model_Initialization.txt $(exp_dir)/output/subject$(MODELNUM)/block$$block/trial$$trial; \
		done \
	done

clean:
	rm log.txt output/*