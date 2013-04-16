#!/usr/bin/perl
use Getopt::Long;
use File::Copy;

# set default values for command line options
my $addition = 0;
my $targeting = 1;
my $difficulty = easy;
my $speed = slow;

# parse command line options
GetOptions('a' => \$addition, 't' => \$targeting, 'd=s' => \$difficulty, 's=s' => \$speed);

print "addition:" . $addition . "\n";
print "targeting:" . $targeting . "\n";
print "difficulty:" . $difficulty . "\n";
print "speed:" . $speed . "\n";

# put shared header into resulting file
copy("qnactr/header.txt", "qnactr/qnactr_model.txt") or die "Copy failed $!";

# open result file for further addition
open(modelfile, ">>qnactr/qnactr_model.txt");

# if addition is enabled, put in addition header stuff
if($addition) {
	# copy in contents of addition header file
	open(additionheader, "qnactr/additionheader.txt");
	while(<additionheader>) {
		print modelfile $_;
	}
	close(additionheader);
}

# if targeting is enabled, put in targeting header stuff
if($targeting) {
	# copy in contents of targeting header file
	open(targetingheader, "qnactr/targetheader.txt");
	while(<targetingheader>) {
		# uncomment conditional lines
		s/^([^;]*);([^;]*; $difficulty)/$1$2/g;
		print modelfile $_;
	}
	close(targetingheader);
}

# put closing paren in for header
print modelfile ")\n";

# if addition is enabled, put in actual addition model stuff
if($addition) {
	# scan lines of addition model file
	open(additionmodel, "addition.lisp");
	# flag to store whether we have gotten to the model definition yet
	my $model_def = 0;
	while(<additionmodel>) {
		if($model_def && !/; end model/) {
			if(!badline($_)) {
				# copy line to output file
				print modelfile $_;
			}
		} elsif(/define-model/) { # check for define-model call
			# set flag to true
			$model_def = 1;
		}
	}
	close(additionmodel);
}

sub badline {
	if(/; end model/ || /!eval!/ || /:needs-mouse/ || /:vwt/ || /:trace-detail/) { return 1; }
	return 0;
}

# if targeting is enabled, determine which model file to read
if($targeting) {
	# error check
	if($difficulty ne easy && $difficulty ne hard || $speed ne slow && $speed ne fast) {
		die "difficulty must be easy or hard and speed must be slow or fast";
	}
	# create file name
	my $targetfile = "targeting-" . $difficulty . "-" . $speed . ".lisp";
	# open file
	open(targetmodel, $targetfile);
	# flag to store whether we have gotten to the model definition yet
	my $model_def = 0;
	while(<targetmodel>) {
		if($model_def) {
			# check for inelligible lines before copying
			if(!badline($_)) {
				# if we are also doing addition, convert to goal-2
				if($addition) {
					s/=goal>/=goal-2>/g;
					s/goal-focus/goal-2-focus/g;
				}
				# copy line to output file
				print modelfile $_;
			}
		} elsif(/define-model/) {
			# set flag to true
			$model_def = 1;
		}
	}
	close(targetmodel);

}

close(modelfile);