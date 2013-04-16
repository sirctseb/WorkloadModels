use GetOpt::Long;

# set default values for command line options
my $addition = 0
my $targeting = 1
my $difficulty = easy
my $speed = slow

# parse command line options
GetOptions('a' => \$addition, 't' => \$targeting, 'd=s' => \$difficulty, 's=s' => \$speed)

# put shared header into resulting file


if($addition) {

}