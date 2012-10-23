$state = 0;

while(<>){
  chomp;

  # if we're starting a single-line special comment
  if(/^\s*\/\*&(.*)\*\*\//){
    print "\n```\n\n$1\n\n```scala\n";
  }

  # if we're starting a single-line special comment
  elsif(/^\s*\/\*&(.*)\*\//){
    print "\n```\n\n$1\n\n```scala\n";
  }

  # if we're starting a special comment
  elsif(/^\s*\/\*&(.*)/){
    print "\n```\n\n$1\n";
    $state = 1;
  }

  # if we're in the middle of a special comment with a leading *
  elsif(/^\s*\* (.*)/ && ($state = 1)){
    print "$1\n";
  }

  # if we're in the middle of a special comment blankline
  elsif(/^\s*\*\s*$/ && ($state = 1)){
    print "\n";
  }

  # if we're ending special comment with **/
  elsif(/^\s*(.*)\*\*\/\s*$/ && ($state = 1)){
    print "$1\n```scala\n";
    $state = 0;
  }

  # if we're ending special comment with */
  elsif(/^\s*(.*)\*\/$/ && ($state = 1)){
    print "$1\n\n```scala\n\n";
    $state = 0;
  }

  # any other line
  elsif(/(.*)/) {
    print "$1\n";
  }
}
