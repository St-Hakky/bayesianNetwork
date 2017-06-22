use strict;
use warnings;
my $datasetnum;
my %info;

sub escape4txt{
    my $str = shift;
    defined $str or return '';
    return $str unless ($str =~ /[,"\r\n\t]/);
    $str =~ s/"/""/g;
    $str =~ s/\r\n/\n/g;
    return $str;
}

print "num, name, organism, type, Dataset, Platforms, samples, series\n";

sub print_GDS(){
  return ($info{"num"} . "," . &escape4txt($info{"name"}) . "," . $info{"organism"} . "," . $info{"type"} . "," . $info{"dataset"} . "," . $info{"platforms"} . "," . $info{"samples"} . "," . $info{"series"} . "\n");
}

sub init_info(){
    %info = ();
    $info{"dataset"} = "NoData";
    $info{"platforms"} = "NoData";
}

init_info();
my $input_file_name = $ARGV[0];
my $output_file_name = $ARGV[1];
open(my $input_handle, "<$input_file_name");
open(my $output_handle, ">$output_file_name");
while (my $line = $input_handle) {
    #dataset name
    if($line =~ /^(\d+)\.\s(.+)$/) {
      print "aa¥n";
      %info = ();
    	$info{"num"} = $1;
    	$info{"name"} = $2;
    	next;
    }

    #Organism:
    if($line =~ /^Organism:\s+(\w.*)$/){
      print "bb¥n";
      $info{"organism"} = $1;
    	next;
    }

    #Type:
    if($line =~ /^Type:\s+(\w.*)$/){
      print "cc¥n";
      $info{"type"} = $1;
    	next;
    }

    #Platforms, Series, samples
    if($line =~ /^Platforms?:\s+(\w+)\s+Series:\s+(\w+)\s+(\d+)\s+Samples/  ){
      print "dd¥n";
      $info{"platforms"} = $1;
    	$info{"series"} = $2;
    	$info{"samples"} = $3;
    }

    #Dataset:
    if($line =~ /^DataSet\s+Accession:\s+(\w+)/  ){
      print "ee¥n";
      $info{"dataset"} = $1;
    	my $output_str = print_GDS();
      print $output_str;
      print $output_handle $output_str;
    	next;
    }
}
close($input_handle);
close($output_handle);
