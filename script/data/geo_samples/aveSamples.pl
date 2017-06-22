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
    printf "%d,\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",%d,\"%s\"\n",
    $info{"num"},&escape4txt($info{"name"}),$info{"organism"},
    $info{"type"},$info{"dataset"},$info{"platforms"},$info{"samples"},
    $info{"series"};
}

sub init_info(){
    %info = ();
    $info{"dataset"} = "NoData";
    $info{"platforms"} = "NoData";
}

init_info();
while (my $line = <STDIN>) {

    #dataset name
    if($line =~ /^(\d+)\.\s(.+)$/) {
	%info = ();
	$info{"num"} = $1;
	$info{"name"} = $2;
	next;
    }
    #Organism:
    if($line =~ /^Organism:\s+(\w.*)$/){
	$info{"organism"} = $1;
	next;
    }
    #Type:
    if($line =~ /^Type:\s+(\w.*)$/){
	$info{"type"} = $1;
	next;
    }
    #Platforms, Series, samples
    if($line =~ /^Platforms?:\s+(\w+)\s+Series:\s+(\w+)\s+(\d+)\s+Samples/  ){
	$info{"platforms"} = $1;
	$info{"series"} = $2;
	$info{"samples"} = $3;
    }
    #Dataset:
    if($line =~ /^DataSet\s+Accession:\s+(\w+)/  ){
	$info{"dataset"} = $1;
	print_GDS();
	next;
    }
}
