#!/usr/bin/perl -w
# vi: set ts=4 sw=4 :

use strict;

use MyModProxyFilter;

my $c = join "", <>;

use HTML::Entities qw( %entity2char );
# Add missing &amp;-encoding
$c =~ s/&(\w+)(?!;)/
	exists $entity2char{$1}
		? "$1;"
		: "&amp;$1"
	/eg;

# ??? A common error is the omission of ";" after &nbsp &reg &copy etc.
# We could add it in I suppose....
$c =~ s/&(nbsp|reg|copy|trade|gt|lt|quot)(?![;\w])/&$1;/g;

# Better results without the DOCTYPE
$c =~ s/\A<!DOCTYPE .*?>\s*//s;

# Escape characters mess things up.
$c =~ tr/\x1B//d;

#print "New content:\n$c\n";

my $parser = XML::LibXML->new;
my $doc = eval { $parser->parse_html_string($c) };

if (not $doc)
{
	print "Parse error:\n$@\n";
	exit;
}

MyModProxyFilter::vDoDump(undef, $doc->getDocumentElement, 0);

# eof test.pl
