#!/usr/bin/perl -w
# vi: set ts=4 sw=4 :

use strict;

package MyModProxyFilter;

use Apache::Reload;
use Apache::Log;
use Apache::Constants ':common', ':http';
use URI;
use Compress::Zlib;

my %debug = (
		default		=> 0,
		content		=> 0,
		response	=> 0,
		sendresponse=> 0,
		request		=> 0,
		munge		=> 0,
		mungescript	=> 0,
		mungefail	=> 0,
		fetch		=> 0,
		passthrough	=> 0,
		block		=> 0,
		imagemagick	=> 0,
		handler		=> 0,
		gzip		=> 0,
		charset		=> 0,
		libxml		=> 0,
);

sub DEBUG(;$)
{
	my $v = $debug{$_[0] || "default"};
	defined($v) ? $v : $debug{'default'};
}

my $fHaveImageMagick = eval { require Image::Magick; 1 };
my $fHaveLibXML = eval 'use XML::LibXML; 1';

if ($ENV{MOD_PERL}){
for (Apache->server)
{
	$_->log->info(sprintf "Starting %s %s Image::Magick and %s XML::LibXML",
		__PACKAGE__,
		$fHaveImageMagick ? "with" : "without",
		$fHaveLibXML ? "with" : "without",
	);
}
}

my %hNodeTypes = hGetNodeTypes();
my @aRemoveRules = aRemoveRules();
my @arcOtherMungeRules = arcOtherMungeRules();

my %hStandardAdSizes = map { ($_->[0] . "x" . $_->[1]), 1 } araStandardAdSizes();
my @araStandardAdSizes = araStandardAdSizes();

################################################################################
# Handlers
################################################################################

sub handler
{
	my $r = shift;

	my $fProxy = $r->proxyreq ? 1 : 0;
	my $sURI = $r->uri;
	my $fXHeader = $r->header_in("X-Reentrant-Proxy");

	return fixup_handler($r, $sURI) if $fProxy and not $fXHeader;

	return DECLINED;
}

sub fixup_handler
{
	my ($r, $sURI) = @_;

	if (fBlockURL($r, $sURI))
	{
		my $sReferrer = $r->header_in("Referer") || "";
		$r->log->info("blocking $sURI (Referer: $sReferrer)")
			if DEBUG "block";
		$r->custom_response(HTTP_FORBIDDEN, "Blocked by MyModProxyFilter");
		return HTTP_FORBIDDEN;
	}

	return DECLINED if $sURI =~ /nytview\.html/;

	$r->log->debug("Setting handler to perl-script") if DEBUG "handler";
	$r->handler("perl-script");
	$r->push_handlers(PerlHandler => \&content_handler);

	return DECLINED;
}

sub content_handler
{
	my ($r, $sURL) = @_;
	my $sURI = $r->uri;

	$r->log->debug("PerlHandler!") if DEBUG "handler";

	use LWP::UserAgent;
	my $ua = new LWP::UserAgent;

	my $request = HTTP::Request->new($r->method, $r->uri);
	$request->header("X-Reentrant-Proxy", "1");

	my %h = $r->headers_in;
	while (my ($k, $v) = each %h)
	{
		$request->header($k => $v);
	}

	if (my $cl = $r->header_in("Content-Length"))
	{
		$r->log->debug("Reading $cl bytes content") if DEBUG "request";
		$r->read(my $c, $cl);
		$request->content($c);
		$r->log->debug("Done reading $cl bytes content") if DEBUG "request";
	}

	vRewriteRequest($r, $request);

	$r->log->info("Made request:\n" . $request->as_string) if DEBUG "request";
	
	my $iSize = 0;
	my $fHandled = 0;
	my $sData = "";

	my $meth = "send_request";
	$meth = "simple_request" unless $ua->can($meth);

	my $response = $ua->$meth(
		$request,
		sub {
			my ($data, $response, $protocol) = @_;

			$r->print($data), return
				if $fHandled;

			my $i = length($data);
			$iSize += $i;
			$r->log->debug(sprintf("Rx +%6d => %10d", $i, $iSize))
				if DEBUG "fetch";
			$sData .= $data;

			return unless $iSize > 500_000;
			
			# Okay, this is getting big.
			# Send what we've seen so far, and signal that the rest
			# is to be passed through too.
			$r->log->info("Response so far is $iSize bytes, so passing through")
				if DEBUG "passthrough";

			send_http_response_headers($r, $response);
			$r->print($sData);
			$sData = "";
			$fHandled = 1;
		},
		8192,
	);

	$r->log->debug("Response was passed through, so returning") if $fHandled and DEBUG "passthrough";
	return OK if $fHandled;

	# Sometimes (e.g. short "404" responses) the content is already in $response,
	# so we don't want to assign $sData to it as that would overwrite it with nothing.

	${ $response->content_ref } = $sData
		unless $sData eq ""
		and $response->content ne "";

	if (DEBUG("response"))
	{
		vRemoveCompression($r, $response) ;
		$r->log->info("Got response (after removing any compression):\n" . $response->as_string);
	}

	my $code = "" . $response->code;
	vRewriteResponse($r, $response) if $code =~ /\A2\d\d\z/;
	
	send_http_response_headers($r, $response);

	unless ($r->header_only)
	{
		$r->log->info("Returning content:\n" . $response->content) if DEBUG("sendresponse");
		$r->print($response->content);
	}

	return OK;
}

sub send_http_response_headers
{
	my ($r, $response) = @_;

	$r->status_line($response->status_line);

	$response->headers->scan(sub {
		my ($k, $v) = @_;

		$r->content_type($v), return
			if lc($k) eq "content-type";
		
		$r->content_encoding($v), return
			if lc($k) eq "content-encoding";
		
		$r->content_languages([$v =~ /(\w+)/g]), return
			if lc($k) eq "content-language";

		$r->log->debug("NOT setting response header $k: $v"), return
			if $k =~ /^content-/;

		# Sometimes there's a stray newline to remove
		chomp $v;

		$r->headers_out->add($k, $v);
	});

	if (DEBUG("sendresponse"))
	{
		use Data::Dumper;
		$r->log->info(Data::Dumper->Dump([ +{$r->headers_out} ],[ '*headers_out' ]));
	}

	$r->send_http_header;
}

################################################################################
# Adjust / filter the request
################################################################################

sub fBlockURL
{
	my ($r, $sURI) = @_;
	
	my $roURI = URI->new($sURI);
	return unless $roURI->can("host");

	my $sServer = lc($roURI->host || "");

	return 1 if $sURI =~ m[/
		(?:
			adserver
			| ad(s|x|)
			| banners?
			| clicks?
		)
		(?:/|\?|$)
		]ix;
		
	return 1 if $sServer eq "ins1.opera.com";
	return 1 if $sServer =~ /^(?:ads?|banners?)\./;
	return 1 if $sServer =~ /\bstats|stats\b/;
	return 1 if $sServer =~ /ads?server/;

	return 1 if $sServer =~ /click/;

	return 1 if $sServer =~ /
		(^|\.)
		(?:
			\d+\.g\.akamai\.net
			# | a\d+\.g\.akamai\.net
			| bluestreak\.com
			| yimg\.com
			| atwola\.com
			| sfads\.osdn\.com
			| atdmt\.com
			| falkag\.net
			| googlesyndication\.com
			| advertising\.com
		)
		$
	/x;

	$r->pnotes(XMLProxy_Server => $sServer);
	$r->pnotes(XMLProxy_URIObj => $roURI);
	$r->pnotes(XMLProxy_URI => $roURI);

	0;
}

sub vRewriteRequest
{
	my ($r, $roHTTPReq) = @_;

	my $headers = $roHTTPReq->headers;
	if (($headers->header("Cache-Control")||"") eq "max-age=0")
	{
		$headers->scan(sub {
			$headers->remove_header($_[0])
				if $_[0] =~ /^If-/i;
		});
	}

	my $uri = $roHTTPReq->uri;

	if ($uri->host eq "www.formula1.com"
		and $uri->path =~ /\.html$/
		and not $uri->query)
	{
		$uri->query("_");
		$roHTTPReq->uri($uri);
	}

	if ($uri->host eq "www.audiogalaxy.com")
	{
		$roHTTPReq->headers->remove_header("accept-encoding");
	}

	if ($uri->host =~ /nytimes/)
	{
		# Hah!  NY Times is obviously on to http://www.majcher.com/nytview.html
		# - remove the "Referer" and suddenly it works again.
		$roHTTPReq->headers->remove_header("referer");
	}
}

################################################################################
# Rewrite the response; XML/HTML or image.
################################################################################

sub vRemoveCompression
{
	my ($r, $roHTTPResp) = @_;

	my $enc = $roHTTPResp->header("content-encoding") || "";
	return unless $enc eq "gzip";

	my $c = $roHTTPResp->content_ref;

	$r->log->debug("Before gunzip: length=" . length($$c)) if DEBUG "gzip";
	$$c = Compress::Zlib::memGunzip($$c);
	$r->log->debug("After gunzip: length=" . length($$c)) if DEBUG "gzip";

	$roHTTPResp->headers->remove_header("content-encoding");
}

sub vRewriteResponse
{
	my ($r, $roHTTPResp) = @_;
	my $c = $roHTTPResp->content_ref;

	my $ct = $roHTTPResp->content_type();

	if ($ct =~ /^text-html\b/i)
	{
		$ct = "text/html";
		$roHTTPResp->content_type($ct);
		$r->log->info("Fixing text-html content-type for " . $r->pnotes("XMLProxy_URI"));
		$r->pnotes("text-html", 1);
	}

	#vFilterCookies($r, $roHTTPResp);

	return unless $ct =~ /\Aimage\// or $ct =~ /text\/html/;

	vRemoveCompression($r, $roHTTPResp);
	return if $roHTTPResp->header("content-encoding");

	# Dumbass Microsoft error message page
	if ($roHTTPResp->content_type() =~ /text-html/)
	{
		my $ct = $roHTTPResp->content_type();
		my $oct = $ct;
		$ct =~ s[^(\w+)-(\w+)$][$1/$2];
		$roHTTPResp->content_type($ct);

		$r->log->info("Munged content-type from $oct to $ct")
			if $ct ne $oct
			and DEBUG("contenttype");
	}

	return vRewriteImage(@_) if $roHTTPResp->content_type() =~ m[\Aimage/]
		and $fHaveImageMagick;
	return vRewriteHTML(@_)  if $roHTTPResp->content_type() eq "text/html"
		and $fHaveLibXML;
	return vRewriteJavascriptContent(@_)  if $roHTTPResp->content_type() =~ /\bjavascript\b/i;
}

sub vFilterCookies
{
	my ($r, $roHTTPResp) = @_;
	my $jar = MyJar->new();
	$jar->extract_cookies($roHTTPResp);
	
	use Data::Dumper;
	$r->log->info(Data::Dumper->Dump([ $roHTTPResp ],[ 'resp' ]));
	$r->log->info(Data::Dumper->Dump([ $jar ],[ 'jar' ]));

	$jar->scan(sub {
		my ($version,$key,$val,$path,$domain,$port,
                     $path_spec,$secure,$expires,$discard,$rest) = @_;
		$r->log->info(Data::Dumper->Dump([ \@_ ],[ 'jar-cb' ]));
	});
}

{
	package MyJar;
	use base qw( HTTP::Cookies );
	sub set_cookie_ok
	{
		my ($self, $hashref) = @_;
		my $r = Apache->request;
		$r->log->info(Data::Dumper->Dump([ \@_ ],[ 'jar-ok' ]));
		1;
	}
}

sub fBlockJavascript
{
	my ($r, $rJS) = @_;

	if ($$rJS =~ /document\.write/)
	{
		for (@araStandardAdSizes)
		{
			my ($x, $y) = @$_;
			return 1 if $$rJS =~ /\b$x\b.*\b$y\b/;
		}

		return 1 if $$rJS =~ /
			(?:
			casino
			| click\x20here!
			)
		/ix;
	}

	0;
}

sub vRewriteJavascriptContent
{
	my ($r, $roHTTPResp) = @_;

	return unless fBlockJavascript($r, $roHTTPResp->content_ref);

	my $sURI = $r->pnotes('XMLProxy_URIObj');
	$r->log->info("Blocking Javascript from $sURI") if DEBUG("mungescript");

	$_[1] = new HTTP::Response(
		404,
		"Ad-writing Javascript blocked",
		undef,
		"Ad-writing Javascript blocked",
	);
}

sub vRewriteHTML
{
	my ($r, $roHTTPResp) = @_;
	my $c = $roHTTPResp->content_ref;

	return if $$c =~ /<frameset\b/i;

	my $sEncoding = "iso-8859-1";
	$sEncoding = $1 if $roHTTPResp->content_type() =~ /charset="?([\w-]+)/;

	$r->log->debug("Encoding is $sEncoding") if DEBUG "charset";

	$$c = XML::LibXML::encodeToUTF8($sEncoding, $$c)
		if defined $sEncoding and $sEncoding ne "utf-8"
		and 1;

	# ??? A common error is the omission of ";" after &nbsp &reg &copy etc.
	# We could add it in I suppose....
	$$c =~ s/&(nbsp|reg|copy|trade|gt|lt|quot)(?![;\w])/&$1;/g;

	$r->log->debug("Making parser") if DEBUG "libxml";
	my $parser = XML::LibXML->new;

	# libxml throws away empty text nodes
	# (e.g. the space between <a>...</a> <a>...</a> disappears).
	# Try to remove space before tags within HEAD,
	# and preserve spaces within the BODY.

	if (my $i = index(lc($$c), "<body"))
	{
		my ($head, $body) = (substr($$c, 0, $i), substr($$c, $i));

		#$r->log->error("HEAD is hopefully: $head");
		#my $n = ($head =~ s/\s+(?=<(?:style|script|meta|link|title)\b)//sig);
		#$r->log->error("Removed $n bits of whitespace from <head> (hopefully)") if DEBUG;

		$body =~ s/>\s(\s*)<(?!(?:!|\/script))/>&#32;$1</g;

		$$c = $head . $body;
	}

	# Better results without the DOCTYPE
	$$c =~ s/\A<!DOCTYPE .*?>\s*//s;

	# Escape characters mess things up.
	$$c =~ tr/\x1B//d;

	# As do so-called "smart quotes"
	$$c =~ s/[\x92\x94](?=[st]\b)/'/g;
	$$c =~ s/[\x92\x94]/"/g;

	# And other bytes
	$$c =~ tr/\x80-\x9F/?/;

	# And the <nobr> tag
	$$c =~ s[</?nobr>][]gi;

	#$r->log->error("Parsing content: " . substr($$c, 0, 100) . "...") if DEBUG;
	eval
	{
		my %hArgs = do {
			my @t = $r->args;
			push @t, "" if @t % 2;
			@t;
		};

		return if $hArgs{"nomunge"};

		$r->log->info("Parsing content:\n" . $$c) if DEBUG("content");
		my $doc = $parser->parse_html_string($$c);

		vDumpXMLDoc($r, $doc) if $hArgs{"xmldump"};

		vFixMultipleBodies($r, $doc, "html");
		vFixMultipleBodies($r, $doc, "body");
		vFixBodyAttributes($doc);

		return if fCheckBlockWords($r, $doc, $roHTTPResp);

		vMungeXMLDoc($r, $doc) unless $hArgs{"nomunge"};

		# We'll always deliver in UTF-8, so we must remove any
		# "meta content-type" node which might contradict us.
		iRemoveNodes($doc, '//meta[translate(@http-equiv, "CONTENTTYPE", "contenttype") = "content-type"]');

		$$c = $doc->toStringHTML;
		$r->log->info("Setting response content:\n$$c") if DEBUG "content";

		$roHTTPResp->header("content-length", length($$c));
		$roHTTPResp->header("content-type", "text/html; charset=utf-8");
	} or warn "eval error: [$@]\n";
}

# When parsing bad HTML, the <body> attributes often end up *within*
# the body element instead: for example,
# 	<body><p> bgcolor="#ffffff" &gt;</p></body>
# Look for those mis-placed attributes, remove them, and re-apply them
# to the body.

sub vFixBodyAttributes
{
	my $doc = shift;

	my @m = $doc->findnodes(<<'EOF');
//body/p
	[
		contains(translate(string(.), 'BGCOLOR', 'bgcolor'), 'bgcolor=')
			or
		contains(translate(string(.), 'TEXT', 'text'), 'text=')
			or
		contains(translate(string(.), 'ONLOAD', 'onload'), 'onload=')
	]
EOF

	return unless @m==1;
	my $p = $m[0];

	$p->parentNode->removeChild($p);
	my $text = $p->firstChild->nodeValue;

	my $parser = XML::LibXML->new;
	my $body = $parser->parse_html_string("<body $text</body>");

	(my $b) = $body->findnodes("//body")
		or return;
	(my $realb) = $doc->findnodes("//body")
		or return;

	for ($b->attributes)
	{
		$realb->setAttribute($_->nodeName, $_->nodeValue);
	}
}

sub vFixMultipleBodies
{
	my ($r, $doc, $tag) = @_;

	my @bodies = $doc->findnodes("//$tag");
	return unless @bodies > 1;

	@bodies = map {
		[ $_, length($_->toString) ]
	} @bodies;

	@bodies = sort { $a->[1] <=> $b->[1] } @bodies;
	
	# Keep the largest body
	pop @bodies;

	# Remove the rest
	my $roURI = Apache->request->pnotes('XMLProxy_URIObj');
	my $sURL = $roURI->as_string;

	for (map { $_->[0] } @bodies)
	{
		$r->log->info("Removing duplicate <$tag> from $sURL:\n", $_->toString)
			if DEBUG "munge";
		$_->parentNode->removeChild($_);
	}
}

# iRemoveNodes($doc, $xpath, [$newnodemaker])
# Remove all nodes from $doc which match $xpath

sub iRemoveNodes
{
	my ($doc, $sXPath, $newnodemaker) = @_;
	my @m = $doc->findnodes($sXPath);
	my $r = Apache->request;

	unless (@m)
	{
		my $l = (caller)[2];
		$r->log->debug("iRemoveNodes called from line $l didn't match anything")
			if DEBUG "mungefail";
		return 0;
	}

	for (@m)
	{
		my $roURI = Apache->request->pnotes('XMLProxy_URIObj');
		my $sURL = $roURI->as_string;
		$r->log->info("Removing node from $sURL:\n", $_->toString)
			if DEBUG "munge";
		$_->parentNode->removeChild($_) unless $newnodemaker;
		$_->parentNode->replaceChild(&$newnodemaker, $_) if $newnodemaker;
	}

	@m;
}

sub vMungeXMLDoc
{
	my ($r, $doc) = @_;

	$r->log->info("Munge start") if DEBUG "mungefail";

	my $sServer = $r->pnotes("XMLProxy_Server");

	for (@aRemoveRules)
	{
		my ($sName, $sXPath) = @$_;
		my $s = sub { $doc->createComment(" Rule='$sName' ") };
		my $iCount = iRemoveNodes($doc, $sXPath, $s)
			or next;

		$r->log->info("Server=$sServer Count=$iCount Rule=$sName");

		$doc->getDocumentElement->appendChild($doc->createComment(" Rule='$sName' Count=$iCount "));
		$doc->getDocumentElement->appendChild($doc->createTextNode("\n"));
	}

	for (@arcOtherMungeRules)
	{
		$_->($r, $doc);
	}

	# Just for a laugh... IMDB auto-headshots

	for ($doc->findnodes(' //a [ contains(@href, "/Name?") ] '))
	{
		last;
		my ($sName) = $_->getAttribute("href") =~ m/\/Name\?(.*)$/
			or next;
		my $img = $doc->createElement("img");
		
		my $sPicURI = "http://i.imdb.com/Photos/CMSIcons/N/000/14/97/Headshot.gif";
		$img->setAttribute("src", $sPicURI);
		
		$_->insertBefore($img, $_->firstChild);
	}

	$r->log->info("Munge complete") if DEBUG "mungefail";
}

sub vRewriteImage
{
	my ($r, $roHTTPResp) = @_;
	my $c = $roHTTPResp->content_ref;

	my $roURI = $r->pnotes('XMLProxy_URIObj');

	eval
	{
		my $i = Image::Magick->new;
		my $e = $i->BlobToImage($$c);

		$r->log->debug("read: e=$e"), return
			if $e;

		$r->log->warn("No images loaded from content"), return
			unless @$i;

		my ($w, $h, $fmt) = $i->[0]->getattributes(qw( width height format ));

		$r->log->debug(sprintf "%s: fmt=%s frames=%d %d x %d",
			$roURI->as_string,
			$fmt,
			scalar(@$i),
			$w, $h,
		) if DEBUG "imagemagick";

		if (@$i > 1 and fIsStandardAdSize($w, $h))
		{
			$r->log->info("Blocking multi-frame ${w}x${h} image");
			$_[1] = new HTTP::Response(
				404,
				"Animated banner blocked",
				undef,
				"Animated banner blocked",
			);
		}

		#$i->Swirl(45);
		#$$c = join "", $i->ImageToBlob;
	};

	$r->log->warn("eval error: $@") if $@;
}

sub fIsStandardAdSize
{
	my ($x, $y) = @_;
	$hStandardAdSizes{ (0+$x) . "x" . (0+$y) };
}

################################################################################
# Debugging
################################################################################

sub vDumpXMLDoc
{
	my ($r, $doc) = @_;
	#DoDump($r, $doc->getDocumentElement, 0);
	$r->pnotes('XMLProxy_Document', $doc);
	#$r->push_handlers(PerlCleanupHandler => \&cleanup);
	vDoDump($r, $doc->getDocumentElement, 0);
}

sub cleanup
{
	my $r = shift;
	my $doc = $r->pnotes('XMLProxy_Document') or return;
	$r->log->debug("Cleanup:");
	vDoDump($r, $doc->getDocumentElement, 0);
}

sub vDoDump
{
	my ($r, $e, $lev) = @_;
	print STDERR " " x $lev;

	my $t = $e->nodeType;
	$t = $hNodeTypes{$t} || $t;
	print STDERR "$t";

	unless ($t eq "XML_CDATA_SECTION_NODE")
	{
		my $n = $e->nodeName;
		print STDERR " N=$n" if defined $n;
	}

	my $v = $e->nodeValue;
	print STDERR " V=\"$v\"" if defined $v;

	for my $a ($e->getAttributes)
	{
		my ($k, $v) = ($a->name, $a->value);
		$v = "undef" unless defined $v;
		print STDERR " \@$k=\"$v\"";
	}

	print STDERR "\n";

	vDoDump($r, $_, $lev+1) for $e->childNodes;
}

sub HTTP::Message::as_string_no_content
{
	my $self = shift;
	my $r = $self->content_ref;
	my $c = $$r; $$r = undef;
	my $s = $self->as_string;
	$$r = $c;
	$s;
}

################################################################################
# Initialisation & Config
################################################################################

sub hGetNodeTypes
{
	return unless $fHaveLibXML;

	map {
		XML::LibXML->can($_)->() => $_
	} qw(
		  XML_ELEMENT_NODE 
		  XML_ATTRIBUTE_NODE
		  XML_TEXT_NODE
		  XML_CDATA_SECTION_NODE
		  XML_ENTITY_REF_NODE
		  XML_ENTITY_NODE
		  XML_PI_NODE
		  XML_COMMENT_NODE
		  XML_DOCUMENT_NODE
		  XML_DOCUMENT_TYPE_NODE
		  XML_DOCUMENT_FRAG_NODE
		  XML_NOTATION_NODE
		  XML_HTML_DOCUMENT_NODE
		  XML_DTD_NODE
	);
}

sub aRemoveRules
{
	use vars '@r';
	local @r = ();
	
	sub AddRule($$)
	{
		push @r, \@_;
	}

	AddRule "Google sponsored links, down the size of the page", <<'EOF';
	//table
		[ count(./tr) = 3 ]
		[ string(./tr[1]) = 'Sponsored Links' ]
EOF

	AddRule "Google sponsored link, across the page, before the search results", <<'EOF';
	//table
		[ count(./tr) = 1 ]
		[
			./tr
				[ count(./td) = 2 ]
				[ string(./td[2]) = 'Sponsored Link' ]
		]
EOF

	AddRule "Salon ads", <<'EOF';
	//table
		[ count(./tr) = 1 ]
		[
			./tr
				[ count(./td) = 1 ]
				[
					./td/a [ contains(@href, '/ads/') ] /img
				]
		]
EOF

	AddRule "Salon ads", <<'EOF';
	//table
		[ @width = 736 ]
		[ count(./tr) = 1 ]
		[
			./tr
				[
					./td/iframe
				]
		]
EOF

	AddRule "ZDnet in-article advert", <<'EOF';
	//table
		[ count(./tr) = 2 ]
		[ ./tr[1]/td/img ]
		[
			./tr[2]/td
				[ ./a/img ]
				[ ./img [ @width=1 ] [ @height=1 ] ]
		]
EOF

	AddRule "ZDnet in-article advert (2)", <<'EOF';
	//table
		[ following-sibling::comment() [ contains(., 'IMU TABLE END') ] ]
EOF

	AddRule "ZDnet in-article advert (3)", <<'EOF';
	//table
		[ following-sibling::comment() [ contains(., 'SKYSCRAPER Ad TABLE END') ] ]
EOF

	AddRule "468 x 60 banners", <<'EOF';
	//a [ ./img [ @width=468 ] [ @height=60 ] ]
EOF

	AddRule "728 x 90 banners", <<'EOF';
	//a [ ./img [ @width=728 ] [ @height=90 ] ]
EOF

	AddRule "Anything 468 x 60", <<'EOF';
	//* [ @width=468 ] [ @height=60 ]
EOF

	AddRule "Anything 728 x 90", <<'EOF';
	//* [ @width=728 ] [ @height=90 ]
EOF

	AddRule "theregister.co.uk adverts", <<'EOF';
	//tr
		[ count(td) = 1 ]
		[ ./td/table
			[ count(tr) = 2 ]
			[
				./tr[1]/td/img
					[ @alt = "Advertisement" ]
			]
		]
EOF

	AddRule "Slashdot ads", <<'EOF';
	//center
		[ preceding-sibling::comment() [ contains(., 'advertisement') ] ]
		[ following-sibling::comment() [ contains(., 'ad') ] ]
		[ .//script ]
EOF

	AddRule "Slashdot ads (2)", <<'EOF';
	//table
		[ @width = 346 ]
		[ @align = "right" ]
		[ count(tr) = 1 ]
		[ ./tr
			[ count(td) = 1 ]
			[
				./td
				[ ./script [ contains(., 'adlog.pl') ] ]
				[ ./a [ contains(@href, 'adlog.pl') ] ]
			]
		]
EOF

	AddRule "Perl.com (generic O-Reilly?) ads", <<'EOF';
	//table
		[ count(tr) = 1 ]
		[ ./tr
			[ count(td) = 1 ]
			[ ./td
				[./script]
				[
					./noscript/a [ contains(@href, 'ad.doubleclick.net') ]
				]
			]
		]
EOF

	AddRule "Perl.com (generic O-Reilly?) ads (2)", <<'EOF';
	//td
		[ @width=135 ]
		[ ./img
			[ ./preceding-sibling::comment() [ contains(., 'sponsor') ] ]
			[ ./following-sibling::center ]
		]
EOF

	AddRule "Perl.com (generic O-Reilly?) ads (3)", <<'EOF';
	//td
		[ @width=122 ]
		[ ./preceding-sibling::comment() [ contains(., 'right column') ] ]
		[ ./span/center [ contains(string(.), 'Sponsor') ] ]
EOF

	AddRule 'nytimes "skyscraper" ads', <<'EOF' if 0;
	//table
		[ ./preceding-sibling::comment() [ contains(., 'Middle ad type') ] ]
EOF

	AddRule 'nytimes "context" ads', <<'EOF' if 0;
	//table
		[ ./preceding-sibling::comment()[1] [ contains(., 'context ad') ] ]
EOF

	AddRule "scripts which look like they might generate an ad", <<'EOF';
	//script
		[
			contains(., '468')
			and contains(., '60')
			and contains(., 'document.write')
		]
EOF

	AddRule "scripts which look like they might generate an ad (728x90)", <<'EOF';
	//script
		[
			contains(., '728')
			and contains(., '90')
			and contains(., 'document.write')
		]
EOF

	AddRule "ZDnet IFRAME ads", <<'EOF';
	//table
		[ count(./tr) = 1 ]
		[ ./tr
			[ count(td) = 1 ]
			[ ./td
				[./iframe [ contains(@src, '/ads') ] ]
			]
		]
EOF

	AddRule "ZDnet IFRAME ads (2)", <<'EOF';
	//table
		[ count(./tr) = 1 ]
		[ ./tr
			[ count(td) = 7 ]
			[ ./td[3]
				[./iframe [ contains(@src, '/ads') ] ]
			]
		]
EOF

	AddRule "theonion 728x90 ad", <<'EOF';
	//table
		[ ./preceding-sibling::comment()[1] [ contains(., 'Ad Begin') ] ]
EOF

	AddRule "slashdot 728x90 ad", <<'EOF';
	* [ @width="728" and @height="90" ]
EOF

	AddRule "theregister.co.uk ad cell", <<'EOF';
	//* [ @class="advert" ]
EOF

	AddRule "imdb pageflicker", <<'EOF';
	//table
		[ count(./tr) = 3 ]
		[ ./tr[1]
			[ count(td) = 1 ]
			[ ./td[1]
				[ ./img [ @alt = "PageFlicker" ] ]
			]
		]
EOF

	AddRule "imdb advert table", <<'EOF';
	//table
		[ ./tr[1]
			[ count(td) = 1 ]
			[ ./td[1]
				[ ./img [ @alt = "Advert" ] ]
			]
		]
EOF

	AddRule "imdb ads etc.", <<'EOF';
	//table [ @width=165 and @align="right" ]
EOF

	AddRule "imdb celeb photos ad", <<'EOF';
	//table [ contains(./tr[2]/td, "Celeb Photos") ]
EOF

	AddRule "imdb rhs ads", <<'EOF' if 0;
	//table [ count(./tr)=1 ]
		/tr [ count(./td) = 3 ]
			/td[3]/table [ count(./tr)=3 or count(./tr)=2 ]
				/tr[2] [ count(./td)=3 ]
					/td[3]
EOF

	AddRule "yahoo in-story ads", <<'EOF';
	//center/a [ ./img [ @width=300 and @height=250 ] ]
EOF

	AddRule "planet-f1 hotspot block", <<'EOF';
	//div
		[ preceding-sibling::comment() [ contains(., 'start of hotspot') ] ]
		[ following-sibling::comment() [ contains(., 'end of hotspot') ] ]
EOF

	@r;
}

sub arcOtherMungeRules
{
	(
		\&vRemoveAdSizedThings,
		\&vRemoveLinkTargets,
		\&vRemoveBlockedContent,
		\&vAutoSkipTransitionalAd,
		\&vRewriteJavascriptBlocks,
	);
}

sub vRemoveAdSizedThings
{
	my ($r, $doc) = @_;

	my @n;
	push @n, $doc->findnodes(' //img ');
	push @n, $doc->findnodes(' //iframe ');
	push @n, $doc->findnodes(' //embed ');
	push @n, $doc->findnodes(' //object ');

	for (@n)
	{
		my $w = $_->getAttribute("width") or next;
		my $h = $_->getAttribute("height") or next;
		next if $w =~ /\D/;
		next if $h =~ /\D/;

		next unless fIsStandardAdSize($w, $h);

		my $roURI = Apache->request->pnotes('XMLProxy_URIObj');
		my $sURL = $roURI->as_string;
		$r->log->info("Removing ad-sized node from $sURL:\n", $_->toString)
			if DEBUG "munge";
		$_->parentNode->removeChild($_);
	}
}

sub vRemoveLinkTargets
{
	my ($r, $doc) = @_;
	my %a;

	# Remove 'target' from 'a'
	for ($doc->findnodes(' //a/attribute::target '))
	{
		my $t = $_->nodeValue;
		next unless $t eq "new" or $t eq "_blank";
		my $p = $_->parentNode;
		$a{$p+0} ||= [ $p, $p->toString ];
		$p->removeAttribute($_->nodeName);
	}

	for (values %a)
	{
		$r->log->info("munged 'A'\n  from " . $_->[1] . "\n  to   " . $_->[0]->toString)
			if DEBUG "munge";
	}
}

sub vRemoveBlockedContent
{
	my ($r, $doc) = @_;
	# Remove blocked IFRAMEs, IMGs, As etc

	for ($doc->findnodes(' //* [ @src != "" or @href != "" ] '))
	{
		for my $sAttr (qw( src href ))
		{
			my $uri = $_->getAttribute($sAttr);
			next unless defined $uri;
			next unless fBlockURL($r, $uri);

			goto BLOCKED;
		}

		next;

BLOCKED:
		$r->log->info("Element with blocked href/src:\n", $_->toString)
			if DEBUG("munge");
		$_->parentNode->removeChild($_);
	}
}

sub vAutoSkipTransitionalAd
{
	my ($r, $doc) = @_;

	for ($doc->findnodes(' //a [ contains(., "Skip Ad") ] '))
	{
		my $url = $_->getAttribute("href");
		for ($doc->findnodes(' /html/head/meta [ @http-equiv="refresh" ] '))
		{
			my $content = $_->getAttribute("content");
			$r->log->error("Auto-skip ad: link=$url meta=$content");
			return;
		}
	}
}

sub vRewriteJavascriptBlocks
{
	my ($r, $doc) = @_;

SCRIPT:
	for my $script ($doc->findnodes(' //script '))
	{
		for my $text ($script->childNodes)
		{
			my $js = $text->nodeValue;
			next unless defined($js) and $js =~ /\S/;

			# $r->log->info("\n\nSCRIPT:\n$js\n\n");

			next unless fBlockJavascript($r, \$js);

			my $sURI = $r->pnotes('XMLProxy_URIObj');
			$r->log->info("Blocking script in $sURI:\n===\n$js\n===\n") if DEBUG("mungescript");

			$script->parentNode->removeChild($script);
			next SCRIPT;
		}
	}
}

sub araStandardAdSizes
{
	# From www.iab.net: standard advert sizes.
	# IMU == "Interactive Marketing Unit".  Nice.

	# Rectangles and Pop-Ups
	[ 300, 250 ], # IMU (Medium Rectangle)
	#[ 250, 250 ], # IMU (Square Pop-up)
	[ 240, 400 ], # IMU (Vertical Rectangle)
	[ 336, 280 ], # IMU (Large Rectangle) 
	[ 180, 150 ], # IMU (Rectangle) 

	# Banners and Buttons
	[ 468, 60 ], # IMU (Full Banner)
	[ 234, 60 ], # IMU (Half Banner)
	[ 88, 31 ], # IMU (Micro Bar)
	[ 120, 90 ], # IMU (Button 1)
	[ 120, 60 ], # IMU (Button 2)
	[ 120, 240 ], # IMU (Vertical Banner)
	[ 125, 125 ], # IMU (Square Button)

	# Skyscrapers
	[ 160, 600 ], # IMU (Wide Skyscraper)
	[ 120, 600 ], # IMU (Skyscraper)

	# Other sizes:
	[ 728, 90 ], # slashdot, theonion
	[ 768, 90 ], # theonion
}

my @asBlockWords;

push @asBlockWords, qw(
	Ferrari Schumacher Barichello Barrichello
	Williams BMW Montoya
	McLaren Mercedes Raikkonen Coulthard
	Renault Trulli Button
	Jordan Honda Fisichella Sato
	BAR Honda Villeneuve Panis
	Minardi Asiatech Webber Yoong
	Toyota Salo McNish
	Sauber Petronas Heidfeld Massa
	Arrows Frentzen Bernoldi
	Jaguar Irvine Rosa rosa
	Suzuka Japan japan
) if 0;

my $UPPER = join "", "A" .. "Z";
#my $lower = lc $UPPER;
my $lower = $UPPER;

sub fCheckBlockWords
{
	my ($r, $doc, $roHTTPResp) = @_;

	for (@asBlockWords)
	{
		#my $lc = lc $_;
		my $lc = $_;

		my @n = $doc->findnodes(<<EOF);
	//text() [ contains(translate(string(.), '$UPPER', '$lower'), '$lc') ]
EOF

		next unless @n;

		# Return a "blocked" response
		$r->log->info("Found a Block Word!");

		$roHTTPResp->content(<<EOF);
block word detected!
EOF
		$roHTTPResp->header("Content-Type" => "text/plain");
		$roHTTPResp->code("403");

		return 1;
	}

	0;
}

1;
# eof MyModProxyFilter.pm
