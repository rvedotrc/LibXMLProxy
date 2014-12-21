#!/usr/bin/perl -w
# vi: set ts=4 sw=4 :

use strict;

package RewritingProxy;

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

# eof RewritingProxy.pm
