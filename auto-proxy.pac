Status: 200 Here it is
Content-Type: application/x-ns-proxy-autoconfig

// vi: set ts=4 sw=4 ft=javascript :

// Save with a .pac extension
// Serve with a MIME type of application/x-ns-proxy-autoconfig
// See http://wp.netscape.com/eng/mozilla/2.0/relnotes/demo/proxy-live.html

function FindProxyForURL(url, host)
{
	// We can only handle http
	if (url.substring(0, 5) != "http:")
		return "DIRECT";

	// Don't proxy for private IP addresses (e.g. mkbackup)
	if (isInNet(host, "127.0.0.0", "255.0.0.0"))
		return "DIRECT";
	if (isInNet(host, "10.0.0.0", "255.0.0.0"))
		return "DIRECT";
	if (isInNet(host, "172.16.0.0", "255.240.0.0"))
		return "DIRECT";
	if (isInNet(host, "192.168.0.0", "255.255.0.0"))
		return "DIRECT";

	// Also don't proxy anything within Powernet's public IP range
	if (isInNet(host, "195.60.0.0", "255.255.224.0"))
		return "DIRECT";

	// Other exceptions
	if (dnsDomainIs(host, "spamcop.net")) return "DIRECT";
	if (dnsDomainIs(host, "musicbrainz.org")) return "DIRECT";
	if (dnsDomainIs(host, "metabrainz.org")) return "DIRECT";
	//if (dnsDomainIs(host, "eorbit.net")) return "DIRECT";
	//if (dnsDomainIs(host, "mozilla.org")) return "DIRECT";
	//if (dnsDomainIs(host, "w3.org")) return "DIRECT";
	if (dnsDomainIs(host, "power.net.uk")) return "DIRECT";
	if (dnsDomainIs(host, "suprnova.org")) return "DIRECT";

	var iPort = 8888;
	if (isInNet(myIpAddress(), "10.0.0.0", "255.0.0.0"))
		iPort = 8888;

	// And if all good so far...
	return("PROXY 192.168.1.99:" + iPort);
}

// eof
