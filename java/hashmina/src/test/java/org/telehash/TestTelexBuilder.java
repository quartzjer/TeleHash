package org.telehash;

import static org.telehash.TelexBuilder.formatAddress;
import static org.telehash.TelexBuilder.parseAddress;

import java.net.InetSocketAddress;

import junit.framework.Assert;
import junit.framework.TestCase;

public class TestTelexBuilder extends TestCase {
	
	public void testFormatAddress() {
		Assert.assertEquals(
				"208.68.163.247:42424",
				formatAddress(new InetSocketAddress("telehash.org", 42424)));
	}
	
	public void testParseAddress() {
		InetSocketAddress addr = parseAddress("208.68.163.247:42424");
		Assert.assertEquals("208.68.163.247", addr.getAddress().getHostAddress());
		Assert.assertEquals(42424, addr.getPort());
	}
	
}
