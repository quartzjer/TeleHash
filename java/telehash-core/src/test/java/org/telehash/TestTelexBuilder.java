package org.telehash;

import java.net.InetSocketAddress;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.eclipse.emf.ecore.EDataType;
import org.telehash.model.TelehashFactory;
import org.telehash.model.TelehashPackage;

public class TestTelexBuilder extends TestCase {
	
	static private final TelehashFactory tf = TelehashFactory.eINSTANCE;
	static private final EDataType ENDPOINT = TelehashPackage.Literals.ENDPOINT;
	
	public void testFormatAddress() {
		Assert.assertEquals(
				"208.68.163.247:42424",
				tf.convertToString(ENDPOINT, new InetSocketAddress("telehash.org", 42424)));
	}
	
	public void testParseAddress() {
		InetSocketAddress addr = 
			(InetSocketAddress) tf.createFromString(ENDPOINT, "208.68.163.247:42424");
		Assert.assertEquals("208.68.163.247", addr.getAddress().getHostAddress());
		Assert.assertEquals(42424, addr.getPort());
	}
	
}
