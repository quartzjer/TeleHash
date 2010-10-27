package org.eclipse.emf.json;

import junit.framework.TestCase;

import org.telehash.model.TelehashPackage;
import org.telehash.model.Telex;

/**
 * Unit tests for JsonUtil.
 */
public class TestJsonUtil extends TestCase {
	
	/**
	 * Rigourous Test :-)
	 */
	public void testSimpleTelex() {
		Telex telex = (Telex)
			JsonUtil.fromJson("{\"_to\": \"147.26.10.11:40401\", \"+end\": \"a9993e364706816aba3e25717850c26c9cd0d89d\"}",
				TelehashPackage.Literals.TELEX);
		assertEquals("147.26.10.11", telex.getTo().getAddress().getHostAddress());
		assertEquals(40401, telex.getTo().getPort());
		assertEquals("a9993e364706816aba3e25717850c26c9cd0d89d", telex.getEnd().toString());
	}
	
}
