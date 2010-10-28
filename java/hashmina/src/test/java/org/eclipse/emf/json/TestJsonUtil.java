package org.eclipse.emf.json;

import java.util.List;
import java.util.Map;

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
		assertEquals("a9993e364706816aba3e25717850c26c9cd0d89d", telex.get("end").toString());
	}
	
	/**
	 * Test stuff in the JSON that isn't mapped
	 */
	public void testFallThru() {
		Telex telex = (Telex)
			JsonUtil.fromJson("{\"foo\": 1, \"bar\": true, \"baz\": {\"a\": [1,2,3]} }",
				TelehashPackage.Literals.TELEX);
		assertEquals(1, ((Integer)telex.get("foo")).intValue());
		assertEquals(true, ((Boolean)telex.get("bar")).booleanValue());
		List<Object> a = (List<Object>) ((Map)telex.get("baz")).get("a");
		for (int i = 0; i < 3; i++) {
			assertEquals(i + 1, ((Integer)a.get(i)).intValue());
		}
	}
	
}
