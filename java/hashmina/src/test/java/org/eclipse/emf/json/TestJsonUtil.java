package org.eclipse.emf.json;

import java.io.IOException;
import java.net.Inet4Address;
import java.net.InetSocketAddress;
import java.util.List;
import java.util.Map;

import junit.framework.TestCase;

import org.telehash.Hash;
import org.telehash.model.TelehashFactory;
import org.telehash.model.TelehashPackage;
import org.telehash.model.Telex;

/**
 * Unit tests for JsonUtil.
 */
public class TestJsonUtil extends TestCase {
	
	/**
	 * Rigourous Test :-)
	 * @throws IOException 
	 */
	public void testSimpleTelex() throws IOException {
		Telex telex = (Telex)
			JsonMapper.fromJson("{\"_to\": \"147.26.10.11:40401\", \"+end\": \"a9993e364706816aba3e25717850c26c9cd0d89d\"}",
				TelehashPackage.Literals.TELEX);
		assertEquals("147.26.10.11", telex.getTo().getAddress().getHostAddress());
		assertEquals(40401, telex.getTo().getPort());
		assertEquals("a9993e364706816aba3e25717850c26c9cd0d89d", telex.getEnd().toString());
		assertEquals("a9993e364706816aba3e25717850c26c9cd0d89d", telex.get("end").toString());
	}
	
	/**
	 * Test stuff in the JSON that isn't mapped
	 * @throws IOException 
	 */
	public void testFallThru() throws IOException {
		Telex telex = (Telex)
			JsonMapper.fromJson("{\"foo\": 1, \"bar\": true, \"baz\": {\"a\": [1,2,3]} }",
				TelehashPackage.Literals.TELEX);
		assertEquals(1, ((Integer)telex.get("foo")).intValue());
		assertEquals(true, ((Boolean)telex.get("bar")).booleanValue());
		List<Object> a = (List<Object>) ((Map)telex.get("baz")).get("a");
		for (int i = 0; i < 3; i++) {
			assertEquals(i + 1, ((Integer)a.get(i)).intValue());
		}
	}
	
	public void testWrite() throws Exception {
		Telex telex = TelehashFactory.eINSTANCE.createTelex();
		telex.setTo(new InetSocketAddress(Inet4Address.getLocalHost(), 40104));
		telex.setEnd(Hash.of(telex.getTo()));
		telex.setLine(2323);
		telex.getUnmatched().put("master_shake", "shakezoola");
		telex.getUnmatched().put("frylock", "on_top");
		telex.getUnmatched().put("meatwad", "making_money_see");
		String jsonTxt = JsonMapper.toJson(telex);
		System.out.println(jsonTxt);
	}
	
	public void testWrite2() throws Exception {
		Telex telex = TelehashFactory.eINSTANCE.createTelex();
		telex.setTo(new InetSocketAddress(Inet4Address.getLocalHost(), 40104));
		telex.setEnd(Hash.of(telex.getTo()));
		telex.setLine(2323);
		telex.getSee().add(new InetSocketAddress(Inet4Address.getLocalHost(), 30103));
		telex.getUnmatched().put("master_shake", "shakezoola");
		telex.getUnmatched().put("frylock", "on_top");
		telex.getUnmatched().put("meatwad", "making_money_see");
		String jsonTxt = JsonMapper.toJson(telex);
		System.out.println(jsonTxt);
	}
	
}
