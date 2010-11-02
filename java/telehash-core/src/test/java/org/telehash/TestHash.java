package org.telehash;

import junit.framework.Assert;
import junit.framework.TestCase;

public class TestHash extends TestCase {

	public void testExpectedHash() throws Exception {
		Hash abcHash = Hash.of("abc");
		Assert.assertEquals("a9993e364706816aba3e25717850c26c9cd0d89d",
				abcHash.toString());
	}
	
	public void testDiff0() throws Exception {
		Hash hash1 = new Hash("a9993e364706816aba3e25717850c26c9cd0d890");
		Hash hash2 = new Hash("a9993e364706816aba3e25717850c26c9cd0d891");
		Assert.assertEquals(0, hash1.diffBit(hash2));
		Assert.assertEquals(0, hash2.diffBit(hash1));
	}
	
	public void testDiff7() throws Exception {
		Hash hash1 = new Hash("a9993e364706816aba3e25717850c26c9cd0d88d");
		Hash hash2 = new Hash("a9993e364706816aba3e25717850c26c9cd0d80d");
		Assert.assertEquals(7, hash1.diffBit(hash2));
		Assert.assertEquals(7, hash2.diffBit(hash1));
	}
	
	public void testDiff159() throws Exception {
		Hash hash1 = new Hash("a9993e364706816aba3e25717850c26c9cd0d89d");
		Hash hash2 = new Hash("29993e364706816aba3e25717850c26c9cd0d89d");
		Assert.assertEquals(159, hash1.diffBit(hash2));
		Assert.assertEquals(159, hash2.diffBit(hash1));
	}
	
}
