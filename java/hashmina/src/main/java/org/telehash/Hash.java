package org.telehash;

import java.io.UnsupportedEncodingException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang.ArrayUtils;

public class Hash implements Comparable<Hash> {

	public static final int HASHES_EQUAL = -1;
	private byte[] bytes;
	
	public Hash(byte[] bytes) {
		this.bytes = bytes;
	}
	
	public Hash(String hashString) {
		try {
			bytes = Hex.decodeHex(hashString.toCharArray());
		}
		catch (DecoderException e) {
			throw new RuntimeException(e);
		}
	}
	
	public static Hash of(InetSocketAddress address) {
		return of(address.getAddress().getHostAddress() + ":" + address.getPort());
	}
	
	public static Hash of(String content) {
		MessageDigest md;
		try {
			md = MessageDigest.getInstance("SHA-1");
			md.update(content.getBytes("UTF-8"), 0, content.length());
			return new Hash(md.digest());
		}
		catch (NoSuchAlgorithmException e) {
			throw new RuntimeException(e);
		}
		catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public int hashCode() {
		IntBuffer intbuf = ByteBuffer.wrap(bytes).asIntBuffer();
		int result = intbuf.get();
		while (intbuf.hasRemaining()) {
			result ^= intbuf.get();
		}
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Hash) {
			return ArrayUtils.isEquals(this.bytes, ((Hash)obj).bytes);
		}
		
		return false;
	}

	@Override
	public String toString() {
		return Hex.encodeHexString(bytes);
	}

	@Override
	public int compareTo(Hash h) {
	    for (int i = 0; i < bytes.length; i++) {
	        int d = bytes[i] - h.bytes[i];
	        if (d != 0) {
	            return d;
	        }
	    }
	    return 0;
	}
	
	/**
	 * Calculate the first different bit position between two hashes.
	 * 
	 * @return 
	 */
	public int diffBit(Hash h) {
	    for (int i = 0; i < bytes.length; i++) {
	    	int diff = bytes[i] ^ h.bytes[i];
	    	for (int bit = 0; diff != 0 && bit < 8; bit++) {
	    		if ((diff & 1<<bit) != 0) {
	    			return ((bytes.length - i - 1) * 8) + bit;
	    		}
	    	}
	    }
	    
	    return HASHES_EQUAL;
	}
	
}
