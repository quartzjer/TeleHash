package org.telehash;

import java.net.InetSocketAddress;
import java.util.Map;

import com.google.common.collect.Maps;

public class TelexBuilder {

	Map<String, Object> map = Maps.newHashMap();
	
	public static TelexBuilder to(String _to) {
		return new TelexBuilder().with("_to", _to);
	}
	
	public static TelexBuilder to(InetSocketAddress _to) {
		return to(formatAddress(_to));
	}
	
	public static TelexBuilder to(Line line) {
		if (line.getLineId() != Line.NOT_SET) {
			return to(line.getAddress()).with("_line", line.getLineId());
		}
		else {
			return to(line.getAddress()).with("_ring", line.getRingout());
		}
	}

	public static String formatAddress(InetSocketAddress addr) {
		return addr.getAddress().getHostAddress() + ":" + addr.getPort();
	}

	public static InetSocketAddress parseAddress(String addr) {
		String[] addrFields = addr.split(":");
		return new InetSocketAddress(addrFields[0], Integer.parseInt(addrFields[1]));
	}

	public TelexBuilder end(Hash endHash) {
		return end(endHash.toString());
	}
	
	public TelexBuilder end(String end) {
		map.put("+end", end);
		return this;
	}
	
	public TelexBuilder with(String k, Object v) {
		map.put(k, v);
		return this;
	}
	
	public Map<String, Object> build() {
		return map;
	}

}
