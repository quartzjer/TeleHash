package org.telehash;

import java.io.StringWriter;
import java.util.Map;

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;

public class Json {

	public static String toJson(Object o) {
		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(SerializationConfig.Feature.FAIL_ON_EMPTY_BEANS, false);
		StringWriter writer = new StringWriter();
		try {
			mapper.writeValue(writer, o);
			return writer.toString();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	public static String toJson(Map<String, ?> map) {
		ObjectMapper mapper = new ObjectMapper();
		StringWriter writer = new StringWriter();
		try {
			mapper.writeValue(writer, map);
			return writer.toString();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	@SuppressWarnings("unchecked")
	public static Map<String, ?> fromJson(String json) {
		try {
			return new ObjectMapper().readValue(json, Map.class);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}
