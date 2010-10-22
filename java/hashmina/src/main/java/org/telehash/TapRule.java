package org.telehash;

import java.util.List;
import java.util.Map;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class TapRule {

	static public Builder builder() {
		return new Builder();
	}
	
	static public class Builder {
		
		private TapRule rule = new TapRule();
		
		public Builder is(String key, Object value) {
			rule.getIs().put(key, value);
			return this;
		}
		
		public Builder has(String key) {
			rule.getHas().add(key);
			return this;
		}
		
		public TapRule build() { 
			return rule;
		}
		
	}
	
	private Map<String, Object> is = Maps.newHashMap();
	
	private List<String> has = Lists.newArrayList();

	public Map<String, Object> getIs() {
		return is;
	}

	public List<String> getHas() {
		return has;
	}
	
}
