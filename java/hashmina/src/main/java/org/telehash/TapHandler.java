package org.telehash;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.ImmutableSet;

public class TapHandler implements TelexHandler {

	static private Set<String> MATCHING_KEYS = ImmutableSet.of(".tap");
	
	@Override
	public Set<String> getMatchingKeys() {
		return MATCHING_KEYS;
	}

	@Override
	public void telexReceived(SwitchHandler switchHandler, Line line,
			Map<String, ?> telex) {
	    // handle a tap command, add/replace rules
		List<Map<String, ?>> tapRules = (List<Map<String, ?>>) telex.get(".tap");
		if (tapRules != null) {
	        line.getRules().clear();
	        line.getRules().addAll(tapRules);
	    }
	}

}
