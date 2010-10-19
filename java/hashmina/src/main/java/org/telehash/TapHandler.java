package org.telehash;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class TapHandler implements TelexHandler {

	@Override
	public Set<String> getMatchingKeys() {
		// TODO Auto-generated method stub
		return null;
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
