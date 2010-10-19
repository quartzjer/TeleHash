package org.telehash;

import java.util.Map;
import java.util.Set;

public interface TelexHandler {
	
	Set<String> getMatchingKeys();
	
	void telexReceived(SwitchHandler switchHandler, Line line, Map<String, ?> telex);
	
}
