package org.telehash;

import org.telehash.model.Line;
import org.telehash.model.Telex;

public interface TelexHandler {
	
	void telexReceived(SwitchHandler switchHandler, Line line, Telex telex);

	boolean isMatch(Telex telex);
	
}
