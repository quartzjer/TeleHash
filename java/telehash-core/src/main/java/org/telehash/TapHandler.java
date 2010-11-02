package org.telehash;

import org.telehash.model.Line;
import org.telehash.model.Telex;

public class TapHandler implements TelexHandler {

	@Override
	public boolean isMatch(Telex telex) {
		return telex.isSetTap();
	}

	@Override
	public void telexReceived(SwitchHandler switchHandler, Line line, Telex telex) {
	    // handle a tap command, add/replace rules
		line.getRules().clear();
		line.getRules().addAll(telex.getTap());
	}

}
