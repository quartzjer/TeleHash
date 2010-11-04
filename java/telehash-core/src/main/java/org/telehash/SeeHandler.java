package org.telehash;

import java.net.InetSocketAddress;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.telehash.model.Line;
import org.telehash.model.TelehashFactory;
import org.telehash.model.TelehashPackage;
import org.telehash.model.Telex;

public class SeeHandler implements TelexHandler {

	static private Logger logger = LoggerFactory.getLogger(SeeHandler.class);
	
	static private TelehashFactory tf = TelehashFactory.eINSTANCE;
	
	@Override
	public boolean isMatch(Telex telex) {
		return telex.isSetSee();
	}
	
	@Override
	public void telexReceived(SwitchHandler switchHandler, Line line, Telex telex) {
		SeeCommandHandler command = new SeeCommandHandler(switchHandler, line, telex);
		command.execute();
	}
	
	private class SeeCommandHandler {
		
		private SwitchHandler switchHandler;
		private Line recvLine;
		private Telex telex;
		
		private SeeCommandHandler(SwitchHandler switchHandler, Line line, Telex telex) {
			this.switchHandler = switchHandler;
			this.recvLine = line;
			this.telex = telex;
		}
		
		public void execute() {
			logger.debug("BEGIN .see HANDLER");
		    for (InetSocketAddress seeAddr : telex.getSee()) {
		    	if (seeAddr.equals(switchHandler.getAddress())) {
		    		continue;
		    	}
		        
		        // they're making themselves visible now, awesome
		        if (seeAddr.equals(recvLine.getAddress()) && !recvLine.isVisible()) {
		            logger.debug("VISIBLE " + recvLine.getAddress());
		            recvLine.setVisible(true);
		            recvLine.getNeighbors().addAll(
		            		switchHandler.nearTo(recvLine.getEnd(), switchHandler.getAddress()));
		            switchHandler.nearTo(recvLine.getEnd(), recvLine.getAddress()); // injects this switch as hints into it's neighbors, fully seeded now
		        }
		        
		        Hash seeHash = Hash.of(seeAddr); 
		        if (switchHandler.getLine(seeHash) != null) {
		        	continue;
		        }
		        
		        // XXX todo: if we're dialing we'd want to reach out to any of these closer to that $tap_end
		        // also check to see if we want them in a bucket
		        if (bucketWant(seeAddr, seeHash)) {
		            
		            // send direct (should open our outgoing to them)
		        	Telex telexOut = tf.createTelex().withTo(seeAddr)
		        		.withEnd(switchHandler.getAddressHash());
		            switchHandler.send(telexOut);
		            
		            // send pop signal back to the switch who .see'd us in case the new one is behind a nat
		            telexOut = (Telex) tf.createTelex().withTo(recvLine)
		            	.withEnd(seeHash)
		            	.with("+pop", "th:" + 
		            			tf.convertToString(TelehashPackage.Literals.ENDPOINT, switchHandler.getAddress()))
		            	.with("_hop", 1);
		            switchHandler.send(telexOut);
		        }
		    }
			logger.debug("END .see HANDLER");
		}

		private boolean bucketWant(InetSocketAddress seeAddr, Hash seeHash) {
			int dist = seeHash.diffBit(switchHandler.getAddressHash());
			logger.debug("BUCKET WANT[{} -> {} -> {}]", new Object[]{
					seeAddr, Integer.toString(dist), switchHandler.getAddress()});
			return dist >= 0;
		}
		
	}
	
}
