package org.telehash;

import static org.telehash.TelexBuilder.formatAddress;
import static org.telehash.TelexBuilder.parseAddress;

import java.net.InetSocketAddress;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ImmutableSet;

public class SeeHandler implements TelexHandler {

	static private Logger logger = LoggerFactory.getLogger(SeeHandler.class);
	
	static private Set<String> MATCHING_KEYS = ImmutableSet.of(".see");
	
	@Override
	public Set<String> getMatchingKeys() {
		return MATCHING_KEYS;
	}

	@Override
	public void telexReceived(SwitchHandler switchHandler, Line line, Map<String, ?> telex) {
		SeeCommandHandler command = new SeeCommandHandler(switchHandler, line, telex);
		command.execute();
	}
	
	private class SeeCommandHandler {
		
		private SwitchHandler switchHandler;
		private Line recvLine;
		private Map<String, ?> telex;
		
		private SeeCommandHandler(SwitchHandler switchHandler, Line line, Map<String, ?> telex) {
			this.switchHandler = switchHandler;
			this.recvLine = line;
			this.telex = telex;
		}
		
		public void execute() {
		    @SuppressWarnings("unchecked")
			List<String> seeAddrStrs = (List<String>) telex.get(".see");
		    if (seeAddrStrs == null) {
		    	logger.warn("SeeHandler was dispatched a Telex without .see command!");
		    	return;
		    }
		    
		    for (String seeAddrStr : seeAddrStrs) {
		    	InetSocketAddress seeAddr = parseAddress(seeAddrStr);
		    	if (seeAddr.equals(switchHandler.getAddress())) {
		    		continue;
		    	}
		        
		        // they're making themselves visible now, awesome
		        if (seeAddr.equals(recvLine.getAddress()) && !recvLine.isVisible()) {
		            logger.info("VISIBLE " + recvLine.getAddress());
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
		        	Map<String, Object> telexOut = TelexBuilder.to(seeAddr)
		        		.with("+end", switchHandler.getAddressHash().toString())
		        		.build();
		        	switchHandler.send(telexOut);
		            
		            // send pop signal back to the switch who .see'd us in case the new one is behind a nat
		            telexOut = TelexBuilder.to(recvLine)
		            	.end(seeHash)
		            	.with("+pop", "th:" + formatAddress(switchHandler.getAddress()))
		            	.with("_hop", 1).build();
		            switchHandler.send(telexOut);
		        }
		    }
		}

		private boolean bucketWant(InetSocketAddress seeAddr, Hash seeHash) {
			int dist = seeHash.diffBit(switchHandler.getAddressHash());
			logger.info("BUCKET WANT[{} -> {} -> {}]", new Object[]{
					seeAddr, Integer.toString(dist), switchHandler.getAddress()});
			return dist >= 0;
		}
		
	}
	
}
