package org.telehash;

import java.net.InetSocketAddress;
import java.util.Collection;
import java.util.List;

import org.eclipse.emf.ecore.EDataType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.telehash.model.Line;
import org.telehash.model.TelehashFactory;
import org.telehash.model.TelehashPackage;
import org.telehash.model.Telex;

import com.google.common.base.Function;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

public class EndSignalHandler implements TelexHandler {

	static private Logger logger = LoggerFactory.getLogger(SeeHandler.class);
	
	static private final TelehashFactory tf = TelehashFactory.eINSTANCE;
	static private final EDataType ENDPOINT = TelehashPackage.Literals.ENDPOINT;
	
	@Override
	public boolean isMatch(Telex telex) {
		return telex.isSetEnd();
	}

	@Override
	public void telexReceived(final SwitchHandler switchHandler, Line line, Telex telex) {
		Integer maybeHop = (Integer) telex.get("_hop");
	    int hop = maybeHop != null ? maybeHop.intValue() : 0;
	    Hash endHash = telex.getEnd();
	    
	    if (hop == 0) {
	    	// start from a visible switch (should use cached result someday)
	    	InetSocketAddress visibleAddr = 
	        	line.isVisible() ? line.getAddress() : switchHandler.getAddress();
	        
	        Collection<Hash> closestNeighbors = switchHandler.nearTo(endHash, visibleAddr);
	        
//	      console.log("+end hashes: " + JSON.stringify(hashes));
	        
	        // convert back to address strings
	        List<InetSocketAddress> closestAddrs = Lists.newArrayList();
	        Iterables.addAll(closestAddrs,
	        	Iterables.limit(
		        	Iterables.filter(
			        	Iterables.transform(closestNeighbors, new Function<Hash, InetSocketAddress>() {
			        		@Override
			        		public InetSocketAddress apply(Hash hash) {
			        			return switchHandler.getLine(hash).getAddress();
			        		}
						}),
						InetSocketAddress.class),
					5));
	        
//	      console.log("+end ipps: " + JSON.stringify(ipps));
	        
	        // TODO: this is where dampening should happen to not advertise switches that might be too busy
	        if (!line.isAdvertised()) {
	            closestAddrs.add(switchHandler.getAddress());  // mark ourselves visible at least once
	            line.setAdvertised(true);
	        }
	        
	        if (!closestAddrs.isEmpty()) {
	        	Telex telexOut = tf.createTelex().withTo(line);
	        	telexOut.getSee().addAll(closestAddrs);
	        	switchHandler.send(telexOut);
	        }
	    }
	    
	    // this is our .tap, requests to +pop for NATs
	    if (endHash.equals(switchHandler.getAddressHash()) && telex.get("+pop") != null) {
	    	String pop = (String) telex.get("+pop");
	        logger.debug("POP? " + pop);
	        
            // should we verify that this came from a switch we actually have a tap on?
	        if (pop.startsWith("th:")) {
	        	InetSocketAddress popToAddr = 
	        		(InetSocketAddress) tf.createFromString(ENDPOINT, pop.substring(3));
	        	logger.debug("POP to " + popToAddr);
	        	switchHandler.send(tf.createTelex().withTo(popToAddr));
	        }
	    }
	}

}
