package org.telehash;

import static org.telehash.TelexBuilder.formatAddress;
import static org.telehash.TelexBuilder.parseAddress;

import java.net.InetSocketAddress;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Function;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

public class EndSignalHandler implements TelexHandler {

	static private Logger logger = LoggerFactory.getLogger(SeeHandler.class);
	
	static private Set<String> MATCHING_KEYS = ImmutableSet.of("+end");
	
	@Override
	public Set<String> getMatchingKeys() {
		return MATCHING_KEYS;
	}

	@Override
	public void telexReceived(final SwitchHandler switchHandler, Line line,
			Map<String, ?> telex) {
	    int hop = telex.containsKey("_hop") ? (Integer)telex.get("_hop") : 0;
	    Hash endHash = new Hash((String)telex.get("+end"));
	    
	    if (hop == 0) {
	    	// start from a visible switch (should use cached result someday)
	    	InetSocketAddress visibleAddr = 
	        	line.isVisible() ? line.getAddress() : switchHandler.getAddress();
	        
	        Collection<Hash> closestNeighbors = switchHandler.nearTo(endHash, visibleAddr);
	        
//	      console.log("+end hashes: " + JSON.stringify(hashes));
	        
	        // convert back to address strings
	        List<String> closestAddrs = Lists.newArrayList();
	        Iterables.addAll(closestAddrs,
	        	Iterables.limit(
		        	Iterables.filter(
			        	Iterables.transform(closestNeighbors, new Function<Hash, String>() {
			        		@Override
			        		public String apply(Hash hash) {
			        			return formatAddress(switchHandler.getLine(hash).getAddress());
			        		}
						}),
						String.class),
					5));
	        
//	      console.log("+end ipps: " + JSON.stringify(ipps));
	        
	        // TODO: this is where dampening should happen to not advertise switches that might be too busy
	        if (!line.isAdvertised()) {
	            closestAddrs.add(formatAddress(switchHandler.getAddress()));  // mark ourselves visible at least once
	            line.setAdvertised(true);
	        }
	        
	        if (!closestAddrs.isEmpty()) {
	        	Map<String, ?> telexOut =
		        	TelexBuilder.to(line)
		        		.with(".see", closestAddrs).build();
	        	
	        	switchHandler.send(telexOut);
	        }
	    }
	    
	    // this is our .tap, requests to +pop for NATs
	    if (endHash.equals(switchHandler.getAddressHash()) && telex.containsKey("+pop")) {
	    	String pop = (String) telex.get("+pop");
	        logger.info("POP? " + pop);
	        
            // should we verify that this came from a switch we actually have a tap on?
	        if (pop.startsWith("th:")) {
	        	InetSocketAddress popToAddr = parseAddress(pop.substring(3));
	        	logger.info("POP to " + popToAddr);
	        	switchHandler.send(TelexBuilder.to(popToAddr).build());
	        }
	    }
	}

}
