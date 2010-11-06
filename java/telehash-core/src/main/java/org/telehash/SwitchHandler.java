package org.telehash;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;

import org.apache.commons.lang.StringUtils;
import org.apache.mina.core.buffer.IoBuffer;
import org.apache.mina.core.buffer.SimpleBufferAllocator;
import org.apache.mina.core.future.IoFuture;
import org.apache.mina.core.future.IoFutureListener;
import org.apache.mina.core.service.IoHandlerAdapter;
import org.apache.mina.core.session.IoSession;
import org.apache.mina.transport.socket.nio.NioDatagramAcceptor;
import org.eclipse.emf.json.JsonMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.telehash.SwitchState.ConnectionStatus;
import org.telehash.model.Line;
import org.telehash.model.TapRule;
import org.telehash.model.TelehashFactory;
import org.telehash.model.TelehashPackage;
import org.telehash.model.Telex;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

public class SwitchHandler extends IoHandlerAdapter {

	static private Logger logger = LoggerFactory.getLogger(SwitchHandler.class);

	static private TelehashFactory tf = TelehashFactory.eINSTANCE;
	
	private final NioDatagramAcceptor acceptor;
	private final SimpleBufferAllocator allocator;
	private final SwitchState state = new SwitchState();

	private List<TelexHandler> telexHandlers = Lists.newArrayList();

	private SwitchScanner scannerThread;

	private List<TapRule> tapRules = Lists.newArrayList();

	public SwitchHandler(NioDatagramAcceptor acceptor) {
		this.acceptor = acceptor;
		allocator = new SimpleBufferAllocator();
		
		addTelexHandler(new SeeHandler());
		addTelexHandler(new TapHandler());
		addTelexHandler(new EndSignalHandler());
	}
	
	@Override
	public void sessionCreated(IoSession session) throws Exception {
		super.sessionCreated(session);
		Line line = state.getOrCreateLine((InetSocketAddress)session.getRemoteAddress());
		session.setAttribute("line", line);
		logger.debug("Session[{}] created", session.getId());
	}

	@Override
	public void sessionClosed(IoSession session) throws Exception {
		super.sessionClosed(session);
		logger.debug("Session[{}] closed", session.getId());
	}

	public Line getLine(Hash endHash) {
		return state.getLines().get(endHash);
	}
	
	public Collection<InetSocketAddress> getLineAddresses() {
		return Collections2.transform(state.getLines().values(), 
				new Function<Line, InetSocketAddress>() {
					@Override
					public InetSocketAddress apply(Line from) {
						return from.getAddress();
					}
				});
	}
	
	public InetSocketAddress getAddress() {
		return state.getSelfAddress();
	}

	public Hash getAddressHash() {
		return state.getSelfHash();
	}

	public void addTelexHandler(TelexHandler handler) {
		telexHandlers.add(handler);
	}
	
	public void removeTelexHandler(TelexHandler handler) {
		telexHandlers.remove(handler);
	}
	
	public void seed(final InetSocketAddress seedAddress) {
		state.seeding(new Runnable(){
			@Override
			public void run() {
				state.setSeedAddress(seedAddress);
				Line seedLine = state.getOrCreateLine(seedAddress);
				send(tf.createTelex().withTo(seedLine)
						.withEnd(Hash.of(state.getSeedAddress())));
			}
		});
	}

	private void startScannerThread() {
		if (scannerThread != null && scannerThread.isRunning()) {
			scannerThread.stopRunning();
		}
		
		scannerThread = new SwitchScanner(this);
		scannerThread.start();
	}

	private void stopScannerThread() {
		if (scannerThread != null) {
			scannerThread.stopRunning();
		}
		scannerThread = null;
	}

	public void send(final Telex telex) {
	    final Line line = state.getOrCreateLine(telex.getTo());
	    
	    // check br and drop if too much
	    if (line.getBsent() - line.getBrIn() > 10000) {
	        logger.warn("MAX SEND DROP");
	        return;
	    }
	    
	    // if a line is open use that, else send a ring
	    if (line.isSetLineId()) {
	        telex.setLine(line.getLineId());
	    }
	    else {
	    	telex.unsetLine();
	        telex.setRing(line.getRingOut());
	    }
	    
	    // update our bytes tracking and send current state
	    line.setBrOut(line.getBr());
	    telex.setBytesReceived(line.getBr());
	    
	    try {
	        final String msg = JsonMapper.toJson(telex);
	        final byte[] bytes = msg.getBytes();
	        final IoBuffer buffer = allocator.allocate(bytes.length, true);
	        buffer.put(bytes);
	        buffer.flip();
	        
	        final IoFutureListener<IoFuture> onWriteComplete = new IoFutureListener<IoFuture>() {
	        	public void operationComplete(IoFuture future) {
	            	line.setBsent(line.getBsent() + bytes.length);
	            	line.setSentAt(time());
	        	};
			};
			
	        if (line != null && line.getSession() != null && line.getSession().getAttribute("line") == line) {
	            logger.debug("SEND[{}]: {} bytes: {}", new Object[]{
	            		line.getSession().getId(), bytes.length, msg});
	        	line.getSession().write(buffer).addListener(onWriteComplete);
	        }
	        else {
		        IoSession session = acceptor.newSession(telex.getTo(), acceptor.getLocalAddress());
                line.setSession(session);
                session.setAttribute("line", line);
                logger.debug("SEND[{}]: {} bytes: {}", new Object[]{
                		session.getId(), bytes.length, msg});
                session.write(buffer).addListener(onWriteComplete);
	        }
	    }
	    catch (IOException e) {
	    	logger.error("Failed to write Telex to JSON: " + e.getMessage(), e);
	    }
	}

    static private final CharsetDecoder decoder = Charset.forName("UTF-8").newDecoder();
    
    @Override
	public void messageReceived(IoSession session, Object message)
			throws Exception {
		if (message instanceof IoBuffer) {
			IoBuffer buffer = (IoBuffer) message;
			String response = buffer.getString(decoder);
			int br = response.getBytes().length;
			logger.debug("RECV[{}] from {}: {} bytes: {}", new Object[]{
					session.getId(),
					(InetSocketAddress)session.getRemoteAddress(), br, response});
			Telex telex = (Telex) 
					JsonMapper.fromJson(response, TelehashPackage.Literals.TELEX);
			switch (state.getConnectionStatus()) {
			case SEEDING:
				completeBootstrap(session, telex, br);
				break;
			case CONNECTED:
				processTelex(session, telex, br);
				break;
			}
		}
	}

	protected void processTelex(IoSession session, final Telex telex, int br) {
		Line line = (Line) session.getAttribute("line");
		if (line == null) {
			session.close(true);
			return;
		}
		
		boolean lineStatus = checkLine(line, telex, br);
		if (lineStatus) {
			logger.debug("LINE [{}] STATUS {}", line.getAddress(), 
					telex.isSetLine() ? "OPEN" : "RINGING");
		}
		else {
			logger.debug("LINE [{}] FAIL", line.getAddress());
			return;
		}
		
		for (TelexHandler handler : telexHandlers) {
			if (handler.isMatch(telex)) {
				handler.telexReceived(this, line, telex);
			}
		}
		
        Integer hop = (Integer) telex.get("_hop");
        if (hop == null) {
        	hop = 0;
        }
        
        // if not last-hop, check for any active taps (todo: optimize the matching, this is just brute force)
        if (hop < 4) {
        	for (Line matchedLine : Iterables.filter(state.getLines().values(), new Predicate<Line>() {
        		@Override
        		public boolean apply(Line input) {
        			return !input.getRules().isEmpty() && input.isRulesMatch(telex);
        		}
			})) {
        		
                // it's us, it has to be our tap_js        
                if (matchedLine.getAddress().equals(state.getSelfAddress())) {
                    try {
						logger.debug("STDOUT[{}]", JsonMapper.toJson(telex));
					} catch (IOException e) {
						logger.error(e.getMessage(), e);
					}
                }
                else{
                    Telex telexOut = tf.createTelex();
                    for (String fieldName : telex.getFieldNames()) {
                    	telexOut.with(fieldName, telex.get(fieldName));
                    }
                    telexOut.with("_hop", hop + 1);
                    send(telexOut);
                }
        	}
        }
	}
	
	protected void completeBootstrap(final IoSession session, final Telex telex, final int br) {
		state.connected(new Runnable(){
			@Override
			public void run() {
				state.setSelfAddress(telex.getTo());
				logger.debug("SELF[{} = {}]", state.getSelfAddress(), state.getSelfHash());
				
				Line line = state.getOrCreateLine(state.getSelfAddress());
				if (line == null) {
					return;
				}
				
				line.setVisible(true);
				line.getRules().addAll(tapRules);
				
				if (state.getSelfAddress().equals(session.getRemoteAddress())) {
					logger.debug("We're the seed.");
				}
				
				processTelex(session, telex, br);
				
				// start scanning thread
				startScannerThread();
			}
		});
	}

	static public int time() {
		return (int)(System.currentTimeMillis() / 1000);
	}
	
	/**
	 * Check a line's status.
	 * True if open, false if ringing.
	 */
	public boolean checkLine(Line line, Telex telex, int br) {
	    if (line == null) {
	        return false;
	    }
	    
//	    Integer _line = (Integer) telex.get("_line");
//	    if (_line == null) {
//	    	_line = Line.NOT_SET;
//	    }
	    
	    // first, if it's been more than 10 seconds after a line opened, 
	    // be super strict, no more ringing allowed, _line absolutely required
	    if (line.getLineAt() > 0 && time() - line.getLineAt() > 10) {
	        if (!telex.isSetLine() || telex.getLine() != line.getLineId()) {
	            return false;
	        }
	    }
	    
	    // second, process incoming _line
	    if (telex.isSetLine()) {
	        if (line.getRingOut() <= 0) {
	            return false;
	        }
	        
	        // must match if exist
	        if (line.isSetLineId() && telex.getLine() != line.getLineId()) {
	            return false;
	        }
	        
	        // must be a product of our sent ring!!
	        if (telex.getLine() % line.getRingOut() != 0) {
	            return false;
	        }
	        
	        // we can set up the line now if needed
	        if (line.getLineAt() == 0) {
	            line.setRingIn(telex.getLine() / line.getRingOut()); // will be valid if the % = 0 above
	            line.setLineId(telex.getLine());
	            line.setLineAt(time());
	        }
	    }
	    
//	    Integer _ring = (Integer) telex.get("_ring");
//	    if (_ring == null) {
//	    	_ring = Line.NOT_SET;
//	    }
	    
	    // last, process any incoming _ring's (remember, could be out of order, after a _line)
	    if (telex.isSetRing()) {
	    	
	        // already had a ring and this one doesn't match, should be rare
	        if (line.isSetRingIn() && telex.getRing() != line.getRingIn()) {
	        	logger.debug("unmatched _ring from " + line.getAddress());
	            return false;
	        }
	        
	        // make sure within valid range
	        if (telex.getRing() <= 0 || telex.getRing() > 32768) {
	            return false;
	        }
	        
	        // we can set up the line now if needed
	        if (line.getLineAt() == 0) {
	            line.setRingIn(telex.getRing());
	            line.setLineId(line.getRingIn() * line.getRingOut());
	            line.setLineAt(time());
	        }
	    }
	    
	    // we're valid at this point, line or otherwise, track bytes
	    logger.debug(
	        "BR " + line.getAddress() + " [" + line.getBr() + " += " 
	        	+ br + "] DIFF " + (line.getBsent() - telex.getBytesReceived()));
	    line.setBr(line.getBr() + br);
	    line.setBrIn(telex.getBytesReceived());
	    
	    // they can't send us that much more than what we've told them to, bad!
	    if (line.getBr() - line.getBrOut() > 12000) {
	        return false;
	    }
	    
	    // XXX if this is the first seenat,
	    // if we were dialing we might need to re-send our telex as this could be a nat open pingback
	    line.setSeenAt(time());
	    return true;
	}

	/**
	 * Update status of all lines, removing stale ones.
	 */
	public void scanLines() {
	    long now = time();
	    int numValid = 0;
	    
    	logger.debug("SCAN " + state.getLines().size() + " lines");
    	
	    for (Iterator<Entry<Hash, Line>> entryIter = state.getLines().entrySet().iterator(); entryIter.hasNext(); ) {
	    	Entry<Hash, Line> entry = entryIter.next();
	    	Hash hash = entry.getKey();
	        if (hash.equals(state.getSelfHash())) {
	            continue; // skip our own endpoint and what is this (continue)
	        }
	        
	        Line line = entry.getValue();
	        if (!line.getEnd().equals(hash)) {
	        	continue; // empty/dead line (continue)
	        }
	        
	        if ((line.getSeenAt() == 0 && now - line.getInit() > 70)
	                || (line.getSeenAt() != 0 && now - line.getSeenAt() > 70)) {
	            // remove line if they never responded or haven't in a while
	            logger.debug("PURGE[" + hash + " " + line.getAddress() + "] last seen "
	            		+ Long.toString(now - line.getSeenAt()) + "s ago");
	            entryIter.remove();
	            continue;
	        }
	        
	        numValid++;
	        
	        if (state.getConnectionStatus() == ConnectionStatus.CONNECTED) {
	        
	            // +end ourselves to see if they know anyone closer as a ping
	        	Telex telexOut = tf.createTelex().withTo(line).withEnd(state.getSelfHash());
	            
	            // also .see ourselves if we haven't yet, default for now is to participate in the DHT
	            if (!line.isAdvertised()) {
	            	line.setAdvertised(true);
	            	telexOut.getSee().add(state.getSelfAddress());
	            }
	            
	            // also .tap our hash for +pop requests for NATs
	            TapRule tapRule = tf.createTapRule();
	            tapRule.setIs(tf.createTelex().withEnd(state.getSelfHash()));
	            tapRule.getHas().add("+pop");
	            telexOut.getTap().add(tapRule);
	            send(telexOut);
	        }
	    }
	    
	    if (state.getConnectionStatus() == ConnectionStatus.CONNECTED && numValid == 0 
	    		&& !state.getSelfAddress().equals(state.getSeedAddress())) {
	        state.offline(new Runnable(){
	        	@Override
	        	public void run() {
	        		stopScannerThread();
			        seed(state.getSeedAddress());
	        	}
	        });
	    }
	}
	
	public Collection<Hash> nearTo(final Hash endHash, InetSocketAddress address) {
		Hash addrHash = Hash.of(address);
	    Line addrLine = getLine(addrHash);
	    if (addrLine == null) {
	        return Collections.emptyList(); // should always exist except in startup or offline, etc
	    }
	    
	    // of the existing and visible cached neighbors, sort by distance to this end
	    List<Hash> visibleNeighbors = Lists.newArrayList(
	    	Iterables.filter(addrLine.getNeighbors(),
	    		new Predicate<Hash>() {
		    		@Override
		    		public boolean apply(Hash hash) {
		    			Line line = getLine(hash);
		    			return line != null && line.isVisible();
		    		}
				}));
	    Collections.sort(visibleNeighbors, new Comparator<Hash>(){
	    	@Override
	    	public int compare(Hash o1, Hash o2) {
	    		return endHash.diffBit(o1) - endHash.diffBit(o2);
	    	}
	    });
	    
//	    console.log("near_to: see[]=" + JSON.stringify(see));
//	    console.log("near_to: line=" + JSON.stringify(line));
	    
	    if (visibleNeighbors.isEmpty()) {
	        return Collections.emptyList();
	    }
	    
	    Hash firstSeeHash = visibleNeighbors.get(0);
	    
	    logger.debug(StringUtils.join(
	    		new String[]{
	    			"NEARTO " + endHash,
	    			address.toString(),
	    			endHash.toString(),
	    			Integer.toString(addrLine.getNeighbors().size()),
	    			">",
	    			Integer.toString(visibleNeighbors.size()),
	    			";",
	    			firstSeeHash.toString(),
	    			"=",
	    			Integer.toString(addrLine.getEnd().diffBit(endHash))
	    		}, " "));
	    
	    // it's either us or we're the same distance away so return these results
	    if (firstSeeHash.equals(addrLine.getEnd())
	            || (firstSeeHash.diffBit(endHash) == addrLine.getEnd().diffBit(endHash))) {
	        
	        // this +end == this line then replace the neighbors cache with this result 
	        // and each in the result walk and insert self into their neighbors
	        if (addrLine.getEnd().equals(endHash)) {
	        	logger.debug("NEIGH for " + endHash + " was " 
	        			+ StringUtils.join(addrLine.getNeighbors(), ",") 
	        			+ " " + visibleNeighbors.size());
	        	
	        	addrLine.getNeighbors().clear();
	        	Iterables.addAll(addrLine.getNeighbors(),
	        			Iterables.limit(visibleNeighbors, 5));
	            
	        	logger.debug("NEIGH for " + endHash + " is now " 
	        			+ StringUtils.join(addrLine.getNeighbors(), ",") 
	        			+ " " + visibleNeighbors.size());
	        	
	        	for (Hash neighborHash : addrLine.getNeighbors()) {
	        		Line neighborLine = getLine(neighborHash);
	        		if (neighborLine == null || neighborLine == addrLine
	        				|| neighborLine.getNeighbors().contains(endHash)) {
	        			continue;
	        		}
	        		
        			neighborLine.getNeighbors().add(endHash);
                    logger.debug("SEED " + address + " into " + neighborLine.getAddress());
	        	}
	        	
	        }
	        
	        logger.debug("SEE distance=" + endHash.diffBit(firstSeeHash) 
	        		+ " count=" + visibleNeighbors.size());
	        return visibleNeighbors;
	    }

	    // whomever is closer, if any, tail recurse endseeswitch them
	    return nearTo(endHash, getLine(firstSeeHash).getAddress());
	}

	public void taptap() throws IOException {
		for (TapRule tapRule : tapRules) {
			if (state.getConnectionStatus() != ConnectionStatus.CONNECTED) {
				break;
			}
			
			Hash tapEnd = (Hash) tapRule.getIs().get("+end");
	        if (tapEnd == null) {
	        	continue;
	        }
	        
	        Collection<Hash> nearestHashes = nearTo(tapEnd, state.getSelfAddress());
	        for (Hash hash : Iterables.limit(nearestHashes, 3)) {
	            Line line = getLine(hash);
	            
	            if (line.isSetTapLastAt() && line.getTapLastAt() + 50 > time()) {
	                return; // only tap every 50sec
	            }
	            
	            line.setTapLastAt(time());
	            Telex telexOut = (Telex) tf.createTelex().withTo(line)
	            	.with(".tap", Lists.newArrayList(tapRule)); // tap the closest ipp to our target end 
	            logger.info("TAPTAP to {} end {} tap {}", new Object[]{
	            		line.getAddress(), tapEnd, JsonMapper.toJson(telexOut)});
	            send(telexOut);
	        }
	    }
	}
	
	public void addTapRule(TapRule rule) {
		tapRules.add(rule);
	}

	public void setAddress(InetSocketAddress selfAddress) {
		state.setSelfAddress(selfAddress);
	}
	
}
