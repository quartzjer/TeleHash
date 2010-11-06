package org.telehash;

import java.net.InetSocketAddress;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.telehash.model.Line;
import org.telehash.model.TelehashFactory;

import com.google.common.collect.MapMaker;

public class SwitchState {

	static private Logger logger = LoggerFactory.getLogger(SwitchState.class);
	
	static private TelehashFactory tf = TelehashFactory.eINSTANCE;
	
	public enum ConnectionStatus {
		SEEDING,
		CONNECTED,
		OFFLINE
	};
	
	private ConnectionStatus connectionStatus = ConnectionStatus.OFFLINE;

	private InetSocketAddress selfAddress = null;

	private Hash selfHash = null;

	private Map<Hash, Line> lines = new MapMaker().makeMap();

	private InetSocketAddress seedAddress;
	
	public ConnectionStatus getConnectionStatus() {
		return connectionStatus;
	}

	public void setState(ConnectionStatus state) {
		this.connectionStatus = state;
	}

	public InetSocketAddress getSelfAddress() {
		return selfAddress;
	}

	public void setSelfAddress(InetSocketAddress selfAddress) {
		this.selfAddress = selfAddress;
		this.selfHash = Hash.of(selfAddress);
	}

	public Hash getSelfHash() {
		return selfHash;
	}

	public Map<Hash, Line> getLines() {
		return lines;
	}

	public InetSocketAddress getSeedAddress() {
		return seedAddress;
	}

	public void setSeedAddress(InetSocketAddress seedAddress) {
		this.seedAddress = seedAddress;
	}

	public Line getOrCreateLine(InetSocketAddress endpoint) {
		if (endpoint == null) {
			return null;
		}
		
		Hash endHash = Hash.of(endpoint);
		Line line = lines.get(endHash);
		if (line == null || !line.getAddress().equals(endpoint)) {
			line = tf.createLine().withAddress(endpoint).withEnd(endHash);
			line.getNeighbors().add(endHash);
			lines.put(endHash, line);
		}
		return line;
	}
	
	public void removeLine(Hash end) {
		lines.remove(end);
	}

	public void seeding(final Runnable runnable) {
		connectionStatus = ConnectionStatus.SEEDING;
		logger.debug("SEEDING");
		new Thread(){
			public void run() {
				runnable.run();
			};
		}.start();
	}

	public void connected(Runnable runnable) {
		connectionStatus = ConnectionStatus.CONNECTED;
		logger.debug("CONNECTED");
		runnable.run();
	}
	
	public void offline(Runnable runnable) {
		selfAddress = null;
		selfHash = null;
		lines.clear();
		connectionStatus = ConnectionStatus.OFFLINE;
		logger.debug("OFFLINE");
		runnable.run();
	}

}
