package org.telehash;

import java.net.InetSocketAddress;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.google.common.collect.Lists;

public class Line {

	private InetSocketAddress address;
	
	private Hash end;
	
	private int ringin = -1;
	
	private int ringout = -1;
	
	private long init = 0;
	
	private long seenAt = 0;
	
	private long sentAt = 0;
	
	private int lineAt = 0;
	
	private int br = 0;
	
	private int brOut = 0;
	
	private int brIn = 0;
	
	private int bsent = 0;
	
	private int lineId = 0;
	
	private List<Hash> neighbors;
	
	private boolean visible;
	
	private boolean advertised = false;

	private List<Map<String, ?>> rules = Lists.newArrayList();
	
	public Line(InetSocketAddress address) {
		this.address = address;
		this.end = Hash.of(address);
		this.neighbors = Lists.newArrayList();
		this.ringout = new Random().nextInt(32768);
		this.init = System.currentTimeMillis();
	}

	public long getSeenAt() {
		return seenAt;
	}

	public void setSeenAt(long seenAt) {
		this.seenAt = seenAt;
	}

	public long getSentAt() {
		return sentAt;
	}

	public void setSentAt(long sentAt) {
		this.sentAt = sentAt;
	}

	public int getLineAt() {
		return lineAt;
	}

	public void setLineAt(int lineAt) {
		this.lineAt = lineAt;
	}

	public int getBr() {
		return br;
	}

	public void setBr(int br) {
		this.br = br;
	}

	public int getBrOut() {
		return brOut;
	}

	public void setBrOut(int brOut) {
		this.brOut = brOut;
	}

	public int getBrIn() {
		return brIn;
	}

	public void setBrIn(int brIn) {
		this.brIn = brIn;
	}

	public int getBsent() {
		return bsent;
	}

	public void setBsent(int bsent) {
		this.bsent = bsent;
	}

	public boolean isVisible() {
		return visible;
	}

	public void setVisible(boolean visible) {
		this.visible = visible;
	}

	public InetSocketAddress getAddress() {
		return address;
	}

	public Hash getEnd() {
		return end;
	}

	public int getRingout() {
		return ringout;
	}

	public long getInit() {
		return init;
	}

	public List<Hash> getNeighbors() {
		return neighbors;
	}

	public boolean isAdvertised() {
		return advertised;
	}

	public void setAdvertised(boolean advertised) {
		this.advertised = advertised;
	}

	public List<Map<String, ?>> getRules() {
		return rules;
	}

	public int getLineId() {
		return lineId;
	}

	public void setLineId(int lineId) {
		this.lineId = lineId;
	}

	public int getRingin() {
		return ringin;
	}

	public void setRingin(int ringin) {
		this.ringin = ringin;
	}
	
}
