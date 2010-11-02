package org.telehash;

import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SwitchScanner extends Thread {

	static private Logger logger = LoggerFactory.getLogger(SwitchScanner.class);
	
	private static final long SCAN_DELAY = 5000;
	private SwitchHandler switchHandler;
	private boolean isRunning = false;

	public SwitchScanner(SwitchHandler switchHandler) {
		this.switchHandler = switchHandler;
	}
	
	@Override
	public synchronized void start() {
		isRunning = true;
		super.start();
	}

	@Override
	public void run() {
		while (isRunning) {
			switchHandler.scanLines();
			try {
				switchHandler.taptap();
			}
			catch (IOException e) {
				logger.error(e.getMessage(), e);
			}
			
			try {
				sleep(SCAN_DELAY);
			}
			catch (InterruptedException e) {
				
			}
		}
	}

	public void stopRunning() {
		isRunning = false;
		interrupt();
	}

	public boolean isRunning() {
		return isRunning && isAlive();
	}
	
}
