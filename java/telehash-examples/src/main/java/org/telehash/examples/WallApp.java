package org.telehash.examples;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.InetSocketAddress;

import org.apache.mina.core.filterchain.DefaultIoFilterChainBuilder;
import org.apache.mina.core.service.IoService;
import org.apache.mina.core.service.IoServiceListener;
import org.apache.mina.core.session.IdleStatus;
import org.apache.mina.core.session.IoSession;
import org.apache.mina.filter.logging.LoggingFilter;
import org.apache.mina.transport.socket.DatagramSessionConfig;
import org.apache.mina.transport.socket.nio.NioDatagramAcceptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.telehash.Hash;
import org.telehash.SwitchHandler;
import org.telehash.TelexHandler;
import org.telehash.model.Line;
import org.telehash.model.TapRule;
import org.telehash.model.TelehashFactory;
import org.telehash.model.Telex;

import com.google.common.base.Objects;

/**
 * Hello world!
 * 
 */
public class WallApp {

	static private Logger logger = LoggerFactory.getLogger(WallApp.class);
	
	static private TelehashFactory tf = TelehashFactory.eINSTANCE;
	
	static private Hash room = Hash.of("42");

	private static SwitchHandler handler;
	
	public static void main(String[] args) throws IOException {
		
		NioDatagramAcceptor acceptor = new NioDatagramAcceptor();
		handler = new SwitchHandler();
		
		TapRule wallRule = tf.createTapRule();
		wallRule.setIs(tf.createTelex().withEnd(room));
		wallRule.getHas().add("+wall");
		handler.addTapRule(wallRule);
		
		handler.addTelexHandler(new TelexHandler() {
			
			@Override
			public void telexReceived(SwitchHandler switchHandler, Line line,
					Telex telex) {
				System.out.println("<" + line.getAddress() + "> " + telex.get("+wall"));
			}
			
			@Override
			public boolean isMatch(Telex telex) {
				return Objects.equal(telex.get("+wall"), room);
			}
			
		});
		
		acceptor.setHandler(handler);

		DefaultIoFilterChainBuilder chain = acceptor.getFilterChain();
		LoggingFilter loggingFilter = new LoggingFilter();
		chain.addLast("logger", loggingFilter);

		DatagramSessionConfig dcfg = acceptor.getSessionConfig();
		dcfg.setReuseAddress(true);

		acceptor.addListener(new IoServiceListener() {
			
			@Override
			public void sessionDestroyed(IoSession session) throws Exception {
			}
			
			@Override
			public void sessionCreated(IoSession session) throws Exception {
			}
			
			@Override
			public void serviceIdle(IoService service, IdleStatus idleStatus)
					throws Exception {
			}
			
			@Override
			public void serviceDeactivated(IoService service) throws Exception {
			}
			
			@Override
			public void serviceActivated(IoService service) throws Exception {
				InetSocketAddress seedAddr = new InetSocketAddress(InetAddress.getByName("telehash.org"), 42424);
//				InetSocketAddress seedAddr = new InetSocketAddress(InetAddress.getByName("localhost"), 40401);
				handler.seed(seedAddr);
			}
		});
		
		InetSocketAddress bindAddress = new InetSocketAddress(0);
		acceptor.bind(bindAddress);

		logger.info("Listening on address: "
				+ acceptor.getLocalAddress().getPort());
		
		BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
		for (String line = br.readLine(); line != null; line = br.readLine()) {
			sendWallMessage(line.trim());
		}
	}

	private static void sendWallMessage(String msg) {
		for (InetSocketAddress addr : handler.getLineAddresses()) {
			Telex telex = (Telex) tf.createTelex().withTo(addr)
				.withEnd(Hash.of(addr))
				.with("+guid", System.currentTimeMillis())
				.with("_hop", 1)
				.with("+wall", msg);
			handler.send(telex);
		}
	}

}
