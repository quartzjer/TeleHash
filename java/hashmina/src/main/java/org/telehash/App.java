package org.telehash;

import java.io.IOException;
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

/**
 * Hello world!
 * 
 */
public class App {

	static private Logger logger = LoggerFactory.getLogger(App.class);

	public static void main(String[] args) throws IOException {
		
		NioDatagramAcceptor acceptor = new NioDatagramAcceptor();
		final SwitchHandler handler = new SwitchHandler();
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
	}

}
