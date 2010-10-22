package org.telehash;

import java.io.IOException;
import java.net.InetSocketAddress;

import org.apache.mina.core.filterchain.DefaultIoFilterChainBuilder;
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
		SwitchHandler handler = new SwitchHandler();
		acceptor.setHandler(handler);
		
		DefaultIoFilterChainBuilder chain = acceptor.getFilterChain();
		LoggingFilter loggingFilter = new LoggingFilter();
		chain.addLast("logger", loggingFilter);
		
		DatagramSessionConfig dcfg = acceptor.getSessionConfig();
		dcfg.setReuseAddress(true);

		InetSocketAddress bindAddress = new InetSocketAddress(0);
		acceptor.bind(bindAddress);

		logger.info("Listening on address: "
				+ acceptor.getLocalAddress().getPort());
		handler.seed(new InetSocketAddress("telehash.org", 42424));
	}

}
