package org.telehash.examples;

import java.io.IOException;
import java.net.InetSocketAddress;

import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.log4j.Level;
import org.apache.mina.core.filterchain.DefaultIoFilterChainBuilder;
import org.apache.mina.core.service.IoService;
import org.apache.mina.core.service.IoServiceListener;
import org.apache.mina.core.session.IdleStatus;
import org.apache.mina.core.session.IoSession;
import org.apache.mina.filter.logging.LogLevel;
import org.apache.mina.filter.logging.LoggingFilter;
import org.apache.mina.transport.socket.DatagramSessionConfig;
import org.apache.mina.transport.socket.nio.NioDatagramAcceptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.telehash.SwitchHandler;
import org.telehash.model.TelehashFactory;
import org.telehash.model.TelehashPackage;

/**
 * Hello world!
 * 
 */
public class SwitchApp {

	static private Logger logger = LoggerFactory.getLogger(SwitchApp.class);

	static private Options options = new Options();
	
	static {
		options.addOption("help", false, "Display this usage info.");
		options.addOption("port", true, "Listen port. Default: random open port");
		options.addOption("seed", true, "Seed, <hostname:port>. Default: telehash.org:42424");
		options.addOption("v", "loglevel", true, "log4j log level. Default: INFO");
	}
	
	public static void main(String[] args) throws IOException, ParseException {
		CommandLineParser cliParser = new BasicParser();
		final CommandLine cli = cliParser.parse(options, args);
		
		if (cli.hasOption("help")) {
			new HelpFormatter().printHelp(SwitchApp.class.toString(), options);
			System.exit(1);
		}
		
		org.apache.log4j.Logger.getRootLogger().setLevel(Level.toLevel(
				cli.getOptionValue("loglevel", "INFO")));
		
		NioDatagramAcceptor acceptor = new NioDatagramAcceptor();
		final SwitchHandler handler = new SwitchHandler(acceptor);
		acceptor.setHandler(handler);

		DefaultIoFilterChainBuilder chain = acceptor.getFilterChain();
		LoggingFilter loggingFilter = new LoggingFilter();
		loggingFilter.setMessageReceivedLogLevel(LogLevel.TRACE);
		loggingFilter.setMessageSentLogLevel(LogLevel.TRACE);
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
				InetSocketAddress seedAddr = 
					(InetSocketAddress) TelehashFactory.eINSTANCE
					.createFromString(TelehashPackage.Literals.ENDPOINT,
						cli.getOptionValue("seed", "telehash.org:42424"));
				handler.seed(seedAddr);
			}
		});
		
		Integer port = Integer.parseInt(cli.getOptionValue("port", "0"));
		InetSocketAddress bindAddress = new InetSocketAddress(port);
		acceptor.bind(bindAddress);

		logger.debug("Listening on address: "
				+ acceptor.getLocalAddress().getPort());
	}

}
