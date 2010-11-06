package org.telehash.examples;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetSocketAddress;
import java.util.Map;
import java.util.concurrent.TimeUnit;

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
import org.eclipse.emf.json.JsonMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.telehash.Hash;
import org.telehash.SwitchHandler;
import org.telehash.TelexHandler;
import org.telehash.model.Line;
import org.telehash.model.TapRule;
import org.telehash.model.TelehashFactory;
import org.telehash.model.TelehashPackage;
import org.telehash.model.Telex;

import com.google.common.base.Objects;
import com.google.common.collect.MapMaker;

/**
 * Hello world!
 * 
 */
public class WallApp {

	static private Logger logger = LoggerFactory.getLogger(WallApp.class);
	
	static private TelehashFactory tf = TelehashFactory.eINSTANCE;
	
	static private Hash room;

	private static SwitchHandler handler;
	
	static private Options options = new Options();
	
	static {
		options.addOption("help", false, "Display this usage info.");
		options.addOption("port", true, "Listen port. Default: random open port");
		options.addOption("seed", true, "Seed, <hostname:port>. Default: telehash.org:42424");
		options.addOption("wall", true, "Wall name. Default: 42");
		options.addOption("v", "loglevel", true, "log4j log level. Default: INFO");
	}
	
	public static void main(String[] args) throws IOException, ParseException {
		CommandLineParser cliParser = new BasicParser();
		final CommandLine cli = cliParser.parse(options, args);
		
		if (cli.hasOption("help")) {
			new HelpFormatter().printHelp(WallApp.class.toString(), options);
			System.exit(1);
		}
		
		org.apache.log4j.Logger.getRootLogger().setLevel(Level.toLevel(
				cli.getOptionValue("loglevel", "INFO")));
		
		room = Hash.of(cli.getOptionValue("wall", "42"));
		
		NioDatagramAcceptor acceptor = new NioDatagramAcceptor();
		handler = new SwitchHandler(acceptor);
		
		TapRule wallRule = tf.createTapRule();
		wallRule.setIs(tf.createTelex().withEnd(room));
		wallRule.getHas().add("+wall");
		handler.addTapRule(wallRule);
		logger.info("Using tap rule {}", JsonMapper.toJson(wallRule));
		
		handler.addTelexHandler(new TelexHandler() {
			
			private Map<Object, Object> msgIds = new MapMaker().expiration(60, TimeUnit.SECONDS).makeMap();
			
			@Override
			public void telexReceived(SwitchHandler switchHandler, Line line,
					Telex telex) {
				logger.info("<{}> {}", line.getAddress(), telex.get("+wall"));
			}
			
			@Override
			public boolean isMatch(Telex telex) {
				Object guidObj = telex.get("+guid");
				if (guidObj == null) {
					return false;
				}
				String guid = guidObj.toString();
				if (guid == null || msgIds.put(guid, guid) != null) {
					return false;
				}
				return Objects.equal(telex.getEnd(), room) && telex.get("+wall") != null;
			}
			
		});
		
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
		
		BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
		for (String line = br.readLine(); line != null; line = br.readLine()) {
			sendWallMessage(line.trim());
		}
	}

	private static void sendWallMessage(String msg) {
		logger.info("Wall: {}", msg);
		for (InetSocketAddress addr : handler.getLineAddresses()) {
			Telex telex = (Telex) tf.createTelex().withTo(addr)
				.withEnd(room)
				.with("+guid", System.currentTimeMillis())
				.with("_hop", 1)
				.with("+wall", msg);
			handler.send(telex);
		}
	}

}
