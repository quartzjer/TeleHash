Hashmina
========
Hashmina is a Telehash switch and application framework for Java. It's based on Apache MINA,
an asynchronous network library. Inspired by the node.js port.

Features
========
* Async network programming model with Apache MINA. Similar to node.js, with similar benefits.
* Apache commons and Google Guava to enable some FP-style and generally turn down the suck.

Status (2010/10/25)
=================
* _line/_ring negotiation working
* .see seems to be working

TODO
----
* Non-recursive nearTo. Algorithm is slick, Java is not (no tail recursion).
* Implement tap rules
* Improve JSON integration
* Research DemuxingIoHandler for cleaner separation of SwitchHandler business logic
* Apps!

Ideas
=====
* Jackson supports JSON streaming. There's got to be a better way to make use of it...
* OSGi would be a neat platform upon which to build a Telehash application server.

Building
========
Get JDK 1.6, Maven2, do 'mvn compile' in this directory. 'mvn test' to run unit tests.

