telehash-core
=============
telehash-core is a Telehash switch and application framework for Java.

Features
========
* Async network programming model with Apache MINA. Similar to node.js, with similar benefits.
* Apache commons and Google Guava to enable some FP-style and generally turn down the suck.
* EMF and Jackson for mapping JSON to Java classes while retaining some dynamic capabilities.

TODO
====
* Implement tap rules
* Research DemuxingIoHandler for cleaner separation of SwitchHandler business logic
* Apps!

Ideas
=====
1. OSGi would be a neat platform upon which to build a Telehash application server.
  The switch could be an OSGi service, dynamically loaded bundles could run on this platform.
  As a module system and service registry, it scales up and down quite well.
2. Alternative lightweight JRE. One of the worst aspects of Java is the huge runtime
  your application either has to embed or have installed correctly on the end user's environment.
  What if we customize Apache Harmony (or even one of the *really* micro VMs+++) to create a tiny 
  executable? I want Telehash apps to download, install and run as easy as Dropbox. (But I don't want
  to develop in C++!)
  +++ Might require retrotranslator or sticking to pre-1.5 Java language features.
3. Mobile platforms. Experience in #2 will help here...

Building
========
Get JDK 1.6, Maven2, do 'mvn compile' in this directory. 'mvn test' to run unit tests.
'mvn package' to build a standalone, self-contained jar that (for now) runs a switch. 

Development
===========
Model classes are generated from the ecore metamodels. Edit the metamodels (preferrably with 
EMF Ecore tools in Eclipse IDE), reload the genmodel, then generate the model code.

You only need EMF tooling if you are working directly on the EMF models. The model classes 
are generated from ecore and checked in.
