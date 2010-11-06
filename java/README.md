Telehash Java Implementation
============================
* Async networking with Apache MINA.
* Apache commons and Google Guava to enable some FP-style and generally turn down the suck.
* EMF and Jackson for static-where-you-want-it, dynamic-where-you-don't JSON mapping.

TODO
====
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
'mvn package' to build a standalone, self-contained jar. 

On OSX, you might need to configure your Java installation to work with Maven's compiler plugin.
See [http://nelz.net/2009/05/13/jdk-1-6-on-os-x/].

Running
=======
Requires JRE 1.6. Tested with Oracle JDK 1.6 and Harmony 6.0-jre-991881.

The example apps in the standalone jar support some command-line options:

    usage: class org.telehash.examples.SwitchApp
     -help                 Display this usage info.
     -port <arg>           Listen port. Default: random open port
     -seed <arg>           Seed, <hostname:port>. Default: telehash.org:42424
     -v,--loglevel <arg>   log4j log level. Default: INFO

The wall app (org.telehash.examples.WallApp) also supports -wall <arg>, useful for testing your own wall.

Development
===========
If you're using Eclipse, do 'mvn eclipse:eclipse' at the top level to create Eclipse project metadata, 
then you can import these projects into your workspace.

Model classes (in org.telehash.model.*) are generated from the ecore metamodels. Edit the metamodels (preferrably with 
EMF Ecore tools in Eclipse IDE), reload the genmodel, then generate the model code.

You only need EMF tooling if you are working directly on the EMF models. The model classes 
are generated from ecore and checked in.
