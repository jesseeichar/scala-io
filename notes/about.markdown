The Scala IO umbrella project consists of a few sub projects for different aspects and extensions of IO. There are two main components of Scala IO:

 * Core - Core primarily deals with Reading and writing data to and from arbitrary sources and sinks. The corner stone traits are Input, Output and Seekable which provide the core API. Other classes of importance are Resource, ReadChars and WriteChars.
 * File - File is a File (called Path) API that is based on a combination of Java 7 NIO filesystem and SBT PathFinder APIs. Path and FileSystem are the main entry points into the Scala IO File API.

The full documentation of Scala IO is available at: [http://jesseeichar.github.com/scala-io-doc/lastest.html](http://jesseeichar.github.com/scala-io-doc/lastest.html)
