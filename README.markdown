The goal of this project is to create an Input/Output and filesystem access API for scala.  

The main inspiration is the Java 7 NIO file API combined with the scala-arm project being worked on by jseureth.

The implementation will work with Java 6+ and will depend on scala-arm but not on the Java 7 NIO file API.

There are two main components to the API.  Core and File.  Core deals with obtaining data from streams, channels, etc...  Basically adapting the IO object of the underlying platform (JVM, CLR) to be more idiomatic Scala.  

For details see [http://jesseeichar.github.com/scala-io-doc/](http://jesseeichar.github.com/scala-io-doc/).