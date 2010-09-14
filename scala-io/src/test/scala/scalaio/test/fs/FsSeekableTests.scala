/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import scalax.io._
import scalax.io.resource._

import Path.AccessModes._
import OpenOption._

import org.junit.Assert._
import org.junit.{
  Test, Ignore
}
import scalaio.test.AbstractSeekableTests


import java.io.{
    IOException, DataInputStream, DataOutputStream
}

abstract class FsSeekableTests extends AbstractSeekableTests with Fixture {
    def open(data : Option[String] = None) : Seekable = data match {
      case None => 
        fixture.text("\n")
      case Some(text) => 
        val path = fixture.path
        path writeString text
        path
    }
}