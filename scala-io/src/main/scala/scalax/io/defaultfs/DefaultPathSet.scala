/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.defaultfs

import resource.ManagedResource
import scalax.io.{
  FileSystem, Path, FileOps,Codec,PathMatcher,AbstractPathPathSet
}

import scalax.io.attributes.FileAttribute

import Path.AccessModes._
import java.io.{File => JFile}
private[defaultfs] class DefaultPathSet(
                    parent : DefaultPath, 
                    pathFilter : Path => Boolean,
                    depth:Int) 
                  extends AbstractPathPathSet[DefaultPath](
                    parent, 
                    pathFilter, 
                    depth,
                    _.jfile.listFiles.view.map (parent.fileSystem.apply).toList ) 