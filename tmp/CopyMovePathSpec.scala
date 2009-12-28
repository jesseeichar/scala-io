/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._

import java.io.IOException

class CopyMovePathSpec extends TestCase{

  def createContext = new Context(){
    def generator : Gen[TestData] = defaultGen
    def createData = new TestData("/tmp/file", 2)
  }

  "A Path" can {
    "move files" in { context =>
      move( context.file, context.path, context.file)
    }
    "copy files" in { context =>
      copy( context.file, context.path, context.file)
    }
    "move directories" in { context =>
      move( context.dir, context.path, context.dir)
    }
    "copy directories" in { context =>
      copy( context.dir, context.path, context.dir)
    }
    "move directory trees" in { context =>
      move( context.tree, context.path, context.tree, canReplace=false)
    }
    "copy directory trees" in { context =>
      copy( context.tree, context.path, context.tree, canReplace=false)
    }
  }

  def move(f1 : Path, f2 : Path, exists : Path, canReplace : Boolean = true) = {
    f1 must be ('exists)
    f2 must be ('notExists)
    f1 moveTo f2
    f2 must be ('exists)
    f1 must be ('notExists)

    f2 moveTo f2
    intercept[IOException] {
      f1 moveTo f2
    }
    intercept[IOException] {
      f2 moveTo exists
    }

    def replace = {
      f2.moveTo (exists, replaceExisting=true)
      f2 must be ('notExists)
      exists must be ('exists)
    }
    
    if (canReplace) replace
    else intercept[IOException] {replace}
}

  def copy(f1 :Path, f2: Path, exists: Path, canReplace: Boolean=true) = {
    f1 must be ('exist)
    f2 must be ('notExists)
    f1 copyTo f2
    f2 must be ('exists)
    f1 must be ('exists)

    f2 copyTo f2 // noop
    intercept[IOException] {
      f1 copyTo f2
    }
    intercept[IOException] {
      f2 copyTo exists
    }

    def overwrite = {
      f2.copyTo (exists, replaceExisting=true)
      f2 must be ('exists)
      exists must be ('exists)
    }
    
    if (canReplace) overwrite
    else intercept[IOException] {overwrite}
  }
  
}
