/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import scalax.io._
import scalax.file._
import scalax.file.ramfs._
import Path.AccessModes._
import PathMatcher._
import scalax.io.Resource
import org.junit.Assert._
import org.junit.Test
import java.io.{OutputStream, IOException}
import java.util.regex.Pattern

abstract class FsBasicPathTests extends scalax.test.sugar.FSAssertionSugar with Fixture {
  implicit val codec = Codec.UTF8

  // test lastmodified

  def fspath(_name:String) = {
    val name = _name.replaceAll(Pattern.quote(fixture.fs.separator),"/")
    val mainParts = (name split "/").toList
    val all = if(name startsWith "/") (fixture.fs.roots.head.path +: mainParts) else mainParts
    fixture.fs.fromSeq(all)
  }
  def fspath(name:Path) = fixture.fs.fromSeq(name.segments)

  @Test //@Ignore
  def recreating_pwd_with_segments_strings_etc : Unit = {
    val absolutePathToPwd = fixture.fs(".").toAbsolute

    assertEquals(absolutePathToPwd, fixture.fs.fromString(absolutePathToPwd.path))
    assertEquals(absolutePathToPwd, fixture.fs.fromSeq(absolutePathToPwd.segments))
    assertEquals(absolutePathToPwd, fixture.fs(absolutePathToPwd.segments:_*))
    assertEquals(absolutePathToPwd, fixture.fs(absolutePathToPwd.path, fixture.fs.separator.charAt(0)))
  }
  @Test //@Ignore
  def path_starting_with_fileSeparator_is_from_root_of_same_root_as_pwd : Unit = {
    val absolutePathToPwd = fixture.fs(".").toAbsolute
    val fs = fixture.fs
    import fs.{separator => sep}

    assertEquals(absolutePathToPwd.root.get / "a", fixture.fs(sep,"a"))
    assertEquals(absolutePathToPwd.root.get / "a", fixture.fs.fromString(sep+"a"))
    assertEquals(absolutePathToPwd.root.get / "a", fixture.fs(sep+"a",fs.separator.charAt(0)))
  }
  @Test //@Ignore
  def delete_a_nonexistent_path_returns_delete_value_of_0 : Unit = {
    val path = fixture.fs.createTempDirectory()
    path.delete(true)
    assertTrue(path.nonExistent)
    val (deleted, remaining) = path.deleteRecursively()
    assertEquals(0,deleted)
    assertEquals(0,remaining)
  }
  @Test //@Ignore
  def can_copy_empty_file : Unit = {
     val tree = fixture.tree(3)._1
     val out = fixture.path(1).createDirectory(true,false)
     tree.**((_:Path).isFile).foreach { p =>
       val to = out / p.relativize(tree)
       p.copyTo(to)
     }
  }

  @Test //@Ignore
  def can_copy_directory_tree_with_copyTo: Unit = {
    val tree = fixture.tree(3)._1
    val out = fixture.path(1)

    tree.copyTo(out)

    val copiedFiles = out.***.toList.map((_:Path).relativize(out))
    val treeFiles = tree.***.map((_:Path).relativize(tree))
    assertTrue(treeFiles.size == copiedFiles.size)
    assertTrue(treeFiles.forall(p => copiedFiles.contains(p)))
  }

  @Test //@Ignore
  def two_paths_are_equal_if_from_same_filesystem : Unit = {
    val path = fixture.path
    val path2 = fixture.fs.fromSeq(path.segments)
    val path3 = fixture.fs.fromString(path.path)

    assertEquals(path, path2)
    assertEquals(path, path3)

    val fs = new RamFileSystem(separator = fixture.fs.separator)
    val rampath = fs.fromSeq(path.segments)
    val equals = rampath == path
    assertFalse(equals)
  }

  @Test //@Ignore
  def parent_of_single_segment_path_is_none {
     assertEquals(None, fixture.fs("a").parent)
  }

  @Test //@Ignore
  def adding_similar_path_from_two_fs_should_have_different_hashcodes {
    val path = List("a","b","c","d","e")
    val fs = new RamFileSystem(separator = fixture.fs.separator)

    val set = Set(fs.fromSeq(path),fixture.fs.fromSeq(path))

    assertEquals(2, set.size)
  }


  @Test //@Ignore
  def root_of_root_is_same_object : Unit = {
    val rpath = fixture.path.root.get
    assertEquals(rpath.root, Some(rpath))
  }
  @Test //@Ignore
  def create_should_fail_if_existing_is_wrong_type : Unit = {
    val path = fixture.path.createFile()
    intercept[IOException] { path.createDirectory(failIfExists = false) }
    path.delete()
    path.createDirectory()
    intercept[IOException] {
      path.createFile(failIfExists = false)
    }
  }
  @Test //@Ignore
  def create_should_be_a_noop_if_exists : Unit = {
    val path = fixture.path.createFile()
    path.createFile(failIfExists = false)
    path.delete()
    path.createDirectory()
    path.createDirectory(failIfExists = false)
  }

  @Test //@Ignore
  def can_create_file_of_one_segment : Unit = {
    val path = fixture.path(1).createFile(accessModes = List(Read,Write))
    assertTrue(path.exists)
  }

  @Test //@Ignore
  def create_should_set_attributes_and_access_if_exists : Unit = {
    val path = fixture.path.createFile(accessModes = List(Read,Write))

    assertEquals(permissions(Read,Write), path.access.toSet)

    path.createFile(failIfExists = false,accessModes = List(Write))
    if(!isWindows) assertFalse(path.canRead)
    assertEquals(permissions(Write), path.access.toSet)
  }

  @Test //@Ignore
  def create_root_dir : Unit = {
    val r = fixture.path.root.get
    intercept[IOException] { r.createDirectory() }
    intercept[IOException] { r.createFile(failIfExists = false) }
    r.createDirectory(failIfExists = false) // should be noop
  }
  @Test //@Ignore
  def name_simpleName_extension = {
    val simpleName = "image"
    val ext = "png"
    val fullname = simpleName + "." + ext
    val path = fspath(fullname)

    assertEquals(simpleName, path.simpleName)
    assertEquals(fullname, path.name)
    assertEquals(Some(ext), path.extension)
    assertEquals(None, fspath(simpleName).extension)
    assertEquals(simpleName, fspath(simpleName).name)
    assertEquals(simpleName, fspath(simpleName).simpleName)
  }
  @Test //@Ignore
  def absolute_path_should_be_rooted_at_a_root = {
    val absolute = fspath("xx/yy/aa/bb").toAbsolute
    val parents = absolute.parents
    assertTrue (fixture.fs.roots exists { parents.last == _})
  }

  @Test //@Ignore
  def convert_to_uri = {
    assertEquals(fspath("xx").toURL.toString, fspath("xx").toURI.toString)
  }


  @Test //@Ignore
  def write_will_create_Path_as_needed= {
     val xx = fixture.path / fixture.segment
     xx.write("testing")

     assertTrue(xx.exists)
     assertEquals(xx.slurpString, "testing")

     xx.write("newData")
     assertEquals(xx.slurpString, "newData")
  }
  @Test //@Ignore
  def will_create_path_when_relative_parent_does_not_exist = {
    val path = fixture.fs("xx")
    try {
      path.write("testing")

      assertTrue(path.exists)
      assertEquals(path.slurpString, "testing")
    } finally {
      path.deleteIfExists(true)
    }
  }
  @Test //@Ignore
  def open_stream_on_url = {
    val xx = fixture.path
    xx.createFile(true, false)
    xx.write("hello")

    def assertContents(s:String) {
      val read = Resource.fromInputStream(xx.toURL.openStream).slurpString
      assertEquals(s, read)
    }

    assertContents("hello")

    val conn = xx.toURL.openConnection()

    val outputStream = util.control.Exception.catching[OutputStream](classOf[Exception]) opt {conn.getOutputStream}
    if(outputStream.isDefined) {
      outputStream.foreach {_.close}
      intercept[IllegalArgumentException] {
        conn.getOutputStream
      }
      conn setDoOutput true
      Resource.fromOutputStream(conn.getOutputStream).write("write")
      assertContents("write")
      }
  }

  @Test
  def sibling_with_parent_makes_sibling_path {
    val p = fixture.root \ "c1"
    assertEquals(fixture.root \ "c2", p sibling ("c2",'/'))
    assertEquals(fixture.root \ "c2", p sibling ("c2"))
  }
  @Test
  def sibling_without_parent_equals_path {
    val p = fixture.fs.roots.head \ "a"
    assertEquals(p, fixture.fs.roots.head sibling p)
  }

  @Test //@Ignore
  def relativize_should_make_a_child_relative_to_parent = {
    val p = fixture.root \ "c1" \ "c2"
    assertEquals(2, (p relativize fixture.root).segments.size)
    assertFalse(fixture.root.path == p.path)
  }

  @Test //@Ignore
  def relativize_return_other_when_not_same_fileSystem = {
    val other:Path = new RamFileSystem().apply("other")
    assertSame(other, fixture.root relativize other)
  }
  @Test //@Ignore
  def createFile_should_fail_to_overwrite_exiting_by_default = {
    val p = (fixture.root \ "c1")

    p.createFile()

    intercept[java.io.IOException] {
      p.createFile()
    }
  }

  @Test //@Ignore
  def createFile_should_allow_option_to_fail_when_parent_is_missing = {
    val p = (fixture.root \ "c1" \ "c2" \ "c3")
    intercept[java.io.IOException] {
      p.createFile(createParents=false)
    }
    assertTrue(p.nonExistent)
  }
  @Test //@Ignore
  def createFile_should_create_parent_file_by_default = {
    val p = (fixture.root \ "c1" \ "c2" \ "c3").createFile()
    assertTrue(p.exists)
  }
  @Test //@Ignore
  def slash_method_should_create_child_path = {
    val p = fixture.root \ "c1" \ "c2"
    assertEquals(2, (p relativize fixture.root).segments.size)
  }
  @Test //@Ignore
  def path_should_support_standard_comparisons() : Unit = {
    check (false, standardPathComparisions _)
  }
  @Test //@Ignore
  def path_should_be_creatable_and_deletable() : Unit = {
    check (false, creatableAndDeletable _)
  }
  @Test //@Ignore
  def path_should_respect_file_access_restrictions() : Unit = {
    check (false, respectsAccess _)
  }
  @Test //@Ignore
  def path_should_have_exists_and_notExists_methods_that_are_not_equal() : Unit = {
    check (false, existsTest _)
  }
  @Test //@Ignore
  def create_should_fail_in_known_manner_when_parent_dir_is_not_available() : Unit = {
    val dir = fixture.path.createDirectory()
    dir.access = Read :: Nil
    def testFailure() = {
      val file = (dir \ "child")
      intercept[IOException] {  // TODO should be specific exception but maybe for next version?
        file.createFile()
      }
      intercept[IOException] {  // TODO should be specific exception but maybe for next version?
        file.createDirectory()
      }
    }
    if(!isWindows) testFailure()
    dir.access = Write :: Nil
    if(!isWindows) testFailure()
    dir.delete().createFile()
    testFailure()

    dir.delete()
  }

  @Test //@Ignore
  def non_existant_file_should_not_be_file_or_directory : Unit = {
    val f1 = fixture.path
    assertFalse(f1.exists)
    assertTrue(f1.nonExistent)
    assertFalse(f1.isDirectory)
    assertFalse(f1.isFile)
  }

  @Test //@Ignore
  def path_can_move_files_between_file_systems() : Unit = {
    val data = "file to move"
    val f1 = fixture.path
    f1.write(data)

    val otherfs = new RamFileSystem()
    val otherpath = otherfs("/","a")

    f1.moveTo(otherpath)

    assertTrue(f1.nonExistent)
    assertTrue(otherpath.exists)
    assertArrayEquals(data.getBytes(codec.charSet), otherpath.byteArray)
    assertEquals(data, otherpath.slurpString)
  }


  @Test //@Ignore
  def path_can_copy_files_between_file_systems() : Unit = {
    val data = "file to move"
    val f1 = fixture.path
    f1.write(data)

    val otherfs:FileSystem = new RamFileSystem()
    val otherpath = otherfs("/","a")

    f1.copyTo(otherpath)

    assertTrue(f1.exists)
    assertTrue(otherpath.exists)
    assertEquals(data, otherpath.slurpString)
    assertEquals(data, f1.slurpString)
  }

  @Test //@Ignore
  def path_can_move_directories_between_file_systems() : Unit = {
    def doMoveTest(otherPathSep:String) = {
      val f1 = fixture.path.createDirectory()


      val otherfs = new RamFileSystem()
      val otherpath = otherfs("/","a")

      f1.moveTo(otherpath)

      assertTrue(f1.nonExistent)
      assertTrue(otherpath.exists)
    }
    doMoveTest("/")
    doMoveTest("\\")
  }
  @Test //@Ignore
  def path_can_move_directory_trees_between_file_systems() : Unit = {
    val f1 = fixture.path.createDirectory()
    f1 \ "b" createFile()

    val otherfs = new RamFileSystem()
    val otherpath = otherfs("/","a")

    f1.moveTo(otherpath)

    assertTrue(f1.nonExistent)
    assertTrue(otherpath.exists)

    assertTrue(otherpath \ "b" exists)
  }

  @Test //@Ignore
  def path_will_rename_when_moved_to_non_existant_path() : Unit = {
    val parent = fixture.path.createDirectory()
    val f1 = parent \ "a"

    f1.write("data")

    val otherpath = f1.parent.get \ "b"

    f1.moveTo(otherpath)

    assertTrue(otherpath.exists)

    otherpath.copyTo(f1)

    assertTrue(f1.exists)
  }


    @Test //@Ignore
    def path_will_fail_when_copyTo_existing_path() : Unit = {
      val parent = fixture.path.createDirectory()
      val f1 = parent \ "a"

      f1.write("data")

      val otherpath = f1.parent.get \ "b"
      otherpath.write("data2")

      intercept[IOException] {f1.moveTo(otherpath)}
    }



  @Test //@Ignore
  def path_can_move_files() : Unit = {
    val f1 = fixture.path
    f1.write("file to move")

    val exists = fixture.path
    exists.write("pre existing file")

    move( f1, fixture.path, exists)
  }
  @Test //@Ignore
  def directories_can_overwrite_files : Unit = {
    val f = fixture
    import f.path
    path.createDirectory() moveTo (path.createFile(), replace=true)
  }
  @Test //@Ignore
  def files_cannot_overwrite_non_empty_directories : Unit = {
    val f = fixture
    import f.path
    intercept[IOException] {
      // cannot replace non_empty directory
    val dir = path.createDirectory()
    dir \ "f__" createFile ()
      path.createFile() moveTo (dir, replace=true)
    }
  // but can replace empty directories
    path.createFile() moveTo (path.createDirectory(), replace=true)
  }
  @Test //@Ignore
  def path_can_move_directories() : Unit = {
    repeat {move(fixture.path.createDirectory (), fixture.path, fixture.path.createDirectory ())}
  }
  @Test //@Ignore
  def path_can_copy_files() : Unit = {
    repeat {
      val source = fixture.path.createFile ()
      source.write(Array(1,2,3,4))
      copy( source, fixture.path(2), fixture.path.createFile ())
    }
  }
  @Test //@Ignore
  def path_can_copy_directories() : Unit = {
    repeat {copy( fixture.path.createDirectory (), fixture.path(2), fixture.path.createDirectory ())}
  }
  @Test //@Ignore
  def path_can_move_directory_trees() : Unit = {
    repeat {
      val tree1 = fixture.tree()._1
      val tree2 = fixture.tree()._1
      val target = fixture.path
      move( tree1, target, tree2, canReplace=false)
     }(5)
  }
  @Test //@Ignore
  def path_can_copy_directory_trees() : Unit = {
    repeat {copy( fixture.tree()._1, fixture.path(2), fixture.tree()._1, canReplace=false)}
  }
  @Test //@Ignore
  def path_children_only_lists_directly_contained_files() : Unit = {
    val path = fixture.tree(5)._1

    val onlyChildren = path.children() forall {_.parent.get == path}
    assertTrue((path.children() mkString ",")+" contains more files than just children", onlyChildren)
  }

  @Test //@Ignore
  def access_equal_should_be_assignable_with_string() {
    val p = fixture.path.createFile()

    p.access = "r"
    assertEquals("Read assignment failed: " + p.access, permissions(Read), p.access.toSet)
    p.access = "w"
    assertEquals("Write assignment failed: " + p.access, permissions(Write), p.access.toSet)
    p.access = "x"
    assertEquals("Execute assignment failed: " + p.access, permissions(Execute), p.access.toSet)

    p.access = ""
    assertEquals("Empty assignment failed: " + p.access, permissions(), p.access.toSet)

    p.access = "+r"
    assertEquals("+r assignment failed: " + p.access, permissions(Read), p.access.toSet)

    p.access = "-r"
    assertEquals("-1 assignment failed: " + p.access, permissions(), p.access.toSet)

    p.access = "+w"
    assertEquals("+w assignment failed: " + p.access, permissions(Write), p.access.toSet)

    p.access = "-w"
    assertEquals("-w assignment failed: " + p.access, permissions(), p.access.toSet)

    p.access = "+rxw"
    assertEquals("+rxw assignment failed: " + p.access, permissions(Read,Write,Execute), p.access.toSet)

    p.access = "+rxw"
    assertEquals("+rxw second assignment failed: " + p.access, permissions(Read,Write,Execute), p.access.toSet)

    p.access = "-xw"
    assertEquals("-xw assignment failed: " + p.access, permissions(Read), p.access.toSet)

    intercept[IOException] {
      p.access = "@"
    }
  }

  @Test //@Ignore
  def path_should_delete_respect_access_by_default() {
    val p = fixture.path.createFile()
    p.access = "r"
    intercept[IOException] {
      p.delete()
    }
    p.delete(force=true)
    assertTrue(p.nonExistent)
  }

  @Test //@Ignore
  def path_should_delete_directories_recursively() {
    val (root,_) = fixture.tree(5)

    val numFiles = root.descendants().size + 1 // add root

    val (deleted, remaining) = root.deleteRecursively()

    assertEquals(numFiles, deleted)
    assertEquals(0, remaining)
    assertTrue(root.nonExistent)
  }
  @Test //@Ignore
  def delete_recursively_should_throw_exception_on_failure_by_default() {
      val (root,_) = fixture.tree(5)

      val totalFiles = root.descendants().size + 1 // add root
      root.descendants (IsFile).take(totalFiles/2) foreach {p => p.access = Read :: Nil}
      intercept[IOException] {
        root.deleteRecursively()
      }
    }

    @Test //@Ignore
    def delete_recursively_should_be_able_to_continue_on_failure() {
      val (root,_) = fixture.tree(5)

      val totalPaths = root.descendants().size + 1

      val totalFiles = root.descendants (IsFile).size
      root.descendants (IsFile).take(totalFiles/2) foreach {p => p.access = p.access - Write}
      val (deleted, remaining) = root.deleteRecursively(continueOnFailure = true)

      assertTrue(root.exists)
      assertEquals(totalPaths, deleted + remaining)
      assertTrue(deleted != remaining)
      assertTrue(root.descendants() forall {p => (p.isDirectory && p.children().nonEmpty) || !p.canWrite})
    }

    @Test //@Ignore
    def delete_recursively_should_be_able_to_force_delete() {
      val (root,_) = fixture.tree(5)

      val totalPaths = root.descendants().size + 1
      val totalFiles = root.descendants(IsFile).size
      root.descendants (IsFile).take(totalFiles/2) foreach {p => p.access = p.access - Write}

      val (deleted, remaining) = root.deleteRecursively(force = true)

      assertEquals(totalPaths, deleted)
      assertEquals(0, remaining)
      assertTrue(root.nonExistent)
    }

  @Test //@Ignore
  def delete_recursively_should_delete_files() {
    val path = fixture.path
    path.createFile()
    assertTrue(path.exists)
    path.deleteRecursively()
    assertFalse(path.exists)
  }

  @Test //@Ignore
  def normalize_should_change_a_path_to_be_minimized() {
    assertEquals(fspath("/a/d"), fspath("/a/d").normalize)
    assertEquals(fspath("/a/d"), fspath("/a//d").normalize)
    assertEquals(fspath("a/d"), fspath("a//d").normalize)
    assertEquals(fspath("/d"), fspath("/a/../d").normalize)
    assertEquals(fspath("d"), fspath("a/../d").normalize)
    assertEquals(fspath("/a/d"), fspath("/a/./d").normalize)
    assertEquals(fspath("/a/d"), fspath("/a/b/.//c/../../d").normalize)
  }

  @Test //@Ignore
  def parent() {
    assertEquals(Some(fspath("/a/b/c")), fspath("/a/b/c/d").parent)
  }

  @Test //@Ignore
  def segments_should_return_parts_of_path() {
    assertEquals(List("a","b","c",".","..","d"), fspath("a/b//c/./../d").segments.toList)
    assertEquals(List("a","b","c",".","..","d"), fspath("a/b//c/./../d/").segments.toList)
    assertEquals(List("a","b","c",".","..","d"), fspath("a/b//c/./../d//").segments.toList)
  }
  @Test //@Ignore
  def creating_path_from_string_should_clean_path() {
    assertEquals(fspath("a/b/c/./../d").path, fspath("a/b//c/./../d").path)
    assertEquals(fspath("a/b/c/./../d").path, fspath("a/b//c/./../d/").path)
    assertEquals(fspath("a/b/c/./../d").path, fspath("a/b//c/./../d//").path)
  }

  @Test //@Ignore
  def creating_paths() {
    assertEquals("a" :: "b" :: "c" :: Nil, fixture.fs("a","b","c").segments.toList)
    assertEquals(fspath("a/b").path, fixture.fs.fromString(fspath("a/b").path).path)
    assertEquals(fspath("a/b").path, fixture.fs("a-b",'-').path)
    assertEquals(fspath("a/b/c").path, fspath("a") / "b" / "c" path)
    assertEquals(fspath("a/b/c").path, fspath("a") / ("b,c",',') path)
    assertEquals(fspath("a/b/c").path, fspath("a") / (",,b,,c,",',') path)
    assertEquals(fspath("a/b/c").path, fspath("a") / (",,b,,c,",',') path)
    assertEquals(fspath("a/b/c").path, fspath("a") \ (",,b,,c,",',') path)
    val pathToResolve = "b/c"
    assertEquals(fspath("a/b/c").path, (fspath("a") resolve (pathToResolve,'/')).path)
    val path = fspath("a")
    assertEquals(path toRealPath(), path \ "." toRealPath())
    assertEquals(path toRealPath(), path.resolve (".",'/').toRealPath())
    intercept[IllegalArgumentException] {
          path \ fixture.fs.separator
    }
  }

  @Test //@Ignore
  def path_separator_in_path_segment_throws_exception() {
    val sep = fixture.fs.separator
    intercept[IllegalArgumentException] {
    fixture.fs.apply("with","a",""+sep,"does","it","work","?")
    }
    intercept[IllegalArgumentException] {
    fixture.fs.apply("with","a") \ sep
    }
    intercept[IllegalArgumentException] {
      fixture.fs.fromSeq(Seq("with","a",""+sep,"does","it","work","?"))
    }
  }

  def check = fixture.check _

  def move(f1 :Path, f2: Path, exists: Path, canReplace: Boolean=true)={
    assertTrue("expected 'exists' to exist before test", exists.exists)
    assertTrue("expected f1 to exist before move", f1.exists)
    assertTrue("expected f2 to NOT exist before move", f2.nonExistent)
    assertEquals(f2, f1 moveTo f2)
    assertTrue("expected f2 to exist after move", f2.exists)
    assertTrue("expected f1 to NOT exist after move", f1.nonExistent)

    f2 moveTo f2
    assertTrue("expected f2 to exist after move to self", f2.exists)
    intercept[IOException] {
      f1 moveTo f2
    }
    assertTrue("expected f2 to exist after attempting to move a nonexisting f1 to f2", f2.exists)

    val existsBeforeMove = if(exists.isFile) {
        val content = exists.chars mkString ""
        assertFalse("contents of exists should not equal f2", content == (f2.chars mkString ""))
        content
      } else {
        ""
      }
    intercept[IOException] {
      f2 moveTo exists
    }
    assertTrue("expected f2 to exist after attempting a non-overwrite move to an existing file", f2.exists)
    assertTrue("expected exists to exist after attempting a non-overwrite", exists.exists)
    if(exists.isFile) {
      assertTrue("expected exists to have the same contents after an illegal replace", existsBeforeMove == (exists.chars mkString ""))
    }
    def tryReplace = {
      assertTrue("expected f2 to exist before replace", f2.exists)
      f2.moveTo (exists, replace=true)
      assertTrue (f2.nonExistent)
      assertTrue (exists.exists)
    }

    if (canReplace) tryReplace
    else intercept[IOException] {tryReplace}


    intercept[IOException] {
      fixture.fs.roots.head moveTo fixture.path
    }

    // TODO test moving between filesystems
    // TODO check access of moved file/directory

    // TODO check that attributes of moved file are the same as before move
  }


  def copy(f1 :Path, f2: Path, exists: Path, canReplace: Boolean=true)={
    assertTrue(f1.exists)
    assertTrue(f2.nonExistent)
    assertTrue(f2.parent.forall{_.nonExistent})
    intercept[IOException] {
      f1 copyTo (f2, createParents=false)
    }
    f1.access(Path.AccessModes.Execute) = true
    f1 copyTo f2

    assertTrue("lastModified attribute was not copied: f1="+f1.lastModified+", f2="+f2.lastModified,
               f1.lastModified - f2.lastModified < 0.000001)
    assertTrue(f2.exists)
    assertTrue(f1.exists)
    assertTrue("canExecute was not copied when it should not have", f2.canExecute)

    if(f2.isDirectory) {
      if(f2.children().nonEmpty) {
        try {
          f2.delete()
          fail("Expected an exception because delete is not intended to delete recursively")
        } catch {
          case e:IOException => assertFalse("Error should be a known error and have good message", e.getMessage().contains("unknown"))
        }
      }
    } else {
      intercept[NotDirectoryException]{
        // can't call children on a file
        f2.children()
      }
    }
    f2.deleteRecursively(force=true)
    assertTrue("failed to delete f2", f2.nonExistent)
    f1.lastModified = 10000
    f1.access(Path.AccessModes.Execute) = true
    f1 copyTo (f2, copyAttributes=false)

    assertTrue("lastModified attribute was copied when it should not have", f1.lastModified < f2.lastModified)
    if(f2.isFile && !isWindows) assertFalse("canExecute was copied when it should not have", f2.canExecute)


    f2 copyTo f2 // noop
    intercept[IOException] {
      f1 copyTo f2
    }
    intercept[IOException] {
      f2 copyTo exists
    }
    def overwrite = {
      if(!isWindows) {
        val access = exists.access.toList
        exists.access = List(Read)
        intercept[IOException] {
          f2.copyTo (exists, replaceExisting=true)
        }
        exists.access = access
      }

      f2.copyTo (exists, replaceExisting=true)
      assertTrue(f2.exists)
      assertTrue(exists.exists)
    }

    if (canReplace) overwrite
    else intercept[IOException] {overwrite}

    // TODO check attributes/access of copied file/directory
  }

  def standardPathComparisions(testData: TestData):Unit = {
    import testData._

    val path = fspath(pathName)
    assertEquals(path, path)
    assertEquals(path, fspath(pathName))

    assertEquals(pathName, path.path)
    assertTrue(pathName endsWith path.name)

    assertTrue(path endsWith fspath(path.name))
    assertTrue(path startsWith fspath(path.segments.head))
    assertTrue(path isSame fspath(pathName))
    assertEquals(fspath(pathName), path)
    assertEquals(path, fspath(pathName))
    assertEquals(path.hashCode, fspath(pathName).hashCode)


    val pathx = path.resolve(fspath("x"))

    assertNotNull(pathx)
    assertEquals(pathx, path.resolve("x",'/'))
    assertEquals("x", pathx.segments.last)
    assertEquals(fspath("x"), pathx.relativize(path))
  }

  def creatableAndDeletable(testData: TestData):Unit = {
    import testData._

    val path = fspath(pathName)

    assertTrue(path.nonExistent)
    assertFalse(path.exists)
    assertFalse("expected path access to NOT be "+access, !access.isEmpty && (path checkAccess (access:_*)))
    path.createFile (accessModes = access)

    assertFalse(path.nonExistent)
    assertTrue(path.exists)
    assertTrue("expected path access to be "+access+".  Access is "+path.access, path checkAccess (access:_*))

    path.access = List(Read)
    intercept[IOException] {
      path.delete()  // not writeable
    }
    assertTrue(path.exists)

    path.delete(force=true)
    assertTrue(path.nonExistent)

    path.createFile()
    assertTrue(path.exists)

    intercept[IOException] {
      // fails since it does not specify failIfExists = false
      path.createFile()
    }
    assertSame(path, path.createFile(failIfExists = false))

    path.delete()
    path.delete()

    assertTrue(path.nonExistent)
    assertFalse(path.exists)
    assertFalse(!access.isEmpty && (path checkAccess (access:_*)))
  }

  def existsTest(testData : TestData) : Unit = {
      val path = fspath(testData.pathName)
      assertTrue(path.exists != path.nonExistent)
      assertFalse(path.exists)
      path.createFile()
      assertTrue(path.exists)
      assertTrue(path.exists != path.nonExistent)
  }

  def respectsAccess(testData: TestData):Unit = {
    import testData._

    val path = fspath(pathName)
    assertEquals(Nil, path.access.toList)
    intercept[IOException] {
      path.access = Path.AccessModes.Write :: Nil
    }
    path.createFile()
    path.write("some test data")
    path.access = access
    (Path.AccessModes.values -- access) foreach { a => matchAccess(a, path, false) }

    access foreach { a => matchAccess(a, path, true) }
  }
  def verifyAccess (test: => Unit)(is : Boolean)={
    if (is) test
    else intercept[IOException] {test}
  }

  def readTest(path: Path) = path.chars.head

  def writeTest(path: Path) = path.write("abc")

  def execTest(path: Path) = if(!path.canExecute) throw new IOException()

  def matchAccess(access: AccessMode, path: Path, is: Boolean) = access match {
    case Execute if isWindows => verifyAccess (execTest(path))(true)
    case Execute => verifyAccess (execTest(path))(is)
    case Write => verifyAccess (writeTest(path))(is)
    case Read if isWindows=> verifyAccess (readTest(path))(true)
    case Read => verifyAccess (readTest(path))(is)
  }
  /*
    AccessModes.values intersect (access) foreach { a => verifyTest(access, path, is) }

    access foreach { a => verifyTest(access, path, is) }
  */
}
