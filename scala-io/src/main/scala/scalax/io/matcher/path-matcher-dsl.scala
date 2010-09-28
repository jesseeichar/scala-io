package scalax.io

import collection.mutable.HashSet
import java.net.URL


/** A path finder constructs a set of paths.  The set is evaluated by a call to the <code>get</code>
* method.  The set will be different for different calls to <code>get</code> if the underlying filesystem
* has changed.*/
sealed abstract class PathFinder extends PathMatcher
{
  type FileFilter = Path => Boolean 
	/** The union of the paths found by this <code>PathFinder</code> with the paths found by 'paths'.*/
	def +++(paths: PathFinder): PathFinder = new Paths(this, paths)
	/** Excludes all paths from <code>excludePaths</code> from the paths selected by this <code>PathFinder</code>.*/
	def ---(excludePaths: PathFinder): PathFinder = new ExcludePaths(this, excludePaths)
	/** Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
	* descendents of paths selected by this finder.*/
	def **(filter: FileFilter): PathFinder = new DescendentOrSelfPathFinder(this, filter)
	def *** : PathFinder = **(AllPassFilter)
	/** Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
	* immediate children of paths selected by this finder.*/
	def *(filter: FileFilter): PathFinder = new ChildPathFinder(this, filter)
	/** Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
	* of paths selected by this finder.*/
	def / (literal: String): PathFinder = new ChildPathFinder(this, new ExactFilter(literal))
	/** Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
	* of paths selected by this finder.*/
	final def \ (literal: String): PathFinder = this / literal

	/** Makes the paths selected by this finder into base directories.
	* @see Path.##
	*/
	def ## : PathFinder = new BasePathFinder(this)

	/** Selects all descendent paths with a name that matches <code>include</code> and do not have an intermediate
	* path with a name that matches <code>intermediateExclude</code>.  Typical usage is:
	*
	* <code>descendentsExcept("*.jar", ".svn")</code>*/
	def descendentsExcept(include: FileFilter, intermediateExclude: FileFilter): PathFinder =
		(this ** include) --- (this ** intermediateExclude ** include)

	/** Evaluates this finder.  The set returned by this method will reflect the underlying filesystem at the
	* time of calling.  If the filesystem changes, two calls to this method might be different.*/
	final def get: scala.collection.Set[Path] =
	{
		val pathSet = new HashSet[Path]
		addTo(pathSet)
		wrap.Wrappers.readOnly(pathSet)
	}
	/** Only keeps paths for which `f` returns true.  It is non-strict, so it is not evaluated until the returned finder is evaluated.*/
	final def filter(f: Path => Boolean): PathFinder = Path.lazyPathFinder(get.filter(f))
	/* Non-strict flatMap: no evaluation occurs until the returned finder is evaluated.*/
	final def flatMap(f: Path => PathFinder): PathFinder = Path.lazyPathFinder(get.flatMap(p => f(p).get))
	/** Evaluates this finder and converts the results to an `Array` of `URL`s..*/
	final def getURLs: Array[URL] = Path.getURLs(get)
	/** Evaluates this finder and converts the results to a `Set` of absolute path strings.*/
	final def getPaths: Set[String] = strictMap(_.absolutePath)
	/** Evaluates this finder and converts the results to a `Set` of relative path strings.*/
	final def getRelativePaths: Set[String] = strictMap(_.relativePath)
	final def strictMap[T](f: Path => T): Set[T] = Path.mapSet(get)(f)
	private[sbt] def addTo(pathSet: Set[Path])

	/** Create a PathFinder from this one where each path has a unique name.
	* A single path is arbitrarily selected from the set of paths with the same name.*/
	def distinct: PathFinder = Path.lazyPathFinder((Map() ++ get.map(p => (p.asFile.getName, p))) .values.toList )

	/** Constructs a string by evaluating this finder, converting the resulting Paths to absolute path strings, and joining them with the platform path separator.*/
	final def absString = Path.makeString(get)
	/** Constructs a string by evaluating this finder, converting the resulting Paths to relative path strings, and joining them with the platform path separator.*/
	final def relativeString = Path.makeRelativeString(get)
	/** Constructs a debugging string for this finder by evaluating it and separating paths by newlines.*/
	override def toString = get.mkString("\n   ", "\n   ","")
}
private class BasePathFinder(base: PathFinder) extends PathFinder
{
	private[sbt] def addTo(pathSet: Set[Path])
	{
		for(path <- base.get)
			pathSet += (path ##)
	}
}
private abstract class FilterPath extends PathFinder with FileFilter
{
	def parent: PathFinder
	def filter: FileFilter
	final def accept(file: File) = filter.accept(file)

	protected def handlePath(path: Path, pathSet: Set[Path])
	{
		for(matchedFile <- wrapNull(path.asFile.listFiles(this)))
			pathSet += path / matchedFile.getName
	}
}
private class DescendentOrSelfPathFinder(val parent: PathFinder, val filter: FileFilter) extends FilterPath
{
	private[sbt] def addTo(pathSet: Set[Path])
	{
		for(path <- parent.get)
		{
			if(accept(path.asFile))
				pathSet += path
			handlePathDescendent(path, pathSet)
		}
	}
	private def handlePathDescendent(path: Path, pathSet: Set[Path])
	{
		handlePath(path, pathSet)
		for(childDirectory <- wrapNull(path.asFile.listFiles(DirectoryFilter)))
			handlePathDescendent(path / childDirectory.getName, pathSet)
	}
}
private class ChildPathFinder(val parent: PathFinder, val filter: FileFilter) extends FilterPath
{
	private[sbt] def addTo(pathSet: Set[Path])
	{
		for(path <- parent.get)
			handlePath(path, pathSet)
	}
}
private class Paths(a: PathFinder, b: PathFinder) extends PathFinder
{
	private[sbt] def addTo(pathSet: Set[Path])
	{
		a.addTo(pathSet)
		b.addTo(pathSet)
	}
}
private class ExcludePaths(include: PathFinder, exclude: PathFinder) extends PathFinder
{
	private[sbt] def addTo(pathSet: Set[Path])
	{
		val includeSet = new HashSet[Path]
		include.addTo(includeSet)

		val excludeSet = new HashSet[Path]
		exclude.addTo(excludeSet)

		includeSet --= excludeSet
		pathSet ++= includeSet
	}
}
