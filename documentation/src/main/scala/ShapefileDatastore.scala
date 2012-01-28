import scalax.file.Path
import scalax.io.LongTraversable
import scalax.io.processing.Processor
import scalax.io.processing.ProcessorAPI

/**
 * Read the Shapefile and println one attribute from each record
 * 
 * param 1 == path to shapefile to read
 * param 2 == index of attribute to print
 */
object ShapefileReader extends App{
  val shpFile = Path.fromString(args(0))
  val csvFile = shpFile / ".." / (shpFile.simpleName + ".csv")
  val ds = new ShapefileDatastore(shpFile, csvFile)
  ds.shpRecords.map (_.attributes(args(1).toInt)).foreach(println)
}

/**
 * Assumption is that csv and shp file are in sync and therefore 
 * there are the same number of records
 */
class ShapefileDatastore(val csvFile: Path, val shapefile: Path) {
/*  lazy val metadata = new {
    val headerData = indexFile.bytes.take(100).force
    val size = (headerData(24) - 50) / 8
    val shapeType = Shapetype(headerData(32))
    val bbox = BoundingBox(headerData)
    def recordIndex(recordNum: Int) = (recordNum * 8) + 100
  }*/
  
  /*private val readRecordHeader = (bytes:Seq[Byte]) => {
    SizeBlockMapResult(Shapefile)
  }
  def shpRecords = shapefile.channel().bytes.drop(100).blockMap(8){ bytes => 
    
  }*/
  private def correctRecord(recordNum:Int, api:ProcessorAPI[(String,Int)]):Processor[Seq[String]] =
    for((record, num) <- api.next) yield {
      if(num == recordNum) record.split(",")
      else Seq.empty
    }

  val shpRecords = {
    val process = for {
      shpProcessor <- shapefile.bytes.drop(100).processor
      csvProcessor <- csvFile.lines().zipWithIndex.processor
      _ <- shpProcessor.repeatUntilEmpty()
      recordHeader <- shpProcessor.take(8)
      recordNumber = recordHeader(0)
      geometry <- shpProcessor.take(recordHeader(4).toInt)
      shapeType = ShapeType(geometry(0))
      attributes <- correctRecord(recordNumber,csvProcessor)
    } yield {
      ShapefileRecord(recordNumber, geometry.drop(1), attributes)
    }
    process.traversable
  }
}

case class ShapefileRecord(num:Int, data:Seq[Byte], attributes:Seq[String])
object BoundingBox {
  def apply(headerData: LongTraversable[Byte]): BoundingBox =
    BoundingBox(headerData(36).toInt, headerData(44).toInt, headerData(52).toInt, headerData(60).toInt,
      headerData(68).toInt, headerData(76).toInt, headerData(84).toInt, headerData(92).toInt)
}
case class BoundingBox(xmin: Int, ymin: Int, xmax: Int, ymax: Int, zmin: Int, zmax: Int, mmin: Int, mmax: Int)
trait ShapeType
object NullShape
object Point
object PointZ
object PointM
object MultiPoint
object MultiPointZ
object MultiPointM
object Linestring
object PolyLine
object PolyLineZ
object PolyLineM
object Polygon
object PolygonZ
object PolygonM
object MultiPatch

object ShapeType {
  def apply(byte: Byte) = {
    byte match {
      case 0 => NullShape
      case 1 => Point
      case 3 => PolyLine
      case 5 => Polygon
      case 8 => MultiPoint
      case 11 => PointZ
      case 13 => PolyLineZ
      case 15 => PolygonZ
      case 18 => MultiPointZ
      case 21 => PointM
      case 23 => PolyLineM
      case 25 => PolygonM
      case 28 => MultiPointM
      case 31 => MultiPatch
    }
  }
}