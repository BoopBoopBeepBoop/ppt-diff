import java.io.{PrintWriter, File}
import java.nio.file.{Path, Files}
import scala.sys.process._
import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.Try

/**
  */
object PPTDiff extends App {
  
  var printer: String => Unit = println
  
  implicit def s2file(s: String): File = new File(s)
  
  implicit def f2Path(f: File): Path = f.toPath
  
  implicit def p2File(p: Path): File = p.toFile

  def file(s: String) = new File(s)

  def rm(f: File) = Files.deleteIfExists(f.toPath)

  def mv(f: File, f2: File) = Files.move(f.toPath, f2.toPath).toFile

  def exec(s: String) = {
//    printer(s"EXEC: $s")
    s.!!
  }

  def exec(p: ProcessBuilder) = {
//    printer(s"EXEC: $p")
    p.!!
  }

  def unzipReturnRoot(f: File, dest: File): File = {
    val parent: Path = f.toPath.getParent
    printer(exec(s"unzip -o ${f.getPath} -d $dest"))
    dest
  }
  
  def filesRec(f: File): Iterable[File] = {
    f.listFiles().flatMap { f => 
      if(f.isDirectory) filesRec(f)
      else Seq(f)
    }
  }

  def rm_r(f: File, regex: Regex = ".*".r) = {
    val pattern = regex.pattern

    val files = filesRec(f).filter { file =>
      val relative = f.toPath.relativize(file.toPath).toString
      val matcher = pattern.matcher(relative)
      matcher.find() && matcher.start() == 0
    }

    files foreach rm
  }
  
  def formatXml(f: File) = {
    printer("Reformatting: " + f.getPath)
    exec(s"cat ${f.getPath}" #| s"xmllint --format - " #>> file(s"${f.getPath}.2"))
    rm(f)
    mv(s"${f.getPath}.2", f)
  }

  val suffixes = Seq(".xml", ".xml.rels")

  def recursiveXmlFormat(dir: File): Map[String, File] = {
    filesRec(dir)
      .filter(f => suffixes.exists(f.getName.endsWith))
      .map(formatXml)
      .map(f => dir.relativize(f).toString -> f)
      .toMap
  }

  def zipMaps[A, B](map1: Map[A, B], map2: Map[A, B]): Map[A, (Option[B], Option[B])] = {
    (map1.keys ++ map2.keys)
      .toSet
      .map((key: A) => key ->(map1.get(key), map2.get(key)))
      .toMap
  }

  def compareFiles(f1: File, f2: File) = {
    val cmd = s"diff -u ${f1.getAbsolutePath} ${f2.getAbsolutePath}"
    val c = cmd.run(ProcessLogger(printer))
    if(c.exitValue() == 0) printer(s"No change in file ${f1.getAbsolutePath} ")
  }

  {
    //val writer = new PrintWriter(file("temp"))
//    printer = s => writer.append(s + "\n")

		if(args.size < 2) {
			println("Usage: <file1.ppt> <file2.ppt>")
		}

    val tmp = Files.createTempDirectory("ppt-cmp")
    try {

      val rootOne = unzipReturnRoot(args(0), s"${tmp.toAbsolutePath}/one")
      val rootTwo = unzipReturnRoot(args(1), s"${tmp.toAbsolutePath}/two")

      val xmlFilesOne = recursiveXmlFormat(rootOne)
      val xmlFilesTwo = recursiveXmlFormat(rootTwo)

      val allFilesByName = zipMaps(xmlFilesOne, xmlFilesTwo)


      val order = new Ordering[Action] {
        override def compare(x: Action, y: Action): Int = {
          x.getClass.getSimpleName.compareTo(y.getClass.getSimpleName) * -1
        }
      }

      val actions = allFilesByName.map {
        case (k, (Some(original), None)) => Removed(k)
        case (k, (None, Some(repaired))) => Added(k)
        case (k, (Some(original), Some(repaired))) => Comparison(k, original, repaired)
      }

      actions.toSeq.sorted(order).foreach(a => a.doIt())
    } finally {
			//Try(writer.close())
      Try(rm_r(tmp))      
    }
  }
}



sealed trait Action {
  def doIt()
}

case class Added(s: String) extends Action {
  override def doIt(): Unit = PPTDiff.printer(s"Added $s")
}
case class Removed(s: String) extends Action {
  override def doIt(): Unit = PPTDiff.printer(s"Removed $s")
}
case class Comparison(s: String, original: File, repaired: File) extends Action {
  override def doIt(): Unit = PPTDiff.compareFiles(original, repaired)
}
