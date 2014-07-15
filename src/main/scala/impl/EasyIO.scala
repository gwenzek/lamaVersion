package impl

import java.io.{File, FileWriter}
import scala.language.implicitConversions


object EasyIO{
    implicit class WriteAndClose(output: String) {
        def <<| (s: String) = {
            val writer = new FileWriter(output)
            writer.write(s)
            writer.close()
        }

        def <<| (messages: Iterable[String]) {
            val writer = new FileWriter(output)
            messages.foreach((s: String) => writer.write(s + '\n'))
            writer.close()
        }
    }

    def listExtensions(dir: String, ext: String) = 
        new File(dir).list().filter(_.endsWith("." + ext)).toIterator.map(dir + '/' + _)

    def fileExists(file: String) = new File(file).exists

    def insureDirExists(dir: String) =
        if(!fileExists(dir))
            scala.sys.process.Process(Seq("mkdir", dir)).!

    def getShortFileName(file: String) = file.split("/").last.split("\\.")(0)
}