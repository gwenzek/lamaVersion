package com.lamaVersion.impl

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

    def listExtensions(dir: File, ext: String) = 
        dir.listFiles.filter(_.getName.endsWith("." + ext)).toIterator

    def insureDirExists(dir: File) =
        if(!dir.exists) scala.sys.process.Process(Seq("mkdir", dir.getPath)).!

    def getShortFileName(file: File) = file.getName.split("/").last.split("\\.")(0)

}