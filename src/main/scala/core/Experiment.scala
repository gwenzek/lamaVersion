package com.lamaVersion.core


import scala.sys.process._
import java.io.File
import scala.io.Source
import scala.language.postfixOps
import com.lamaVersion.impl.EasyIO


object Command{
    def mkdir(dir: String) = Seq("mkdir", dir)
}

class Experiment(val name: String,
                 val command: ProcessBuilder,
                 val accept: Commit => Boolean = _ => false, 
                 val outputs: Seq[String] = Nil){

    def execute(output: File) = {
        if(command.toString != "[]")
            command #>> output !
        else
            println("Empty experiment can't be executed")
    }

    def success() = command.! == 0

    def extractResultsTo(workingDir: File, outputDir: File, ext: String){
        for(file <- outputs){
            println(file)
            val splitted = file.split(".")
            new File(workingDir, file) #> 
                new File(outputDir, splitted(0) + '_' + ext + '.' + splitted(1)) !
        }
    }
}

object Experiment{

    def fromStream(lines: Stream[String], name: String, cwd: String = ".") = {
        val workingDir = new File(cwd)

        val beginDate = lines.find(_.startsWith("# BEGIN: ")).map( (line: String) => 
                Commit.gitDateFormat.parseDateTime(line.substring(9)))
        val endDate = lines.find(_.startsWith("# END: ")).map( (line: String) => 
                Commit.gitDateFormat.parseDateTime(line.substring(7)))
        
        val commands = lines filterNot((l: String) => l.startsWith("#") || l == "")

        val outputs = lines filter(_.startsWith("# GET: ")) flatMap(_.substring(7).split(" "))

        val excluded = lines filter(_.startsWith("# EXCLUDE: ")) flatMap(_.substring(11).split(" "))

        def accept(c: Commit) =  {
            val afterBegin = beginDate.map(begin => c.date.isAfter(begin) || c.date.isEqual(begin))
            val beforeEnd = endDate.map(end => c.date.isBefore(end) || c.date.isEqual(end))
            val isExcluded = excluded.contains(c.hash) || excluded.contains(c.shortHash)
            afterBegin.getOrElse(true) && beforeEnd.getOrElse(true) && !isExcluded
        }

        def toProcessBuilder(commands: Stream[String]): ProcessBuilder = commands match {
            case a #:: b #:: l => Process(a, workingDir) ### toProcessBuilder(b #:: l)
            case a #:: Stream.Empty => Process(a, workingDir)
            case Stream.Empty => ""
        }

        new Experiment(name, toProcessBuilder(commands), accept, outputs)
    }

    def fromFile(file: File, cwd: String = ".") = {
        val lines = Source.fromFile(file).getLines.toStream
        fromStream(lines, EasyIO.getShortFileName(file), cwd)
    }
}