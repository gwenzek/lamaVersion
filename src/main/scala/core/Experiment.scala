package com.lamaVersion.core


import scala.sys.process._
import java.io.File
import scala.io.Source
import scala.language.postfixOps


object Command{
    def mkdir(dir: String) = Seq("mkdir", dir)
}

class Experiment(val name: String,
                 val command: ProcessBuilder,
                 val accept: Commit => Boolean = _ => false, 
                 val outputs: Seq[String] = Nil){

    def execute(output: String) = {
        if(command.toString != "[]")
            command #>> new File(output) !
        else
            println("Empty experiment can't be executed")
    }

    def success() = command.! == 0

    def extractResultsTo(workingPath: String, outputPath: String, ext: String){
        for(file <- outputs){
            println(file)
            val splitted = file.split(".")
            new File(workingPath + '/' + file) #> 
                new File(outputPath + '/' + splitted(0) + '_' + ext + '.' + splitted(1)) !
        }
    }
}

object Experiment{

    def fromStream(lines: Stream[String], cwd: String = ".") = {
        val workingDir = new File(cwd)

        val name = lines find(_.startsWith("# NAME: ")) getOrElse("# NAME: --unnamed--") substring(8)
        
        val beginDate = lines.find(_.startsWith("# BEGIN: ")).map( (line: String) => 
                Commit.gitDateFormat.parseDateTime(line.substring(9)))
        val endDate = lines.find(_.startsWith("# END: ")).map( (line: String) => 
                Commit.gitDateFormat.parseDateTime(line.substring(7)))
        
        val commands = lines filterNot((l: String) => l.startsWith("#") || l == "")

        val outputs = lines filter(_.startsWith("# GET: ")) flatMap(_.substring(7).split(" "))

        (beginDate, endDate) match {
            case (None, None) => println("No filtering: accepting all")
            case (Some(begin), None) => println("Filtering dates after : " + beginDate)
            case (None, Some(end)) => println("Filtering dates before : " + endDate)
            case (Some(begin), Some(end)) => println("Filtering dates after : " + beginDate 
                                                    + ", and before : " + endDate)
        }

        def check(c: Commit) = (beginDate, endDate) match {
            case (None, None) => true
            case (Some(begin), None) => c.date.isAfter(begin)
            case (None, Some(end)) => c.date.isBefore(end)
            case (Some(begin), Some(end)) => c.date.isAfter(begin) && c.date.isBefore(end)
        }

        def toProcessBuilder(commands: Stream[String]): ProcessBuilder = commands match {
            case a #:: b #:: l => Process(a, workingDir) ### toProcessBuilder(b #:: l)
            case a #:: Stream.Empty => Process(a, workingDir)
            case Stream.Empty => ""
        }

        new Experiment(name, toProcessBuilder(commands), check, outputs)
    }

    def fromFile(file: String, cwd: String = ".") = {
        val lines = Source.fromFile(file).getLines.toStream
        fromStream(lines, cwd)
    }
}