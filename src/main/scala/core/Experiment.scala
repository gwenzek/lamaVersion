package com.lamaVersion.core


import scala.sys.process._
import java.io.File
import scala.io.Source
import scala.language.postfixOps


object Command{
    def mkdir(dir: String) = Seq("mkdir", dir)
}

class Experiment(val name: String, val command: ProcessBuilder,
    val accept: Commit => Boolean = _ => false, val outputs: Seq[String] = Nil){

    def execute(filename: String) = {
        command #>> new File(filename) ! 
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
    def fromFile(file: String, cwd: String = ".") = {
        val workingDir = new File(cwd)
        val in = Source.fromFile(file)
        val lines = in.getLines.toList
        val name = lines.head.substring(1)
        
        val outputs = lines filter(_.startsWith("# prints in: ")) map(_.substring(1))
        val commands = lines filterNot(_.startsWith("#"))
        val beginDate = lines.find(_.startsWith("# BEGIN: ")).map( (line: String) => 
                Commit.gitDateFormat.parseDateTime(line.substring(9)))
        val endDate = lines.find(_.startsWith("# END: ")).map( (line: String) => 
                Commit.gitDateFormat.parseDateTime(line.substring(7)))

        def check(c: Commit) = (beginDate, endDate) match {
            case (None, None) => false
            case (Some(begin), None) => c.date.isAfter(begin)
            case (None, Some(end)) => c.date.isBefore(end)
            case (Some(begin), Some(end)) => c.date.isAfter(begin) && c.date.isBefore(end)
        }

        def toProcessBuilder(commands: List[String]): ProcessBuilder = commands match {
            case a :: b :: l => Process(a, workingDir) ### toProcessBuilder(b :: l)
            case a :: Nil => Process(a, workingDir)
            case Nil => ""
        }

        new Experiment(name, toProcessBuilder(commands), check, outputs)
    }

    def processFromFile(file: String) = {
        val in = Source.fromFile(file)
        val lines = in.getLines
        val commands = lines filterNot(_.startsWith("#"))

        def toProcessBuilder(commands: List[String]): ProcessBuilder = commands match {
            case a :: b :: l => a ### toProcessBuilder(b :: l)
            case a :: Nil => a
            case Nil => ""
        }

        toProcessBuilder(commands.toList)
    }
}