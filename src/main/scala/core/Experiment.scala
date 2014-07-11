package com.lamaVersion.core


import scala.sys.process._
import java.io.File
import scala.io.Source


object Command{
    def fileExist(name: String) : Boolean = Seq("test", "-f", name).! == 0
    def mkdir(dir: String) = Seq("mkdir", dir)
}

class Experiment(val name: String, val command: ProcessBuilder, val outputs: Seq[String] = Nil){

    def execute() = {
        command #>> new File(name + ".out") ! 
    } 

    def success() = command.! == 0

    def extractResultsTo(workingPath: String, outputPath: String, ext: String){
        for(file <- outputs){
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
        
        val outputs = lines filter(_.startsWith("#")) map(_.substring(1))
        val commands = lines filterNot(_.startsWith("#"))

        def toProcessBuilder(commands: List[String]): ProcessBuilder = commands match {
            case a :: b :: l => Process(a, workingDir) ### toProcessBuilder(b :: l)
            case a :: Nil => Process(a, workingDir)
            case Nil => ""
        }

        new Experiment(name, toProcessBuilder(commands), outputs)
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