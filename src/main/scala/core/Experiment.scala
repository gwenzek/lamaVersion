package com.lamaVersion.core


import scala.sys.process._
import java.io.File
import scala.io.Source
import impl.EasyIO.WriteAndClose
import scala.annotation.tailrec


object Command{
    def fileExist(name: String) : ProcessBuilder = Seq("test", "-f", name)
    def mkdir(dir: String) = Seq("mkdir", dir)
}

class Experiment(val name: String, val command: ProcessBuilder, val outputs: Seq[String] = Nil){

    def execute() = {
        name + ".out" <<| command.lineStream
    } 

    def success() = command.! == 0
}

object Experiment{
    def fromFile(file: String) = {
        val in = Source.fromFile(file)
        val lines = in.getLines.toList
        val name = lines.head.substring(1)
        
        val outputs = lines filter(_.startsWith("#")) map(_.substring(1))
        val commands = lines filterNot(_.startsWith("#"))

        def toProcessBuilder(commands: List[String]): ProcessBuilder = commands match {
            case a :: b :: l => a ### toProcessBuilder(b :: l)
            case a :: Nil => a
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