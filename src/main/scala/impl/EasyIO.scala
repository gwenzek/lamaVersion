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

    def listExtensions(dir: String, ext: String) = 
        new File(dir).list().filter(_.endsWith("." + ext)).toIterator.map(dir + '/' + _)

    def fileExists(file: String) = new File(file).exists

    def insureDirExists(dir: String) =
        if(!fileExists(dir))
            scala.sys.process.Process(Seq("mkdir", dir)).!

    def getShortFileName(file: String) = file.split("/").last.split("\\.")(0)

    class AnyOp {
        def asInt() = this match { case IntOp(i) => i }
        def asString() = this match { case StringOp(s) => s }
        def asBoolean() = this match { case FlagOp(b) => b}

        def length() = this match {
            case IntOp(_) | StringOp(_) => 2
            case FlagOp(_) => 1
        }
    }
    sealed case class IntOp(val value: Int) extends AnyOp
    implicit def toIntValue(i: IntOp) = i.value
    sealed case class StringOp(val value: String) extends AnyOp
    implicit def toStringValue(s: StringOp) = s.value
    sealed case class FlagOp(val value: Boolean) extends AnyOp
    implicit def toBooleanValue(b: FlagOp) = b.value

    private def toOp(a: Any): (String, AnyOp) = a match {
        case (name: String, s: String) => name -> StringOp(s)
        case (name: String, i: Int) => name -> IntOp(i)
        case (name: String) => name -> FlagOp(false)
    }

    case class OptionWithoutExpectedParam(name: String) extends Exception
    case class UnknownOption(name: String) extends Exception

    class ArgOpsParser(default: Map[String, AnyOp]){
        def parse(args: Array[String]) = {
            var i = 0
            var l: List[(String, AnyOp)] = Nil
            while(i < args.length && args(i)(0) == '-' ){
                val name = args(i).substring(1)
                if(!default.contains(name)) throw UnknownOption(name)
                val defOp: AnyOp = default(name)
                val op = defOp match {
                    case IntOp(_) => 
                        try { IntOp(args(i+1).toInt) } catch { 
                            case e: java.lang.NumberFormatException => throw OptionWithoutExpectedParam(name)
                            case e: java.lang.ArrayIndexOutOfBoundsException => throw OptionWithoutExpectedParam(name)
                        }
                    case StringOp(_) => 
                        if((i+1) == args.length || args(i+1)(0) == '-') throw OptionWithoutExpectedParam(name)
                        else StringOp(args(i+1))
                    case FlagOp(_) => FlagOp(true)
                }
                l = (name -> op) :: l            
                i += op.length
            }
            var map = Map(l:_*)
            for(key <- default.keys){
                if(!map.contains(key)) map = map + (key -> default(key))
            }
            new ArgOps(map, args.slice(i, args.length))
        }

        def <<|(args: Array[String]) = parse(args)
    }

    class ArgOps(ops: Map[String, AnyOp], val args: Array[String]){
        def apply(op: String) = ops(op)
    }

    object ArgOps {
        def apply(ops: Any*)() = new ArgOpsParser(ops.map(toOp).toMap)
    }
}