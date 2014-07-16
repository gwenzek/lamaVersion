package com.lamaVersion.impl

import scala.language.implicitConversions

/**
 * @author gwenzek
 *
 */

class ArgsOps(ops: Map[String, OptionalParam], val args: Array[String]){
    def apply(op: String) = ops(op)
}

class OptionalParam(val name: String) {
    def asInt() = this match { case IntParam(_, i) => i }
    def asString() = this match { case StringParam(_, s) => s }
    def asBoolean() = this match { case FlagParam(_, b) => b}

    def length() = this match {
        case IntParam(_, _) | StringParam(_, _) => 2
        case FlagParam(_, _) => 1
    }

    def toPair() = name -> this
}

sealed case class IntParam(override val name: String, val value: Int) extends OptionalParam(name)
sealed case class StringParam(override val name: String, val value: String) extends OptionalParam(name)
sealed case class FlagParam(override val name: String, val value: Boolean = false) extends OptionalParam(name)

object ArgsOps {
    def apply(ops: OptionalParam*)() = new ArgsOpsParser(ops.map(_.toPair).toMap)

    case class OptionWithoutExpectedParam(name: String) extends Exception
    case class UnknownParam(name: String) extends Exception

    class ArgsOpsParser(default: Map[String, OptionalParam]){
        
        def parse(args: Array[String]) = {

            def parseOne(i: Int, l: List[OptionalParam]): (Int, List[OptionalParam]) = {
                if(i == args.length || args(i)(0) != '-') (i, l)
                else{
                    val name = args(i).substring(1)
                    if(!default.contains(name)) throw UnknownParam(name)
                    if(i + default(name).length > args.length) throw OptionWithoutExpectedParam(name)
                    val op : OptionalParam = default(name) match {
                        case _ : IntParam => try { name -> args(i+1).toInt } catch
                            {case e: java.lang.NumberFormatException => throw OptionWithoutExpectedParam(name)}
                        case _ : StringParam => 
                            if(args(i+1)(0) == '-') throw OptionWithoutExpectedParam(name)
                            else name -> args(i+1)
                        case _ : FlagParam => FlagParam(name, true)
                    }
                    parseOne(i+op.length, op :: l)
                }
            }

            val (i, l) = parseOne(0, Nil)
            new ArgsOps(default ++ l.map(_.toPair).toMap, args.slice(i, args.length))
        }

        def <<|(args: Array[String]) = parse(args)
    }

    implicit def asInt(op: OptionalParam) = op match { case IntParam(_, i) => i }
    implicit def asString(op: OptionalParam) = op match { case StringParam(_, s) => s }
    implicit def asBoolean(op: OptionalParam) = op match { case FlagParam(_, b) => b}

    implicit def fromPairSS(kv: (String, String)): OptionalParam= StringParam(kv._1, kv._2)
    implicit def fromPairSI(kv: (String, Int)): OptionalParam = IntParam(kv._1, kv._2)
    implicit def fromString(k: String): OptionalParam = FlagParam(k)
    
}