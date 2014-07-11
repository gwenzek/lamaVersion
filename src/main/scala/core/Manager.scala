package com.lamaVersion.core

import scala.sys.process._
import org.joda.time.DateTime
import org.joda.time.format._


case class Commit(hash: String, date: DateTime){
    def shortHash() = hash.substring(0, 6)
}

object Commit{
    def apply(it: Iterator[String]): Iterator[Commit] = {
        def readCommitHash(line: String) = {
            if(line.startsWith("commit ")) Some(line.split(" ")(1))
            else None
        }

        def readOne(s: String): Option[Commit] = {
            var hash : Option[String] = readCommitHash(s)
            try { 
                while(hash == None){
                    val hashLine = it.next
                    hash = readCommitHash(hashLine)
                }
                val authorLine = it.next
                val dateLine = it.next
                val date = gitDateFormat.parseDateTime(dateLine.substring(8))
                Some(Commit(hash.get, date))
            } catch {
                case e: Exception => None
            }            
        }

        (for(s <- it) yield readOne(s)) filter(_ != None) map(_.get)
    }

    val gitDateFormat = DateTimeFormat.forPattern("EEE MMM dd HH:mm:ss yyyy Z")
}

class Manager(gitPath: String, experimentPath: String, resultPath: String) {

    val repository = Seq("cd", gitPath) ### "git config --get remote.origin.url" !!

    val commits = Commit( (Seq("cd", gitPath) ### "git log").lines.toIterator )


}

