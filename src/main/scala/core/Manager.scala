package com.lamaVersion.core

import scala.sys.process._
import org.joda.time.DateTime
import org.joda.time.format._
import java.io.File

case class Commit(hash: String, date: DateTime){
    def shortHash() = hash.substring(0, 6)
}

object Commit{
    def fromStrings(it: Iterator[String]): Iterator[Commit] = {
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

object Manager extends App {

    val manager = new Manager("results/", "experiments", "results")
    println(manager.repository)
    manager.commits.foreach(println)

}

class Manager(gitPath: String, experimentPath: String, resultPath: String) {
    val gitDir = new File(gitPath)
    val resultDir = new File(resultPath)
    val repository = Process("git config --get remote.origin.url", gitDir).!!
    val repName = repository.split("/").last.split(".")(0)
    val commits = Commit.fromStrings( Process("git log", gitDir).lineStream.toIterator )
    val workingDir = resultPath + '/' + repName

    def pull(){
        if(Command.fileExist(workingDir))
            Process( Seq("git", "pull"), resultDir ).!
        else
            Process( Seq("git", "clone", repository), resultDir ).!
    }

    def switchBranch(hash: String){
        
    }
}



