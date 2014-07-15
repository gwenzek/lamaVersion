package com.lamaVersion.core

import scala.sys.process._
import org.joda.time.DateTime
import org.joda.time.format._
import java.io.File
import com.lamaVersion.impl.EasyIO._


case class Commit(hash: String, date: DateTime){
    def shortHash() = hash.substring(0, 8)

    def toShortString() = Commit.simpleFormat.print(date) + '_' + shortHash
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
    val simpleFormat = DateTimeFormat.forPattern("yyyy_mm_dd_HH_mm_ss")
}

object Manager {
    def main(args: Array[String]) {
        if (args.length < 3) println("Expect : gitPath experimentPath resultPath")
        else{
            val manager = new Manager(args(0), args(1), args(2))
            manager.executeAll()
        }
    }
}

class Manager(gitPath: String, experimentPath: String, resultPath: String) {
    val gitDir = new File(gitPath)
    insureDirExists(resultPath)
    val resultDir = new File(resultPath)

    val repository = Process("git config --get remote.origin.url", gitDir).!!

    val repName = repository.split("/|\n").last
    println("Working with git repository : " + repName)

    val commits = Commit.fromStrings( Process("git log", gitDir).lineStream.toIterator ).toStream
    
    val workingPath = resultPath + '/' + repName
    val workingDir = new File(workingPath)

    def pull(){
        if(new File(workingPath).exists){
            println("pulling " + repName + " into " + workingPath)
            Process( Seq("git", "pull"), workingDir ).!
        } else {
            println("cloning " + repName + " into " + resultPath)
            Process( Seq("git", "clone", repository), resultDir ).!
        }
    }

    def switchBranch(hash: String){
        Process( Seq("git", "reset", "--hard", hash), workingDir ).!
    }

    def switchToHead(){
        Process( Seq("git", "reset", "--hard", "master"), workingDir ).!
    }

    def executeOnCommit(exp: Experiment, commit: Commit, lazylazy: Boolean = true){
        val outputPath = resultPath + '/' + exp.name
        insureDirExists(outputPath)
        val stdoutDump = outputPath + "/std_" + commit.toShortString + ".out"
        if(!(fileExists(stdoutDump) && lazylazy)) {
            if(exp.accept(commit)){
                switchBranch(commit.hash)
                println("switched to : " + commit)
                exp.execute(stdoutDump)
                exp.extractResultsTo(workingPath, outputPath, commit.toShortString)
            }
        }
    }

    def executeAll(lazylazy: Boolean = true){
        pull()
        if(!fileExists(experimentPath)) throw new java.io.FileNotFoundException(experimentPath)

        for(expFile <- listExtensions(experimentPath, "sh")){
            val exp = Experiment.fromFile(expFile, workingPath)
            println("Experiment : "+exp.name + " -> " + exp.command)
            for(c <- commits){
                executeOnCommit(exp, c, lazylazy)
            }
        }
        switchToHead
    }
}



