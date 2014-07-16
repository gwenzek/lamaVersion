package com.lamaVersion.core

import scala.sys.process._
import org.joda.time.DateTime
import org.joda.time.format._
import java.io.File
import com.lamaVersion.impl.EasyIO._
import com.lamaVersion.impl.ArgsOps._


case class Commit(hash: String, date: DateTime){
    def shortHash = hash.substring(0, 8)

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
        val argsOps = ArgsOpsParser("lazy") <<| args
        if (argsOps.args.length < 3) println("Expect 3 args: gitPath experimentPath resultPath")
        else{
            val manager = new Manager(new File(args(0)), new File(args(1)), new File(args(2)))
            manager.executeAll(argsOps("lazy"))
        }
    }

    val sep = System.getProperty("path.separator")
}

class Manager(gitDir: File, val experimentDir: File, val resultDir: File) {
    
    insureDirExists(resultDir)
    
    val repository = Process("git config --get remote.origin.url", gitDir).!!
    val repName = repository.split("/|\r|\n").last
    println("Working with git repository : " + repName)

    val commits = Commit.fromStrings( Process("git log", gitDir).lineStream.toIterator ).toStream
    
    val workingDir = new File(resultDir, repName)

    def pull(){
        if(workingDir.exists){
            println("pulling " + repName + " into " + workingDir)
            Process( Seq("git", "pull"), workingDir ).!
        } else {
            println("cloning " + repName + " into " + resultDir)
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
        val outputDir = new File(resultDir, exp.name)
        insureDirExists(outputDir)
        val stdoutDump = new File(outputDir,  "std_" + commit.toShortString + ".out")
        if(!(stdoutDump.exists && lazylazy)) {
            if(exp.accept(commit)){
                switchBranch(commit.hash)
                println("switched to : " + commit)
                exp.execute(stdoutDump)
                exp.extractResultsTo(workingDir, outputDir, commit.toShortString)
            }
        }
    }

    def executeAll(lazylazy: Boolean = true){
        pull()
        if(!experimentDir.exists) throw new java.io.FileNotFoundException(experimentDir.getPath)

        for(expFile <- listExtensions(experimentDir, "sh")){
            val exp = Experiment.fromFile(expFile, workingDir.getPath)
            println("Experiment : "+exp.name + " -> " + exp.command)
            for(c <- commits){
                executeOnCommit(exp, c, lazylazy)
            }
        }
        switchToHead()
    }
}



