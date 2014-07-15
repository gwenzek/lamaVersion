package com.lamaVersion.core

import org.scalatest.FunSuite
import org.joda.time.DateTime
import scala.sys.process._


class ExperimentTest extends FunSuite {

    val expLoaded = Experiment.fromFile("experiments/test.sh")
    
    test("the experiment parser grabs the correct name") {
        // # NAME: test
        assert(expLoaded.name == "test")
    }

    test("the experiment parser grabs the correct command lines") {
        // ls
        // echo "Hello world" > HelloWorld.txt
        assert(expLoaded.command.toString == ("ls" ### "echo \"Hello world\" > HelloWorld.txt").toString)
    }

    test("the experiment parser grabs date bounds correctly") {
        // # BEGIN: Thu Jun 26 15:40:09 2014 +0200
        // # END: Thu Jul 10 12:20:40 2014 +0200
        val toSoon = Commit("toSoon", new DateTime(2014, 4, 21, 17, 32))
        val toLate = Commit("toLate", new DateTime(2014, 8, 8, 8, 8))
        val inTime = Commit("inTime", new DateTime(2014, 7, 5, 15, 30))
        assert(expLoaded.accept(toSoon) == false)
        assert(expLoaded.accept(toLate) == false)
        assert(expLoaded.accept(inTime) == true)
    }

    test("the experiment parser grabs the correct output files") {
        // # GET: HelloWorld.txt
        assert(expLoaded.outputs == Seq("HelloWorld.txt"))
    }

}