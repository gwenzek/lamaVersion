package com.lamaVersion.core

import org.scalatest.FunSuite
import org.joda.time.DateTime
import scala.sys.process._


class ExperimentTest extends FunSuite {

    val expLoaded = Experiment.fromFile("src/test/resources/test.sh")
    
    test("the experiment gets the correct name") {
        assert(expLoaded.name == "test")
    }

    test("the experiment parser grabs the correct command lines") {
        // ls
        // echo "Hello world" > HelloWorld.txt
        assert(expLoaded.command.toString == ("ls" ### "echo \"Hello world\" > HelloWorld.txt").toString)
    }

    test("the experiment parser grabs date bounds and excluded hashs correctly") {
        // # BEGIN: Thu Jun 26 15:40:09 2014 +0200
        // # END: Thu Jul 10 12:20:40 2014 +0200
        val toSoon = Commit("toSoon_fzljfozjfp", new DateTime(2014, 4, 21, 17, 32))
        val toLate = Commit("toLate_fzljfozjfp", new DateTime(2014, 8, 8, 8, 8))
        val inTime = Commit("inTime_fzljfozjfp", new DateTime(2014, 7, 5, 15, 30))
        val excluded = Commit("e19dfbf4ac549e591069f5a95d9cecdc1e07a663", new DateTime(2014, 7, 5, 15, 30))
        assert(expLoaded.accept(toSoon) == false)
        assert(expLoaded.accept(toLate) == false)
        assert(expLoaded.accept(inTime) == true)
        assert(expLoaded.accept(excluded) == false)
    }

    test("the experiment parser grabs the correct output files") {
        // # GET: HelloWorld.txt
        assert(expLoaded.outputs == Seq("HelloWorld.txt"))
    }

}