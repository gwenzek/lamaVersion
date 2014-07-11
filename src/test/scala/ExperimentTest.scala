package com.lamaVersion.core

import org.scalatest.FunSuite
import scala.sys.process._


class ExperimentTest extends FunSuite {

    ignore("the experiment parser works correctly") {
        val exp = new Experiment("test", "ls" ### "git log")
        val expLoaded = Experiment.fromFile("experiments/test.sh")
        assert(exp.command == expLoaded.command)
    }

    test("the process parser works correctly") {
        val process = "ls" ### "git log"
        val processLoaded = Experiment.processFromFile("experiments/test.sh")
        assert(process.toString == processLoaded.toString)
    }

}