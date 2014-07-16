package com.lamaVersion.impl

import org.scalatest.FunSuite


class ArgsOpsTest extends FunSuite {
    import ArgsOps._

    val parser = ArgsOps("someInt" -> 4, "someFlag", "someWord" -> "hello")

    test("the ArgsOps parser rejects unknow options") {
        intercept[UnknownParam](parser <<| Array("-unknown"))
    }

    test("the ArgsOps parser rejects options without args"){
        intercept[OptionWithoutExpectedParam](parser <<| Array("-someInt","-someFlag"))
        intercept[OptionWithoutExpectedParam](parser <<| Array("-someInt", "3x"))
        intercept[OptionWithoutExpectedParam](parser <<| Array("-someInt"))
        intercept[OptionWithoutExpectedParam](parser <<| Array("-someWord", "-someFlag"))
        intercept[OptionWithoutExpectedParam](parser <<| Array("-someWord"))
    }
    
    test("the ArgsOps parser reads the correct values"){
        val argsOps = parser <<| Array("-someInt", "3", "-someFlag", "-someWord", "goodbye")
        assert(argsOps("someInt").asInt == 3)
        assert(argsOps("someFlag").asBoolean == true)
        assert(argsOps("someWord").asString == "goodbye")
    }

    test("the ArgsOps parser keeps the correct default values"){
        val argsOps = parser <<| Array("arg")
        val someInt : Int = argsOps("someInt")
        val someFlag : Boolean = argsOps("someFlag")
        val someWord : String = argsOps("someWord")
        assert(someInt == 4)
        assert(someFlag == false)
        assert(someWord == "hello")
    }

    test("the ArgsOps parser keeps the correct number of arg"){
        val argsOps = parser <<| Array("-someInt", "3", "-someFlag", "-someWord", "goodbye", "arg0", "arg1", "arg2")
        assert(argsOps.args(0) == "arg0")
        assert(argsOps.args(1) == "arg1")
        assert(argsOps.args(2) == "arg2")
    }

    test("the ArgsOps parser doesn't return nulls"){
        val argsOps = parser <<| Array("-someInt", "3", "-someFlag", "-someWord", "goodbye")
        assert(argsOps.args.length == 0)
    }

}