package com.lamaVersion.impl

import org.scalatest.FunSuite
import EasyIO._

class EasyIOTest extends FunSuite {

    val parser = ArgOps("someInt" -> 4, "someFlag", "someWord" -> "hello")

    test("the ArgOps parser rejects unknow options") {
        intercept[UnknownOption](parser <<| Array("-nothing"))
    }

    test("the ArgOps parser rejects options without args"){
        intercept[OptionWithoutExpectedParam](parser <<| Array("-someInt","-someFlag"))
        intercept[OptionWithoutExpectedParam](parser <<| Array("-someWord", "-someFlag"))
        intercept[OptionWithoutExpectedParam](parser <<| Array("-someInt"))
        intercept[OptionWithoutExpectedParam](parser <<| Array("-someWord"))
    }
    
    test("the ArgOps parser reads the correct values"){
        val argOps1 = parser <<| Array("-someInt", "3", "-someFlag", "-someWord", "goodbye")
        assert(argOps1("someInt").asInt == 3)
        assert(argOps1("someFlag").asBoolean == true)
        assert(argOps1("someWord").asString == "goodbye")
    }

    test("the ArgOps parser keeps the correct default values"){
        val argOps1 = parser <<| Array("arg")
        assert(argOps1("someInt").asInt == 4)
        assert(argOps1("someFlag").asBoolean == false)
        assert(argOps1("someWord").asString == "hello")
    }

    test("the ArgOps parser keeps the correct number of arg"){
        val argOps1 = parser <<| Array("-someInt", "3", "-someFlag", "-someWord", "goodbye", "arg0", "arg1", "arg2")
        assert(argOps1.args(0) == "arg0")
        assert(argOps1.args(1) == "arg1")
        assert(argOps1.args(2) == "arg2")
    }

    test("the ArgOps parser doesn't return nulls"){
        val argOps1 = parser <<| Array("-someInt", "3", "-someFlag", "-someWord", "goodbye")
        assert(argOps1.args.length == 0)
    }

}