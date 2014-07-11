package com.lamaVersion.core

import org.scalatest.FunSuite
import scala.sys.process._


class ManagerTest extends FunSuite {
    test("The commit parser works correctly"){
        val input = Seq(
            "commit 4d5e34ee835c6c236dd2916971de99e675dd9616",
            "Author: Guillaume Wenzek <guillaume.wenzek@polytechnique.org>",
            "Date:   Thu Jul 10 18:10:20 2014 +0200",
            "    gitignore and build.sbt",
            "",
            "commit cbd6d65ebaca3d9ac02fa6a7e113e61e6fe858ee",
            "Author: Guillaume Wenzek <guillaume.wenzek@polytechnique.org>",
            "Date:   Thu Jul 10 18:07:07 2014 +0200",
            "",
            "first commit"
        )
        val it = input.toIterator
        import org.joda.time.{DateTime, DateTimeZone}
        

        val commits = Commit(it).toArray
        assert(commits(0) == Commit("4d5e34ee835c6c236dd2916971de99e675dd9616", 
                                    new DateTime(2014, 7, 10, 18, 10, 20)))

        val secondCommit = Commit(it)
        assert(commits(1) == Commit("cbd6d65ebaca3d9ac02fa6a7e113e61e6fe858ee", 
                                    new DateTime(2014, 7, 10, 18, 7, 7)))
    }
}