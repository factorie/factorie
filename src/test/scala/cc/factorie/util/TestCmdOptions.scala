package cc.factorie.util

import java.io.File

import org.scalatest.{Matchers, FlatSpec}

/**
 * @author John Sullivan
 */
class TestCmdOptions extends FlatSpec with Matchers {

  "CmdOptions" should "parse comma-separated lists properly" in {
    val opts = new DefaultCmdOptions {
      val dList = new CmdOption("dlist", List.empty[Double], "", "")
      val sList = new CmdOption("slist", List.empty[String], "", "")
      val iList = new CmdOption("ilist", List.empty[Int], "" ,"")
    }
    opts parse Array("--dlist=1.0,2,3.5", "--slist=Foo,Bar,Baz", "--ilist=2,4,6,8,10")

    assert(opts.dList.value == List(1.0, 2.0, 3.5))
    assert(opts.sList.value == List("Foo", "Bar", "Baz"))
    assert(opts.iList.value == List(2,4,6,8,10))
  }

  it should "parse primitive types properly" in {
    val opts = new DefaultCmdOptions {
      val int = new CmdOption("i", 2, "", "")
      val long = new CmdOption("l", 1000L, "", "")
      val float = new CmdOption("f", 1.0f, "", "")
      val double = new CmdOption("d", 1.0, "", "")
      val string = new CmdOption("s", "s", "", "")
      val boolean = new CmdOption("b", false, "", "")
    }
    opts parse Array("--i=13", "--l=13", "--f=13", "--d=13", "--s=thirteen", "--b=true")

    assert(opts.int.value == 13)
    assert(opts.long.value == 13L)
    assert(opts.float.value == 13.0f)
    assert(opts.double.value == 13.0)
    assert(opts.string.value == "thirteen")
    assert(opts.boolean.value)
  }

  it should "deal with the short bool case" in {
    val opts = new DefaultCmdOptions {
      val bool = new CmdOption("long-name", false, "", "", false, 'b')
    }
    opts parse Array("-b")

    assert(opts.bool.value)
  }

  it should "carry through default args" in {
    val opts = new DefaultCmdOptions {
      val str = new CmdOption("with-default", "default-val", "", "")
    }
    opts parse Array.empty[String]
    assert(opts.str.value == "default-val")
  }

  it should "parse space-separated args" in {
    val opts = new DefaultCmdOptions {
      val str = new CmdOption("separate", "", "", "")
      val dub = new CmdOption("next", 5.7, "", "")
    }
    opts parse Array("--separate", "argument", "--next", "2.34")
    assert(opts.str.value == "argument")
    assert(opts.dub.value == 2.34)
  }

  it should "parse space-separated lists" in {
    val opts = new DefaultCmdOptions {
      val dubList = new CmdOption("params", List.empty[Double], "", "")
      val name = new CmdOption("name", "", "", "")
    }
    opts parse "--params 1.0 5.3 13 2943.32 --name=steve".split(" ")
    assert(opts.dubList.value == List(1.0, 5.3, 13.0, 2943.32))
    assert(opts.name.value == "steve")
  }

  it should "parse strings into Files properly" in {
    val opts = new CmdOptions {
      val file = new CmdOption[File]("file", null, "", "")
    }
    val tmp = File.createTempFile("test-dir", "test1")
    opts parse s"--file ${tmp.getAbsolutePath}".split(" ")
    assert(opts.file.value.getAbsolutePath == tmp.getAbsolutePath)
  }

  it should "parse string lists into file lists properly" in {
    val opts = new CmdOptions {
      val files = new CmdOption[List[File]]("files", List.empty, "", "")
    }
    val tmps = (1 to 3).map(i => File.createTempFile("test-dir", "test" + i))
    opts parse s"--files=${tmps.map(_.getAbsolutePath).mkString(",")}".split(" ")
    assert(opts.files.value.zip(tmps).forall{case (a, b) => a.getAbsolutePath == b.getAbsolutePath})
  }
}
