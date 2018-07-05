package com.argparser.ArgParser

import com.argparser.ArgParser.Commands._
import org.scalatest.FunSuite

import scala.collection.immutable.HashMap
import scala.util._

class ArgParserTest extends FunSuite {

  object EmptyMain extends ParsedApp

  object EmptyMain2 extends ParsedApp {
    added("empty", UnitCmd)
  }

  object Main extends ParsedApp {

    // global argument type
    case class OG(gg0: Boolean, gg1: Boolean, gg2: String) extends Opts

    case class Inc(v: Int) extends GPhase[Int, Int] {
      override val optDescriptions: HashMap[String, String] = HashMap(
        "n1" -> "inc boolean 1",
        "n2" -> "inc boolean 2",
        "n3" -> "inc boolean 3",
        "n4" -> "inc boolean 4",
        "n5" -> "inc boolean 5"
      )
      override val name: String = s"inc$v"
      override val description: String = s"Inc$v"
      override val arg = OO(n1 = false, n2 = false, v, "oh")

      override def run(gs: List[String], g: Opts, o: OO, s: Int): Try[Int] = {
        Try(s + 10)
      }

      case class OO(n1: Boolean, n2: Boolean, n3: Int, n4: String) extends Opts

    }

    // simple phase
    object Init extends InitPhase[Int] {
      // default local argument
      override val arg: Opts = OO(k1 = true, k2 = false, 10)
      override val name: String = "init"

      override def run(gs: List[String], g: Opts, o: OO, e: Unit): Try[Int] = {
        Try(10)
      }

      // local argument type
      case class OO(k1: Boolean, k2: Boolean, k3: Int) extends Opts

    }

    object Multi extends GPhase[Int, Int] {
      override val arg = OO(g1 = false, g2 = false, 0)
      override val name: String = "multi"

      override def run(gs: List[String], g: Opts, o: OO, e: Int): Try[Int] = {
        Try(e * 2)
      }

      case class OO(g1: Boolean, g2: Boolean, g3: Int) extends Opts

    }

    // Commands
    object Calc extends Cmd(Init >> Multi >> Inc(10) >> Inc(20)) {
      override val arg = OG(gg0 = false, gg1 = false, gg2 = "global")
      override val optDescriptions: HashMap[String, String] = HashMap(
        "gg0" -> "global1",
        "gg1" -> "global2",
        "gg3" -> "global4"
      )

      override val description: String = "simple calculation"
    }

    added("help", Help("name"), default = true)
    added("calc", Calc)
  }


  test("default test") {
    Main.main(Array[String]())
  }
  test("help test") {
    Main.main(Array[String]("help"))
  }
  test("calc test") {
    Main.main(Array[String]("calc"))
  }
  test("unknown arguments.") {
    Main.main(Array[String]("help", "-test1"))
  }
  test("type error") {
    Main.main(Array[String]("calc", "-n1=10"))
  }
  test("type error 2") {
    Main.main(Array[String]("calc", "-n3=true"))
  }
  test("no type error 1") {
    Main.main(Array[String]("calc", "-n1", "-n3=10"))
  }
  test("empty app") {
    EmptyMain.main(Array[String]())
  }
  test("empty2 app") {
    EmptyMain2.main(Array[String]())
  }
  test("empty3 app") {
    EmptyMain2.main(Array[String]("empty"))
  }
}
