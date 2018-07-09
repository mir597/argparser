package com.argparser.ArgParser

import com.argparser.ArgParser.Commands.Cmd

import scala.collection.immutable.HashMap
import scala.util._
import scala.util.control.Exception._

trait ParsedApp {
  private[this] var _args: Array[String] = Array.empty[String]

  @deprecatedInheritance
  def main(arg: Array[String]): Unit = {
    this._args = arg

    val (pSubOpts, mainOpts) = args.toList.partition(_.startsWith("-"))

    val (c, optsRest) = Cmd.get(mainOpts)
    val m_new_e = Try(parseArgs(genMap(c.listOfOpts), pSubOpts))

    m_new_e match {
      case Success(m_new) =>
        c._init(optsRest, m_new) match {
          case Success(v) => println(s"ok: $v")
          case Failure(e) => e.printStackTrace()
        }
      case Failure(e) => // failed parsing the arguments.
    }
  }

  private[this] def genMap(list: List[Opt]): HashMap[String, Object] = {
    (HashMap.empty[String, Object] /: list) ((m_i, o) => {
      m_i + (o.name -> o.default)
    })
  }

  private[this] def parseArgs(m: HashMap[String, Object], pSubOpts: List[String]): HashMap[String, Object] = {
    val p = """-([a-zA-Z0-9_]*)(?:=(.*))?""".r
    (HashMap.empty[String, Object] /: pSubOpts) ((e_i, o) => {
      o match {
        case p(n, v) =>
          def e(msg: String): HashMap[String, Object] = {
            System.err.println(msg)
            e_i
          }

          val iNum = failAsValue(classOf[NumberFormatException])(e(s"- Invalid argument format of $n: $v is not a number format."))
          val iBool = failAsValue(classOf[IllegalArgumentException])(e(s"- Invalid argument format of $n: $v is not a boolean format."))
          val iUnkn = failAsValue(classOf[NoSuchElementException])(e(s"- The following unknown option is ignored: $o"))
          val others = nonFatalCatch withApply (k => e(s"- Unknown exception: ${k.getMessage}"))
          (iNum or iBool or iUnkn or others) (e_i + (n -> parseValue(m(n), v)))
        case _ =>
          System.err.println(s"- failed to parse an argument: $o")
          e_i
      }
    })
  }

  private[this] def parseValue(p: Object, v: String): Object = {
    p match {
      case _: java.lang.Boolean if v == null => Boolean.box(true)
      case _: java.lang.Boolean => Boolean.box(v.toBoolean)
      case _: java.lang.Integer => Int.box(v.toInt)
      case _: java.lang.String => v
      case _ => throw new InternalError("TODO")
    }
  }

  protected def added(cmd: String, ths: Cmd, default: Boolean = false): Unit = {
    Cmd.map += (cmd -> ths)
    if (default) Cmd.default = ths
  }

  protected def args: Array[String] = _args
}
