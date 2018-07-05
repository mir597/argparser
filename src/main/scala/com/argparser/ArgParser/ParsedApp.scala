package com.argparser.ArgParser

import com.argparser.ArgParser.Commands.Cmd

import scala.collection.immutable.HashMap
import scala.util._
import scala.util.control.Exception._

trait ParsedApp {
  private var _args: Array[String] = Array.empty[String]

  def default(args: Array[String]): Unit = Cmd.default._init(args.toList, HashMap.empty)

  def main(arg: Array[String]): Unit = {
    this._args = arg

    val (pSubOpts, mainOpts) = args.toList.partition(_.startsWith("-"))

    val (c, optsRest) = Cmd.get(mainOpts)
    val m_new_e = Try(parseArgs(genMap(c.listOfOpts), pSubOpts))

    var error = false
    //      // find conflicts
    //      val subOpts = pSubOpts.map(s => s.trim().substring(1))
    //      val cOpts = subOpts.filter(c.phase.conflictNames.contains)
    //      System.out.println(c.phase.conflictNames.mkString(", "))
    //      if (cOpts.nonEmpty) {
    //        System.out.println("The following options are conflicted in more than one phases. Please specify it with prefix.")
    //        cOpts.foreach(s => {
    //          val cc = c.phase.opts(s).map(s => s"'$s'")
    //          System.out.println(s"- the sub-option $s conflicts with ${cc.mkString(", ")}")
    //        })
    //        error = true
    //      }
    //
    //      // find unknown
    //      val unknownOpts = subOpts.filter(p => !c.phase.argNames.contains(p))
    //      if (unknownOpts.nonEmpty) {
    //        System.out.println("The following options are unknown:")
    //        unknownOpts.foreach(s => {
    //          System.out.println(s"- $s")
    //        })
    //        error = true
    //      }

    if (error) return

    m_new_e match {
      case Success(m_new) =>
        c._init(optsRest, m_new) match {
          case Success(v) =>
            println(s"ok: $v")
          case Failure(e) =>
            e.printStackTrace()
        }
      case Failure(e) => // failed parsing the arguments.
    }
  }

  protected def args: Array[String] = _args

  def genMap(list: List[Opt]): HashMap[String, OptVal] = {
    (HashMap.empty[String, OptVal] /: list) ((m_i, o) => {
      val v = o match {
        case OSimple(_, b) => VBool(b)
        case OStrArg(_, s) => VStr(s)
        case OIntArg(_, i) => VInt(i)
      }
      m_i + (o.name -> v)
    })
  }

  private def parseArgs(m: HashMap[String, OptVal], pSubOpts: List[String]): HashMap[String, OptVal] = {
    val e = HashMap.empty[String, OptVal]
    val p = """-([a-zA-Z0-9_]*)(?:=(.*))?""".r
    (e /: pSubOpts) ((e_i, o) => {
      o match {
        case p(n, v) =>
          def e(msg: String): HashMap[String, OptVal] = {
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

  private def parseValue(p: OptVal, v: String): OptVal = {
    p match {
      case VBool(_) if v == null => VBool(true)
      case VBool(_) => VBool(v.toBoolean)
      case VInt(_) => VInt(v.toInt)
      case VStr(_) => VStr(v)
    }
  }

  protected def added(cmd: String, ths: Cmd, default: Boolean = false): Unit = {
    Cmd.map += (cmd -> ths)
    if (default) Cmd.default = ths
  }

  case object UnknownCmdException extends Throwable

}
